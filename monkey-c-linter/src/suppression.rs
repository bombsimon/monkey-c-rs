//! `rafiki: ignore[...]` and `rafiki: file-ignore[...]` comments, which silence the rules they
//! name without turning them off for the whole project.
//!
//! An own-line `ignore` covers the whole node below it rather than just the next line, so it keeps
//! working when the formatter reflows that node over several lines.
use std::ops::Range;

use monkey_c_parser::ast::{Ast, CommentStmt, ElseBranch, IfStmt, Span, Stmt};
use monkey_c_parser::parser::ParseOutput;

use crate::rules::{self, invalid_directive, unused_ignore};
use crate::{Diagnostic, Fix};

const PREFIX: &str = "rafiki:";

/// Every suppression comment in a file, resolved to the source range it covers, along with the
/// `rafiki:` comments that can't be read.
pub(crate) struct Suppressions {
    comments: Vec<IgnoreComment>,
    problems: Vec<Problem>,
}

struct IgnoreComment {
    span: Span,
    list: Span,
    rules: Vec<ListedRule>,
    range: Range<usize>,
}

#[derive(Debug, PartialEq)]
struct ListedRule {
    name: String,
    span: Span,
}

struct Problem {
    message: String,
    span: Span,
}

impl Suppressions {
    pub(crate) fn new(output: &ParseOutput, source: &str) -> Self {
        let all_comments = &output.comments.comments;
        let first_token = next_token(source, 0, all_comments);
        let nodes = node_spans(&output.ast);

        let mut comments = Vec::new();
        let mut problems = Vec::new();

        let mut fmt_regions = FmtRegions {
            source,
            comments: all_comments,
            nodes: &nodes,
            open_bodies: Vec::new(),
        };

        for comment in all_comments {
            if let Some(message) = fmt_regions.problem(comment) {
                problems.push(Problem {
                    message,
                    span: comment.span,
                });
                continue;
            }

            let Some(parsed) = parse(comment) else {
                continue;
            };

            let directive = match parsed {
                Ok(directive) => directive,
                Err(message) => {
                    problems.push(Problem {
                        message,
                        span: comment.span,
                    });
                    continue;
                }
            };

            // A `file-ignore` further down would hide a file-wide suppression where nobody looks
            // for one.
            let range = match directive.kind {
                Kind::Ignore => ignore_range(comment, source, all_comments, &nodes),
                Kind::FileIgnore if comment.span.end <= first_token => 0..source.len(),
                Kind::FileIgnore => {
                    problems.push(Problem {
                        message: "`file-ignore` must come before any code".to_string(),
                        span: comment.span,
                    });
                    continue;
                }
            };

            comments.push(IgnoreComment {
                span: comment.span,
                list: directive.list,
                rules: directive.rules,
                range,
            });
        }

        Self { comments, problems }
    }

    /// Whether a comment silences `diagnostic`. Only the start of its span counts, so a finding
    /// covering a whole function belongs to the line the function starts on.
    pub(crate) fn covers(&self, diagnostic: &Diagnostic) -> bool {
        self.comments.iter().any(|comment| {
            comment.range.contains(&diagnostic.span.start)
                && comment
                    .rules
                    .iter()
                    .any(|listed| listed.name == diagnostic.rule)
        })
    }

    /// Findings about the `rafiki:` comments themselves: the ones that can't be read, and every
    /// listed rule that `selected` runs but has nothing in `findings` to silence. `findings` must
    /// be taken before suppression, or every rule in use would look unused.
    pub(crate) fn findings(
        &self,
        findings: &[Diagnostic],
        source: &str,
        selected: impl Fn(&str) -> bool,
    ) -> Vec<Diagnostic> {
        let mut out: Vec<Diagnostic> = self
            .problems
            .iter()
            .map(|problem| invalid_directive::diagnostic(problem.message.clone(), problem.span))
            .collect();

        for comment in &self.comments {
            let mut unused = Vec::new();

            for listed in &comment.rules {
                if let Some(message) = rule_problem(&listed.name) {
                    out.push(invalid_directive::diagnostic(message, listed.span));
                    continue;
                }

                let reported = findings.iter().any(|finding| {
                    finding.rule == listed.name && comment.range.contains(&finding.span.start)
                });

                if selected(&listed.name) && !reported {
                    unused.push(listed);
                }
            }

            if unused.is_empty() {
                continue;
            }

            let fix = comment.without(&unused, source);
            out.extend(
                unused.iter().map(|listed| {
                    unused_ignore::diagnostic(&listed.name, listed.span, fix.clone())
                }),
            );
        }

        out
    }
}

impl IgnoreComment {
    /// A fix dropping `unused` from the comment, or the whole comment when no rule is left.
    fn without(&self, unused: &[&ListedRule], source: &str) -> Fix {
        let kept: Vec<&str> = self
            .rules
            .iter()
            .filter(|listed| !unused.iter().any(|u| u.span == listed.span))
            .map(|listed| listed.name.as_str())
            .collect();

        if kept.is_empty() {
            return Fix::single(removal(self.span, source), "");
        }

        Fix::single(self.list, kept.join(", "))
    }
}

/// A fix silencing `finding` with a `rafiki: ignore` comment, or `None` when a comment can't
/// silence it. The comment goes at the end of the finding's line, which keeps a run of similar
/// statements together, unless that line already has a comment. A suppression already there or
/// on the line above gets the rule added instead, so ignoring several rules doesn't stack up
/// comments.
pub fn ignore_fix(output: &ParseOutput, source: &str, finding: &Diagnostic) -> Option<Fix> {
    let rule = finding.rule;

    if rule_problem(rule).is_some() {
        return None;
    }

    let comments = &output.comments.comments;
    let line = line_range(source, finding.span.start);
    let trailing = comments.iter().find(|comment| {
        line.contains(&comment.span.start)
            && !source[line.start..comment.span.start].trim().is_empty()
    });

    let existing = trailing
        .and_then(ignore_directive)
        .or_else(|| comment_above(comments, source, line.start).and_then(ignore_directive));

    if let Some(directive) = existing {
        // Already listed but still reported means the comment doesn't reach this finding, and
        // listing the rule again wouldn't change that.
        if directive.rules.iter().any(|listed| listed.name == rule) {
            return None;
        }

        let mut names: Vec<&str> = directive.rules.iter().map(|r| r.name.as_str()).collect();
        names.push(rule);

        return Some(Fix::single(directive.list, names.join(", ")));
    }

    let text = &source[line.clone()];

    if trailing.is_none() {
        let end = line.start + text.trim_end().len();

        return Some(Fix::single(
            Span { start: end, end },
            format!(" // {PREFIX} ignore[{rule}]"),
        ));
    }

    let indent = &text[..text.len() - text.trim_start().len()];
    let newline = if source.contains("\r\n") {
        "\r\n"
    } else {
        "\n"
    };

    Some(Fix::single(
        Span {
            start: line.start,
            end: line.start,
        },
        format!("{indent}// {PREFIX} ignore[{rule}]{newline}"),
    ))
}

/// The comment alone on the line before `line_start`, if there is one.
fn comment_above<'a>(
    comments: &'a [CommentStmt],
    source: &str,
    line_start: usize,
) -> Option<&'a CommentStmt> {
    let above = line_range(source, line_start.checked_sub(1)?);

    comments.iter().find(|comment| {
        above.contains(&comment.span.start)
            && source[above.start..comment.span.start].trim().is_empty()
    })
}

fn ignore_directive(comment: &CommentStmt) -> Option<Directive> {
    let directive = parse(comment)?.ok()?;

    (directive.kind == Kind::Ignore).then_some(directive)
}

/// Why the rule `name` can't be silenced, if it can't. The rules about `rafiki:` comments are left
/// out since a comment silencing them could hide its own mistakes.
fn rule_problem(name: &str) -> Option<String> {
    if name == unused_ignore::RULE || name == invalid_directive::RULE {
        return Some(format!("`{name}` can't be silenced by a comment"));
    }

    if !rules::exists(name) {
        return Some(format!("unknown rule `{name}`"));
    }

    None
}

/// The span to delete to remove `comment`: its whole line when it's alone on it, or the comment
/// and the space before it when it trails code.
fn removal(comment: Span, source: &str) -> Span {
    let line_start = source[..comment.start].rfind('\n').map_or(0, |i| i + 1);
    let line_end = source[comment.end..]
        .find('\n')
        .map_or(source.len(), |i| comment.end + i);

    let before = &source[line_start..comment.start];
    let after = &source[comment.end..line_end];

    if before.trim().is_empty() && after.trim().is_empty() {
        return Span {
            start: line_start,
            end: (line_end + 1).min(source.len()),
        };
    }

    Span {
        start: line_start + before.trim_end().len(),
        end: comment.end,
    }
}

#[derive(Debug, PartialEq)]
enum Kind {
    Ignore,
    FileIgnore,
}

#[derive(Debug, PartialEq)]
struct Directive {
    kind: Kind,
    list: Span,
    rules: Vec<ListedRule>,
}

/// What's wrong with the arguments of a `rafiki: fmt` directive, if anything. The formatter acts on
/// it; anything after `off` or `on` is a reason.
fn fmt_problem(args: &str) -> Option<String> {
    match args.split_whitespace().next() {
        Some("off" | "on") => None,
        _ => Some("expected `off` or `on` after `fmt`".to_string()),
    }
}

/// Follows the `rafiki: fmt off` regions through a file the way the formatter does, to find the
/// `fmt` comments it would silently leave as plain comments.
struct FmtRegions<'a> {
    source: &'a str,
    comments: &'a [CommentStmt],
    nodes: &'a [Span],
    open_bodies: Vec<Option<Span>>,
}

impl FmtRegions<'_> {
    /// What's wrong with `comment` if it's a `rafiki: fmt off` or `rafiki: fmt on` that does
    /// nothing. Comments must be passed in source order.
    fn problem(&mut self, comment: &CommentStmt) -> Option<String> {
        let argument = fmt_argument(comment)?;
        let position = comment.span.start;
        let containing: Vec<Span> = self
            .nodes
            .iter()
            .filter(|span| span.start < position && position < span.end)
            .copied()
            .collect();

        // `None` is the top level of the file.
        let body = containing
            .iter()
            .min_by_key(|span| span.end - span.start)
            .copied();

        // A region keeps everything in it as written, so a comment inside one opened further out
        // is just part of its text.
        let in_outer_region = std::iter::once(None)
            .chain(containing.into_iter().map(Some))
            .any(|outer| outer != body && self.open_bodies.contains(&outer));

        if in_outer_region {
            return None;
        }

        let open = self.open_bodies.iter().position(|open| *open == body);

        if argument == "off" && open.is_some() {
            return Some("formatting is already off from a `rafiki: fmt off` above".to_string());
        }

        if let Some(message) = self.placement_problem(comment, argument) {
            return Some(message);
        }

        match open {
            None if argument == "off" => self.open_bodies.push(body),
            Some(index) => {
                self.open_bodies.swap_remove(index);
            }
            None => {
                return Some("`rafiki: fmt on` without a `rafiki: fmt off` before it".to_string());
            }
        }

        None
    }

    /// The formatter only acts on `fmt` comments on their own lines, with `off` before a
    /// declaration or statement.
    fn placement_problem(&self, comment: &CommentStmt, argument: &str) -> Option<String> {
        let line = line_range(self.source, comment.span.start);
        let own_line = self.source[line.start..comment.span.start]
            .trim()
            .is_empty()
            && self.source[comment.span.end..line.end].trim().is_empty();

        if !own_line {
            return Some(format!(
                "`rafiki: fmt {argument}` must be on a line of its own"
            ));
        }

        let next = next_token(self.source, comment.span.end, self.comments);

        if argument == "off" && !self.nodes.iter().any(|span| span.start == next) {
            return Some(
                "`rafiki: fmt off` must come before a declaration or statement".to_string(),
            );
        }

        None
    }
}

/// The `off` or `on` in a `rafiki: fmt` directive.
fn fmt_argument(comment: &CommentStmt) -> Option<&str> {
    let rest = comment.text.trim_start().strip_prefix(PREFIX)?;
    let mut words = rest.split_whitespace();

    if words.next()? != "fmt" {
        return None;
    }

    words.next().filter(|word| matches!(*word, "off" | "on"))
}

/// Read the directive in `comment`: `None` when it isn't a `rafiki:` comment, and an error when
/// it is one that can't be read. Doc comments (`//!`) never match since their text starts with the
/// extra `!`. Anything after `]` is the optional reason.
fn parse(comment: &CommentStmt) -> Option<Result<Directive, String>> {
    let text = comment.text.as_str();
    let rest = text.trim_start().strip_prefix(PREFIX)?.trim_start();

    let verb_end = rest
        .find(|c: char| c == '[' || c.is_whitespace())
        .unwrap_or(rest.len());

    // Directives for other tools, like the formatter's, are checked here too so a typo in one is
    // reported rather than silently doing nothing.
    let kind = match &rest[..verb_end] {
        "ignore" => Kind::Ignore,
        "file-ignore" => Kind::FileIgnore,
        "fmt" => return fmt_problem(&rest[verb_end..]).map(Err),
        "" => return Some(Err("`rafiki:` is missing a directive".to_string())),
        verb => {
            return Some(Err(format!(
                "`{verb}` is not a known `rafiki:` directive, the known ones are `ignore`, \
                 `file-ignore` and `fmt`"
            )));
        }
    };

    let Some(rest) = rest[verb_end..].strip_prefix('[') else {
        return Some(Err(
            "expected the rules in `[...]` right after the directive".to_string(),
        ));
    };

    // `text` starts right after the `//` or `/*`, and `rest` is a suffix of it, so its position
    // in the source follows from its length.
    let list_start = comment.span.start + 2 + text.len() - rest.len();

    let Some(close) = rest.find(']') else {
        return Some(Err("missing `]` after the rules".to_string()));
    };

    let mut rules = Vec::new();
    let mut part_start = list_start;

    for part in rest[..close].split(',') {
        let name = part.trim();

        if !name.is_empty() {
            let start = part_start + part.len() - part.trim_start().len();
            rules.push(ListedRule {
                name: name.to_string(),
                span: Span {
                    start,
                    end: start + name.len(),
                },
            });
        }

        part_start += part.len() + 1;
    }

    if rules.is_empty() {
        return Some(Err("no rules listed, name the rules to ignore".to_string()));
    }

    let list = Span {
        start: list_start,
        end: list_start + close,
    };

    Some(Ok(Directive { kind, list, rules }))
}

/// A trailing comment covers its own line and the outermost node ending on it. An own-line comment
/// covers the outermost node starting at the next token, or just that token's line when no
/// declaration or statement starts there.
fn ignore_range(
    comment: &CommentStmt,
    source: &str,
    comments: &[CommentStmt],
    nodes: &[Span],
) -> Range<usize> {
    let line = line_range(source, comment.span.start);
    let own_line = source[line.start..comment.span.start].trim().is_empty();

    if !own_line {
        // When the formatter splits a long statement, its trailing comment stays after the last
        // token, lines below where the finding starts.
        let ending_here = line.start..=comment.span.start;
        let node_start = nodes
            .iter()
            .filter(|span| ending_here.contains(&span.end))
            .map(|span| span.start)
            .min();

        return node_start.map_or(line.start, |start| start.min(line.start))..line.end;
    }

    let next = next_token(source, comment.span.end, comments);
    let node_end = nodes
        .iter()
        .filter(|span| span.start == next)
        .map(|span| span.end)
        .max();

    match node_end {
        Some(end) => next..end,
        None => line_range(source, next),
    }
}

/// The position of the first token at or after `pos`, skipping whitespace and comments so stacked
/// suppression comments all reach the same node.
fn next_token(source: &str, mut pos: usize, comments: &[CommentStmt]) -> usize {
    loop {
        let rest = &source[pos..];
        pos += rest.len() - rest.trim_start().len();

        let Some(comment) = comments.iter().find(|comment| comment.span.start == pos) else {
            return pos;
        };

        pos = comment.span.end;
    }
}

fn line_range(source: &str, pos: usize) -> Range<usize> {
    let start = source[..pos].rfind('\n').map_or(0, |i| i + 1);
    let end = source[pos..].find('\n').map_or(source.len(), |i| pos + i);

    start..end
}

/// The spans of every declaration and statement a suppression comment can sit above.
fn node_spans(ast: &Ast) -> Vec<Span> {
    let mut spans = Vec::new();

    // The document's own span starts at the first token, so it's skipped or a comment above the
    // first declaration would cover the whole file.
    if let Ast::Document(nodes, _) = ast {
        collect_seq(nodes, &mut spans);
    }

    spans
}

fn collect_seq(nodes: &[Ast], spans: &mut Vec<Span>) {
    for (i, node) in nodes.iter().enumerate() {
        collect_ast(node, spans);

        // Annotations are siblings of the declaration they decorate, so a comment above them has
        // to reach past them to the declaration.
        let Ast::Annotation(_, annotation) = node else {
            continue;
        };

        let decorated = nodes[i + 1..]
            .iter()
            .find(|next| !matches!(next, Ast::Annotation(..)))
            .and_then(Ast::span);

        if let Some(decl) = decorated {
            spans.push(Span {
                start: annotation.start,
                end: decl.end,
            });
        }
    }
}

fn collect_ast(ast: &Ast, spans: &mut Vec<Span>) {
    if let Some(span) = ast.span() {
        spans.push(*span);
    }

    match ast {
        Ast::Document(nodes, _) => collect_seq(nodes, spans),
        Ast::Module(decl) => collect_seq(&decl.body, spans),
        Ast::Class(decl) => collect_seq(&decl.body, spans),
        Ast::Function(decl) => {
            if let Some(body) = &decl.body {
                collect_stmts(&body.stmts, spans);
            }
        }
        _ => {}
    }
}

fn collect_stmts(stmts: &[Stmt], spans: &mut Vec<Span>) {
    for stmt in stmts {
        collect_stmt(stmt, spans);
    }
}

fn collect_stmt(stmt: &Stmt, spans: &mut Vec<Span>) {
    spans.push(*stmt.span());

    match stmt {
        Stmt::Block(block) => collect_stmts(&block.stmts, spans),
        Stmt::If(s) => collect_if(s, spans),
        Stmt::While(s) => collect_stmts(&s.body.stmts, spans),
        Stmt::DoWhile(s) => collect_stmts(&s.body.stmts, spans),
        Stmt::For(s) => collect_stmts(&s.body.stmts, spans),
        Stmt::Switch(s) => {
            for case in &s.cases {
                collect_stmts(&case.stmts, spans);
            }
        }
        Stmt::Try(s) => {
            collect_stmts(&s.body.stmts, spans);

            for catch in &s.catches {
                collect_stmts(&catch.body.stmts, spans);
            }

            if let Some(finally) = &s.finally {
                collect_stmts(&finally.stmts, spans);
            }
        }
        _ => {}
    }
}

fn collect_if(s: &IfStmt, spans: &mut Vec<Span>) {
    collect_stmts(&s.then_branch.stmts, spans);

    match &s.else_branch {
        Some(ElseBranch::Block(block)) => collect_stmts(&block.stmts, spans),
        Some(ElseBranch::If(inner)) => collect_if(inner, spans),
        None => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn comment(text: &str) -> CommentStmt {
        CommentStmt {
            text: text.to_string(),
            is_block: false,
            span: Span { start: 0, end: 0 },
        }
    }

    /// The kind and rule names `text` parses to, leaving out the spans.
    fn parsed(text: &str) -> Option<Result<(Kind, Vec<String>), String>> {
        parse(&comment(text)).map(|parsed| {
            parsed.map(|directive| {
                let names = directive.rules.into_iter().map(|rule| rule.name).collect();

                (directive.kind, names)
            })
        })
    }

    fn directive(kind: Kind, rules: &[&str]) -> Option<Result<(Kind, Vec<String>), String>> {
        Some(Ok((
            kind,
            rules.iter().map(|rule| rule.to_string()).collect(),
        )))
    }

    /// The rule names of the findings left after suppression.
    fn rules(src: &str) -> Vec<&'static str> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src).iter().map(|d| d.rule).collect()
    }

    /// The messages of the findings `rule` reports.
    fn messages(src: &str, rule: &str) -> Vec<String> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src)
            .into_iter()
            .filter(|d| d.rule == rule)
            .map(|d| d.message)
            .collect()
    }

    /// `src` after applying every fix once, as `rafiki lint --fix` does.
    fn fixed(src: &str) -> String {
        let output = Parser::new(src).parse().expect("parse");
        let fixes = lint(&output, src)
            .into_iter()
            .filter_map(|d| d.fix)
            .collect();

        apply_fixes(src, fixes)
    }

    #[test]
    fn parses_ignore() {
        assert_eq!(
            parsed(" rafiki: ignore[bool-comparison]"),
            directive(Kind::Ignore, &["bool-comparison"])
        );
    }

    #[test]
    fn parses_file_ignore() {
        assert_eq!(
            parsed(" rafiki: file-ignore[one-class-per-file]"),
            directive(Kind::FileIgnore, &["one-class-per-file"])
        );
    }

    #[test]
    fn parses_rule_lists_with_spacing_and_trailing_comma() {
        assert_eq!(
            parsed(" rafiki: ignore[ a,b , c, ]"),
            directive(Kind::Ignore, &["a", "b", "c"])
        );
    }

    #[test]
    fn parses_without_spaces() {
        assert_eq!(parsed("rafiki:ignore[a]"), directive(Kind::Ignore, &["a"]));
    }

    #[test]
    fn ignores_the_reason() {
        assert_eq!(
            parsed(" rafiki: ignore[a] name comes from the SDK [sic]"),
            directive(Kind::Ignore, &["a"])
        );
    }

    #[test]
    fn skips_comments_that_are_not_directives() {
        for text in [
            " just a comment",
            "! rafiki: ignore[a]",
            " see rafiki: ignore[a]",
            " rafiki: fmt off",
            " rafiki: fmt on keeps the table aligned",
        ] {
            assert_eq!(parsed(text), None, "{text:?}");
        }
    }

    #[test]
    fn rejects_malformed_directives() {
        for text in [
            " rafiki: ignore",
            " rafiki: ignore [a]",
            " rafiki: ignore[]",
            " rafiki: ignore[ , ]",
            " rafiki: ignore[a",
            " rafiki: file-ignore",
            " rafiki:",
            " rafiki: ignroe[a]",
            " rafiki: fmt",
            " rafiki: fmt of",
        ] {
            assert!(matches!(parsed(text), Some(Err(_))), "{text:?}");
        }
    }

    #[test]
    fn trailing_comment_covers_its_line() {
        let src = r#"
var a = (1); // rafiki: ignore[unneeded-parens]
var b = (2);
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn trailing_comment_only_covers_listed_rules() {
        let src = r#"
function f() {
    var x = (y == true); // rafiki: ignore[bool-comparison]
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn trailing_comment_covers_the_statement_it_ends() {
        let src = r#"
function f() {
    var a = foo(
        (x == true)
    ); // rafiki: ignore[bool-comparison]
    var b = (y == true);
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens", "bool-comparison"]);
    }

    #[test]
    fn trailing_comment_after_an_opening_brace_covers_only_its_line() {
        let src = r#"
function f() {
    if (a == true) { // rafiki: ignore[bool-comparison]
        b = (c == true);
    }
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens", "bool-comparison"]);
    }

    #[test]
    fn trailing_block_comment_covers_its_line() {
        let src = r#"
var a = (1); /* rafiki: ignore[unneeded-parens] */
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn own_line_comment_covers_the_next_statement() {
        let src = r#"
function f() {
    // rafiki: ignore[unneeded-parens]
    var a = (1);
    var b = (2);
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn own_line_comment_covers_a_multi_line_statement() {
        let src = r#"
function f() {
    // rafiki: ignore[bool-comparison]
    foo(
        a == true,
        b == true
    );
}
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn own_line_comment_covers_a_whole_function() {
        let src = r#"
// rafiki: ignore[unneeded-parens]
function f() {
    var a = (1);
    if (a) {
        a = (2);
    }
}
var b = (3);
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn own_line_comment_covers_a_whole_class() {
        let src = r#"
// rafiki: ignore[naming-convention]
class foo {
    var Bar = 1;
}
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn own_line_comment_reaches_past_annotations() {
        let src = r#"
// rafiki: ignore[naming-convention]
(:test)
(:debug)
function Foo() {}
function Bar() {}
"#;

        assert_eq!(rules(src), vec!["naming-convention"]);
    }

    #[test]
    fn own_line_comment_covers_modifiers() {
        let src = r#"
class Foo {
    // rafiki: ignore[modifier-order]
    static private var _x = 1;
}
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn own_line_comment_covers_a_nested_statement() {
        let src = r#"
function f() {
    while (true) {
        // rafiki: ignore[unneeded-parens]
        var a = (1);
    }
    var b = (2);
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn own_line_comment_falls_back_to_the_next_line_inside_an_expression() {
        let src = r#"
var a = [
    // rafiki: ignore[bool-comparison]
    b == true,
    c == true,
];
"#;

        assert_eq!(rules(src), vec!["bool-comparison"]);
    }

    #[test]
    fn stacked_comments_reach_the_same_node() {
        let src = r#"
function f() {
    // rafiki: ignore[bool-comparison]
    // explains the next one
    // rafiki: ignore[unneeded-parens]
    var x = (y == true);
}
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn own_line_comment_before_a_closing_brace_covers_nothing() {
        let src = r#"
function f() {
    var a = (1);
    // rafiki: ignore[unneeded-parens]
}
"#;

        assert_eq!(rules(src), vec!["unneeded-parens", "unused-ignore"]);
    }

    #[test]
    fn comment_above_the_first_declaration_does_not_cover_the_file() {
        let src = r#"
// rafiki: ignore[unneeded-parens]
var a = (1);
var b = (2);
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn file_ignore_covers_the_whole_file() {
        let src = r#"
// Copyright header

// rafiki: file-ignore[one-class-per-file] view and delegate
class A {}
class B {}
"#;

        assert!(rules(src).is_empty());
    }

    #[test]
    fn file_ignore_after_code_is_invalid() {
        let src = r#"
class A {}
// rafiki: file-ignore[one-class-per-file]
class B {}
"#;

        assert_eq!(rules(src), vec!["invalid-directive", "one-class-per-file"]);
    }

    #[test]
    fn doc_comment_is_not_a_directive() {
        let src = r#"
//! rafiki: ignore[unneeded-parens]
var a = (1);
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }

    #[test]
    fn reports_an_unused_rule() {
        let src = r#"
var a = 1; // rafiki: ignore[unneeded-parens]
"#;

        assert_eq!(
            messages(src, unused_ignore::RULE),
            vec!["`unneeded-parens` is never reported here, so ignoring it has no effect"]
        );
    }

    #[test]
    fn does_not_report_rules_that_are_not_selected() {
        let src = r#"
var a = 1; // rafiki: ignore[unneeded-parens]
"#;
        let output = Parser::new(src).parse().expect("parse");

        let findings = crate::lint_selected(&output, src, |rule| rule != "unneeded-parens");

        assert!(findings.is_empty());
    }

    #[test]
    fn unused_ignore_can_be_turned_off() {
        let src = r#"
var a = 1; // rafiki: ignore[unneeded-parens]
"#;
        let output = Parser::new(src).parse().expect("parse");

        let findings = crate::lint_selected(&output, src, |rule| rule != unused_ignore::RULE);

        assert!(findings.is_empty());
    }

    #[test]
    fn fix_removes_an_own_line_comment_with_its_line() {
        let src = r#"
function f() {
    // rafiki: ignore[unneeded-parens] no longer needed
    var a = 1;
}
"#;

        assert_eq!(
            fixed(src),
            r#"
function f() {
    var a = 1;
}
"#
        );
    }

    #[test]
    fn fix_removes_a_trailing_comment_with_the_space_before_it() {
        let src = r#"
var a = 1;   // rafiki: ignore[unneeded-parens]
"#;

        assert_eq!(fixed(src), "\nvar a = 1;\n");
    }

    #[test]
    fn fix_keeps_the_rules_still_in_use() {
        let src = r#"
var a = (b == true); // rafiki: ignore[collapsible-if, unneeded-parens, compound-assignment, bool-comparison] reason
"#;

        assert_eq!(
            rules(src),
            vec!["unused-ignore", "unused-ignore"],
            "one finding per unused rule"
        );
        assert_eq!(
            fixed(src),
            r#"
var a = (b == true); // rafiki: ignore[unneeded-parens, bool-comparison] reason
"#
        );
    }

    #[test]
    fn fix_finds_the_rules_in_a_block_comment() {
        let src = r#"
var a = (1); /* rafiki: ignore[unneeded-parens,bool-comparison] */
"#;

        assert_eq!(
            fixed(src),
            r#"
var a = (1); /* rafiki: ignore[unneeded-parens] */
"#
        );
    }

    #[test]
    fn reports_an_unknown_rule() {
        let src = r#"
var a = (1); // rafiki: ignore[unneeded-paren, unneeded-parens]
"#;

        assert_eq!(rules(src), vec!["invalid-directive"]);
        assert_eq!(
            messages(src, invalid_directive::RULE),
            vec!["unknown rule `unneeded-paren`"]
        );
    }

    #[test]
    fn rules_about_suppressions_cannot_be_silenced() {
        let src = r#"
var a = 1; // rafiki: ignore[unused-ignore, invalid-directive]
"#;

        assert_eq!(
            messages(src, invalid_directive::RULE),
            vec![
                "`unused-ignore` can't be silenced by a comment",
                "`invalid-directive` can't be silenced by a comment",
            ]
        );
    }

    #[test]
    fn reports_a_malformed_suppression() {
        let src = r#"
var a = (1); // rafiki: ignore unneeded-parens
"#;

        assert_eq!(rules(src), vec!["unneeded-parens", "invalid-directive"]);
    }

    #[test]
    fn reports_an_unknown_directive() {
        let src = r#"
// rafiki: ignroe[unneeded-parens]
var a = (1);
"#;

        assert_eq!(rules(src), vec!["invalid-directive", "unneeded-parens"]);
        assert_eq!(
            messages(src, invalid_directive::RULE),
            vec![
                "`ignroe` is not a known `rafiki:` directive, the known ones are `ignore`, \
                 `file-ignore` and `fmt`"
            ]
        );
    }

    #[test]
    fn accepts_fmt_comments_around_statements() {
        let src = r#"
class A {
    // rafiki: fmt off
    (:debug)
    function f() {
        // rafiki: fmt off
        var a  =  1;
        // rafiki: fmt on
    }
    // rafiki: fmt on
}
"#;

        assert!(messages(src, "invalid-directive").is_empty());
    }

    #[test]
    fn reports_a_fmt_comment_sharing_its_line() {
        let src = r#"
function f() {
    var a  =  1; // rafiki: fmt off
    /* rafiki: fmt on */ var b  =  2;
}
"#;

        assert_eq!(
            messages(src, "invalid-directive"),
            vec![
                "`rafiki: fmt off` must be on a line of its own",
                "`rafiki: fmt on` must be on a line of its own",
            ]
        );
    }

    #[test]
    fn reports_fmt_off_without_a_statement_after_it() {
        let src = r#"
function f() {
    var a = {
        // rafiki: fmt off
        :a   =>   1,
    };
    // rafiki: fmt off
}
"#;

        assert_eq!(
            messages(src, "invalid-directive"),
            vec!["`rafiki: fmt off` must come before a declaration or statement"; 2]
        );
    }

    #[test]
    fn reports_fmt_on_without_fmt_off() {
        let src = r#"
// rafiki: fmt on
function f() {
    // rafiki: fmt off
    var a  =  1;
    // rafiki: fmt on
    var b = 2;
    // rafiki: fmt on
}
"#;

        assert_eq!(
            messages(src, "invalid-directive"),
            vec!["`rafiki: fmt on` without a `rafiki: fmt off` before it"; 2]
        );
    }

    #[test]
    fn reports_fmt_on_closing_a_region_in_another_body() {
        let src = r#"
function f() {
    if (a) {
        // rafiki: fmt off
        foo(  1  );
    }
    // rafiki: fmt on
}
"#;

        assert_eq!(
            messages(src, "invalid-directive"),
            vec!["`rafiki: fmt on` without a `rafiki: fmt off` before it"]
        );
    }

    #[test]
    fn reports_fmt_off_while_already_off() {
        let src = r#"
function f() {
    // rafiki: fmt off
    var a  =  1;
    // rafiki: fmt off
    var b  =  2;
    // rafiki: fmt on
}
"#;

        assert_eq!(
            messages(src, "invalid-directive"),
            vec!["formatting is already off from a `rafiki: fmt off` above"]
        );
    }

    #[test]
    fn skips_fmt_comments_inside_a_region() {
        let src = r#"
class A {
    // rafiki: fmt off
    function f() {
        // rafiki: fmt on
        var a  =  1; // rafiki: fmt off
    }
    // rafiki: fmt on
}

// rafiki: fmt off
function g() {
    // rafiki: fmt on
}
"#;

        assert!(messages(src, "invalid-directive").is_empty());
    }

    /// `src` after silencing its first `rule` finding with [`ignore_fix`].
    fn ignored(src: &str, rule: &str) -> Option<String> {
        let output = Parser::new(src).parse().expect("parse");
        let finding = lint(&output, src).into_iter().find(|d| d.rule == rule)?;

        Some(apply_fixes(src, vec![ignore_fix(&output, src, &finding)?]))
    }

    #[test]
    fn ignore_fix_adds_a_comment_at_the_end_of_the_line() {
        let src = r#"
function f() {
    var a = 1;
    var accentColorStart = (height / 6) * 5;
    var b = 2;
}
"#;

        assert_eq!(
            ignored(src, "unneeded-parens").as_deref(),
            Some(
                r#"
function f() {
    var a = 1;
    var accentColorStart = (height / 6) * 5; // rafiki: ignore[unneeded-parens]
    var b = 2;
}
"#
            )
        );
    }

    #[test]
    fn ignore_fix_goes_above_a_line_with_a_comment() {
        let src = r#"
function f() {
    var a = (1); // the answer
}
"#;

        assert_eq!(
            ignored(src, "unneeded-parens").as_deref(),
            Some(
                r#"
function f() {
    // rafiki: ignore[unneeded-parens]
    var a = (1); // the answer
}
"#
            )
        );
    }

    #[test]
    fn ignore_fix_adds_to_the_comment_at_the_end_of_the_line() {
        let src = r#"
var a = (b == true); // rafiki: ignore[unneeded-parens]
"#;

        assert_eq!(
            ignored(src, "bool-comparison").as_deref(),
            Some(
                r#"
var a = (b == true); // rafiki: ignore[unneeded-parens, bool-comparison]
"#
            )
        );
    }

    #[test]
    fn ignore_fix_covers_a_second_statement_on_the_line() {
        let src = r#"
var a = 1; var b = (2);
"#;
        let silenced = ignored(src, "unneeded-parens").expect("a fix");

        assert!(rules(&silenced).is_empty(), "{silenced}");
    }

    #[test]
    fn ignore_fix_silences_the_finding() {
        let src = r#"
function f() {
    foo(
        a,
        b == true
    );
}
"#;
        let silenced = ignored(src, "bool-comparison").expect("a fix");

        assert!(rules(&silenced).is_empty(), "{silenced}");
    }

    #[test]
    fn ignore_fix_adds_to_the_comment_above() {
        let src = r#"
function f() {
    // rafiki: ignore[unneeded-parens] parens kept for clarity
    var a = (b == true);
}
"#;

        assert_eq!(
            ignored(src, "bool-comparison").as_deref(),
            Some(
                r#"
function f() {
    // rafiki: ignore[unneeded-parens, bool-comparison] parens kept for clarity
    var a = (b == true);
}
"#
            )
        );
    }

    #[test]
    fn ignore_fix_keeps_crlf_line_endings() {
        let src = "function f() {\r\n    var a = (1); // why\r\n}\r\n";

        assert_eq!(
            ignored(src, "unneeded-parens").as_deref(),
            Some(
                "function f() {\r\n    // rafiki: ignore[unneeded-parens]\r\n    var a = (1); // why\r\n}\r\n"
            )
        );
    }

    #[test]
    fn ignore_fix_is_not_offered_for_rules_about_comments() {
        let src = r#"
var a = 1; // rafiki: ignore[unneeded-parens]
"#;

        assert_eq!(ignored(src, unused_ignore::RULE), None);
    }
}
