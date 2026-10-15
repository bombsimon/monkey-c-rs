//! `rafiki: ignore[...]` and `rafiki: file-ignore[...]` comments, which silence the rules they
//! name without turning them off for the whole project.
//!
//! An own-line `ignore` covers the whole node below it rather than just the next line, so it keeps
//! working when the formatter reflows that node over several lines.
use std::ops::Range;

use monkey_c_parser::ast::{Ast, CommentStmt, ElseBranch, IfStmt, Span, Stmt};
use monkey_c_parser::parser::ParseOutput;

use crate::Diagnostic;

const PREFIX: &str = "rafiki:";

/// Every suppression comment in a file, resolved to the source range it covers.
pub(crate) struct Suppressions {
    entries: Vec<Suppression>,
}

struct Suppression {
    rules: Vec<String>,
    range: Range<usize>,
}

impl Suppressions {
    pub(crate) fn new(output: &ParseOutput, source: &str) -> Self {
        let comments = &output.comments.comments;
        let first_token = next_token(source, 0, comments);
        let nodes = node_spans(&output.ast);

        let entries = comments
            .iter()
            .filter_map(|comment| {
                let directive = parse(comment)?;
                let range = match directive.kind {
                    Kind::Ignore => ignore_range(comment, source, comments, &nodes),
                    Kind::FileIgnore => file_range(comment, source, first_token)?,
                };

                Some(Suppression {
                    rules: directive.rules,
                    range,
                })
            })
            .collect();

        Self { entries }
    }

    /// Whether a comment silences `diagnostic`. Only the start of its span counts, so a finding
    /// covering a whole function belongs to the line the function starts on.
    pub(crate) fn covers(&self, diagnostic: &Diagnostic) -> bool {
        self.entries.iter().any(|entry| {
            entry.range.contains(&diagnostic.span.start)
                && entry.rules.iter().any(|rule| rule == diagnostic.rule)
        })
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
    rules: Vec<String>,
}

/// Read a suppression from `comment`, or `None` when it isn't one. Doc comments (`//!`) never
/// match since their text starts with the extra `!`. Anything after `]` is the optional reason.
fn parse(comment: &CommentStmt) -> Option<Directive> {
    let rest = comment.text.trim_start().strip_prefix(PREFIX)?.trim_start();

    let (kind, rest) = match rest.strip_prefix("file-ignore[") {
        Some(rest) => (Kind::FileIgnore, rest),
        None => (Kind::Ignore, rest.strip_prefix("ignore[")?),
    };

    let (list, _reason) = rest.split_once(']')?;
    let rules: Vec<String> = list
        .split(',')
        .map(str::trim)
        .filter(|rule| !rule.is_empty())
        .map(String::from)
        .collect();

    if rules.is_empty() {
        return None;
    }

    Some(Directive { kind, rules })
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

/// The whole file, as long as the comment comes before any code. A `file-ignore` further down
/// would hide a file-wide suppression where nobody looks for one.
fn file_range(comment: &CommentStmt, source: &str, first_token: usize) -> Option<Range<usize>> {
    if comment.span.end > first_token {
        return None;
    }

    Some(0..source.len())
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
    use crate::lint;
    use monkey_c_parser::parser::Parser;

    fn comment(text: &str) -> CommentStmt {
        CommentStmt {
            text: text.to_string(),
            is_block: false,
            span: Span { start: 0, end: 0 },
        }
    }

    fn directive(kind: Kind, rules: &[&str]) -> Option<Directive> {
        Some(Directive {
            kind,
            rules: rules.iter().map(|rule| rule.to_string()).collect(),
        })
    }

    /// The rule names of the findings left after suppression.
    fn rules(src: &str) -> Vec<&'static str> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src).iter().map(|d| d.rule).collect()
    }

    #[test]
    fn parses_ignore() {
        assert_eq!(
            parse(&comment(" rafiki: ignore[bool-comparison]")),
            directive(Kind::Ignore, &["bool-comparison"])
        );
    }

    #[test]
    fn parses_file_ignore() {
        assert_eq!(
            parse(&comment(" rafiki: file-ignore[one-class-per-file]")),
            directive(Kind::FileIgnore, &["one-class-per-file"])
        );
    }

    #[test]
    fn parses_rule_lists_with_spacing_and_trailing_comma() {
        assert_eq!(
            parse(&comment(" rafiki: ignore[ a,b , c, ]")),
            directive(Kind::Ignore, &["a", "b", "c"])
        );
    }

    #[test]
    fn parses_without_spaces() {
        assert_eq!(
            parse(&comment("rafiki:ignore[a]")),
            directive(Kind::Ignore, &["a"])
        );
    }

    #[test]
    fn ignores_the_reason() {
        assert_eq!(
            parse(&comment(" rafiki: ignore[a] name comes from the SDK [sic]")),
            directive(Kind::Ignore, &["a"])
        );
    }

    #[test]
    fn rejects_non_directives() {
        for text in [
            " just a comment",
            "! rafiki: ignore[a]",
            " rafiki: ignore",
            " rafiki: ignore[]",
            " rafiki: ignore[a",
            " rafiki: ignroe[a]",
            " rafiki: ignore [a]",
            " see rafiki: ignore[a]",
        ] {
            assert_eq!(parse(&comment(text)), None, "{text:?}");
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
    // rafiki: ignore[unneeded-parens]
    foo(
        (1),
        (2)
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

        assert_eq!(rules(src), vec!["unneeded-parens"]);
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
    fn file_ignore_after_code_does_nothing() {
        let src = r#"
class A {}
// rafiki: file-ignore[one-class-per-file]
class B {}
"#;

        assert_eq!(rules(src), vec!["one-class-per-file"]);
    }

    #[test]
    fn doc_comment_is_not_a_directive() {
        let src = r#"
//! rafiki: ignore[unneeded-parens]
var a = (1);
"#;

        assert_eq!(rules(src), vec!["unneeded-parens"]);
    }
}
