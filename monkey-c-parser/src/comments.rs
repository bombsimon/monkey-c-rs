//! Comment attachment — turns a flat [`CommentTable`] into a
//! [`CommentsMap`] keyed by node span, where each comment has a semantic
//! role (`leading`, `trailing`, or `dangling`).
//!
//! The attachment pass is a pure function of `(Ast, CommentTable)`, so it can
//! be re-run after a re-parse without drift.
use crate::ast::{
    Ast, CaseLabel, CommentStmt, CommentTable, ElseBranch, Expr, ForInit, InterfaceMember, Span,
    Stmt, Type, TypeKind,
};
use crate::line_index::LineIndex;
use std::collections::{HashMap, HashSet};

/// Where a `dangling` comment sits relative to its containing node's
/// structure. The renderer uses this to decide placement.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum DanglingPlacement {
    /// Comment is between the node's header (keyword, parens, etc.) and its
    /// opening `{`: `enum /* X */ {`, `function f() /* C */ {`,
    /// `if (x) /* C */ {`. The renderer emits it just before `{`.
    BeforeBracket,
    /// Comment is on the same source line as the opening `{` but *inside*
    /// the block — `if (x) { // C\n  body }`. The renderer emits it
    /// immediately after `{` on that line.
    AfterOpenBrace,
    /// Comment is inside the brace body but before the first child —
    /// e.g. `{ /* X */ first }` or `{ /* X */ }`.
    BeforeFirstChild,
    /// Comment is inside the node, after all of its direct children —
    /// e.g. `[ 1, 2, /* tail */ ]` or `(arg1, arg2, /* tail */)`.
    AfterLastChild,
    /// Generic "inside the node, between children, not adjacent to either."
    /// Each renderer interprets this for its own layout.
    Inside,
}

/// Attached comments, keyed by the span of the AST node they're attached to.
///
/// Each comment is owned exactly once in `comments` (the table); the three
/// role maps store indices into that table, so a single comment costs one
/// `CommentStmt` + `usize` instead of one clone per role.
#[derive(Debug, Default, Clone)]
pub struct CommentsMap {
    comments: Vec<CommentStmt>,
    leading: HashMap<Span, Vec<usize>>,
    trailing: HashMap<Span, Vec<usize>>,
    dangling: HashMap<(Span, DanglingPlacement), Vec<usize>>,
}

impl CommentsMap {
    pub fn new() -> Self {
        Self::default()
    }

    /// Comments attached to render just *before* the node at `span`.
    pub fn leading(&self, span: Span) -> impl Iterator<Item = &CommentStmt> + '_ {
        self.indexed(self.leading.get(&span))
    }

    /// Comments attached to render just *after* the node at `span`.
    pub fn trailing(&self, span: Span) -> impl Iterator<Item = &CommentStmt> + '_ {
        self.indexed(self.trailing.get(&span))
    }

    /// Comments inside the node but not adjacent to any child.
    pub fn dangling(
        &self,
        span: Span,
        placement: DanglingPlacement,
    ) -> impl Iterator<Item = &CommentStmt> + '_ {
        self.indexed(self.dangling.get(&(span, placement)))
    }

    /// Iterate every attached comment, regardless of role, in source order.
    pub fn iter(&self) -> impl Iterator<Item = &CommentStmt> + '_ {
        self.comments.iter()
    }

    /// Look up a slice of comments from a stored index list.
    fn indexed<'a>(
        &'a self,
        indices: Option<&'a Vec<usize>>,
    ) -> impl Iterator<Item = &'a CommentStmt> + 'a {
        indices.into_iter().flatten().map(|&i| &self.comments[i])
    }
}

/// `(header_end, brace_start)` for a brace-bodied container — the byte
/// offsets that bound the "between the header text and the opening `{`"
/// region. `header_end == 0` is the sentinel for "no header to skip past"
/// (used by enum/class/module, where the keyword and name are the entire
/// header and there's nothing after them to confuse the attach algorithm).
type BracketZone = (usize, usize);

/// Build a [`CommentsMap`] by walking `ast` and attaching every comment in
/// `table` to a node by source-position rules.
pub fn attach_comments(ast: &Ast, table: &CommentTable, line_index: &LineIndex) -> CommentsMap {
    let mut spans: Vec<Span> = Vec::new();
    let mut brace_starts: HashMap<Span, BracketZone> = HashMap::new();
    let mut block_spans: HashSet<Span> = HashSet::new();
    collect_spans_ast(ast, &mut spans, &mut brace_starts, &mut block_spans);
    spans.sort_by(|a, b| a.start.cmp(&b.start).then_with(|| b.end.cmp(&a.end)));

    let mut map = CommentsMap {
        comments: table.comments.clone(),
        leading: HashMap::new(),
        trailing: HashMap::new(),
        dangling: HashMap::new(),
    };

    // A "cluster anchor" — when the previous comment was attached as an
    // own-line comment, this lets the next comment on the immediately
    // following source line stack onto the same anchor.
    let mut cluster: Option<(u32, AttachTarget)> = None;

    for (i, comment) in table.comments.iter().enumerate() {
        let comment_start_line = line_index.line_col(comment.span.start as u32).line;
        let comment_end_line = line_index
            .line_col(comment.span.end.saturating_sub(1) as u32)
            .line;

        if let Some((prev_end_line, ref target)) = cluster
            && comment_start_line <= prev_end_line + 1
            && is_own_line_comment(comment, &spans, line_index)
        {
            match target {
                AttachTarget::Trailing(span) => {
                    map.trailing.entry(*span).or_default().push(i);
                }
                AttachTarget::Leading(span) => {
                    map.leading.entry(*span).or_default().push(i);
                }
                AttachTarget::Dangling(span, placement) => {
                    map.dangling.entry((*span, *placement)).or_default().push(i);
                }
            }

            cluster = Some((comment_end_line, target.clone()));
            continue;
        }

        let outcome = attach_one(
            i,
            comment,
            &spans,
            &brace_starts,
            &block_spans,
            line_index,
            &mut map,
        );
        cluster = outcome.and_then(|o| {
            // Only own-line attachments seed a cluster anchor. Same-line
            // trailing/leading should not pull subsequent comments off
            // their natural targets.
            if o.own_line {
                Some((comment_end_line, o.target))
            } else {
                None
            }
        });
    }
    map
}

/// Internal: where the most-recent comment attached. Used by the cluster
/// rule so a stack of consecutive own-line comments all flow to the same
/// target.
#[derive(Clone)]
enum AttachTarget {
    Trailing(Span),
    Leading(Span),
    Dangling(Span, DanglingPlacement),
}

/// Internal: was the most-recent comment attached as a same-line
/// trailing/leading (which shouldn't cluster — each comment belongs with
/// its own neighbour), or as an own-line attachment (which *should* cluster
/// so a stack of `// ...` lines all flow to the same anchor)?
#[derive(Clone)]
struct AttachOutcome {
    target: AttachTarget,
    own_line: bool,
}

/// Cheap check: would this comment attach as same-line trailing/leading
/// (i.e. *not* an own-line comment)? Used to gate the cluster rule —
/// a same-line trailing comment shouldn't be pulled onto a previous
/// own-line comment's anchor.
fn is_own_line_comment(comment: &CommentStmt, spans: &[Span], line_index: &LineIndex) -> bool {
    let cs_line = line_index.line_col(comment.span.start as u32).line;
    let ce_line = line_index
        .line_col(comment.span.end.saturating_sub(1) as u32)
        .line;
    let prev_same_line = spans
        .iter()
        .filter(|s| s.end <= comment.span.start)
        .any(|s| line_index.line_col(s.end.saturating_sub(1) as u32).line == cs_line);
    let next_same_line = spans
        .iter()
        .filter(|s| s.start >= comment.span.end)
        .any(|s| line_index.line_col(s.start as u32).line == ce_line);

    !prev_same_line && !next_same_line
}

fn attach_one(
    i: usize,
    comment: &CommentStmt,
    spans: &[Span],
    brace_starts: &HashMap<Span, BracketZone>,
    block_spans: &HashSet<Span>,
    line_index: &LineIndex,
    map: &mut CommentsMap,
) -> Option<AttachOutcome> {
    let comment_start_line = line_index.line_col(comment.span.start as u32).line;
    let comment_end_line = line_index
        .line_col(comment.span.end.saturating_sub(1) as u32)
        .line;

    // Smallest span that fully contains the comment — its parent.
    let containing = spans
        .iter()
        .filter(|s| s.start <= comment.span.start && s.end >= comment.span.end)
        .min_by_key(|s| s.end - s.start)
        .copied();

    // If `containing` has a registered bracket zone and the comment sits in
    // the `[header_end, brace_start)` slot, this is a header-position
    // comment: `enum /* X */ {`, `function f() /* C */ {`, `if (x) /* C */ {`.
    // Resolve to `Dangling(BeforeBracket, containing)` so the renderer can
    // emit it between the header text and the opening `{`. Must run before
    // the same-line-on-next rule below, otherwise the comment would be
    // grabbed as leading on the first body child.
    if let Some(c) = containing
        && let Some(&(header_end, brace)) = brace_starts.get(&c)
        && comment.span.start >= header_end
        && comment.span.end <= brace
    {
        map.dangling
            .entry((c, DanglingPlacement::BeforeBracket))
            .or_default()
            .push(i);

        return Some(AttachOutcome {
            target: AttachTarget::Dangling(c, DanglingPlacement::BeforeBracket),
            own_line: false,
        });
    }

    // Nearest preceding node — latest-ending span entirely before the comment.
    // Among same-end spans, prefer the *largest* (outermost) so a trailing
    // comment attaches to the span the statement/clause renderer queries
    // (e.g. `Stmt::Expr` for `x = 0; // C`, not the inner `LitExpr 0`).
    // Mirrors the `next` selection's tiebreak below.
    let prev = spans
        .iter()
        .filter(|s| s.end <= comment.span.start)
        .min_by(|a, b| {
            b.end
                .cmp(&a.end)
                .then_with(|| (b.end - b.start).cmp(&(a.end - a.start)))
        })
        .copied();

    // Nearest succeeding node — earliest-starting span entirely after the
    // comment. Among same-start spans, prefer the smallest (deepest).
    // Nearest succeeding node — earliest-starting span entirely after the
    // comment. Among same-start spans, prefer the *largest* (outermost) so a
    // leading comment on `// step 2\n doY();` attaches to the `Stmt::Expr`
    // span (which the formatter renders) rather than the inner `IdentExpr`.
    let next = spans
        .iter()
        .filter(|s| s.start >= comment.span.end)
        .min_by(|a, b| {
            a.start
                .cmp(&b.start)
                .then_with(|| (b.end - b.start).cmp(&(a.end - a.start)))
        })
        .copied();

    let prev_same_line = prev
        .map(|p| line_index.line_col(p.end.saturating_sub(1) as u32).line == comment_start_line);
    let next_same_line = next.map(|n| line_index.line_col(n.start as u32).line == comment_end_line);
    // "Adjacent" = no blank source line between the comment and the neighbour.
    let prev_adjacent = prev.is_some_and(|p| {
        line_index.blank_lines_between(p.end as u32, comment.span.start as u32) == 0
    });
    let next_adjacent = next.is_some_and(|n| {
        line_index.blank_lines_between(comment.span.end as u32, n.start as u32) == 0
    });

    // "Within" the containing node: only consider prev/next that are
    // strict children of `containing`. A comment sitting between the
    // containing's start and its first child — or between its last child
    // and the containing's end — is a dangling comment on the containing
    // node, *not* a leading/trailing of some outer sibling.
    let prev_within = prev
        .and_then(|p| containing.and_then(|c| (p.start >= c.start && p.end <= c.end).then_some(p)));
    let next_within = next
        .and_then(|n| containing.and_then(|c| (n.start >= c.start && n.end <= c.end).then_some(n)));

    // Same-line trailing/leading is only valid when the neighbour sits in the
    // same containing scope as the comment. Crossing a container boundary
    // (e.g. `if (cond) { // C`, where prev=`cond` is outside the block body
    // that contains the comment) would render the comment in the wrong
    // structural slot. When there is no containing node (top-level), no
    // boundary can be crossed, so accept the same-line neighbour as-is.
    let prev_same_line_in_scope =
        matches!(prev_same_line, Some(true)) && (containing.is_none() || prev_within.is_some());
    let next_same_line_in_scope =
        matches!(next_same_line, Some(true)) && (containing.is_none() || next_within.is_some());

    // Attachment rules, in priority order:
    //
    // 1. Same line on both sides → tiebreak by source distance. Covers
    //    `op /* C */ rhs` (closer to rhs → leading on rhs) and
    //    `lhs /* C */ op rhs` (closer to lhs → trailing on lhs).
    // 2. Same line on prev only → trailing on prev (`x; // tail`).
    // 3. Same line on next only → leading on next (`/* x */ foo`).
    // 4. Adjacent (no blank line) on prev only → trailing on prev. Covers
    //    a trailing comment that immediately follows a member.
    // 5. Adjacent on next only — or both adjacent — leading on next. Covers
    //    `// header` style own-line comments above a node.
    // 6. Adjacent on neither → dangling on the containing node, or fall
    //    back to leading-on-next / trailing-on-prev if no parent exists.
    match (prev_same_line_in_scope, next_same_line_in_scope) {
        (true, true) => {
            let p = prev.unwrap();
            let n = next.unwrap();
            let dist_prev = comment.span.start.saturating_sub(p.end);
            let dist_next = n.start.saturating_sub(comment.span.end);
            return Some(if dist_prev <= dist_next {
                map.trailing.entry(p).or_default().push(i);
                AttachOutcome {
                    target: AttachTarget::Trailing(p),
                    own_line: false,
                }
            } else {
                map.leading.entry(n).or_default().push(i);
                AttachOutcome {
                    target: AttachTarget::Leading(n),
                    own_line: false,
                }
            });
        }
        (true, false) => {
            let p = prev.unwrap();
            map.trailing.entry(p).or_default().push(i);
            return Some(AttachOutcome {
                target: AttachTarget::Trailing(p),
                own_line: false,
            });
        }
        (false, true) => {
            let n = next.unwrap();
            map.leading.entry(n).or_default().push(i);
            return Some(AttachOutcome {
                target: AttachTarget::Leading(n),
                own_line: false,
            });
        }
        _ => {}
    }

    if let Some(c) = containing {
        match (prev_within, next_within) {
            (None, Some(_)) => {
                // Inside `c` but before any child. When `c` is a block whose
                // opening `{` sits on the comment's own source line, render
                // it as `{ // C` (same line). Otherwise render on its own
                // line above the first child.
                let block_open_same_line = block_spans.contains(&c)
                    && line_index.line_col(c.start as u32).line == comment_start_line;
                let placement = if block_open_same_line {
                    DanglingPlacement::AfterOpenBrace
                } else {
                    DanglingPlacement::BeforeFirstChild
                };
                map.dangling.entry((c, placement)).or_default().push(i);
                return Some(AttachOutcome {
                    target: AttachTarget::Dangling(c, placement),
                    own_line: !block_open_same_line,
                });
            }
            (Some(_), None) => {
                // Inside `c` but after every child — dangling
                // `AfterLastChild`. Used by `[ …, /* tail */ ]` and
                // `( arg, /* tail */ )` style positions.
                map.dangling
                    .entry((c, DanglingPlacement::AfterLastChild))
                    .or_default()
                    .push(i);
                return Some(AttachOutcome {
                    target: AttachTarget::Dangling(c, DanglingPlacement::AfterLastChild),
                    own_line: true,
                });
            }
            (None, None) => {
                // The container has no children near the comment — it's an
                // empty body (`catch (e) { /* C */ }`, `function f() { /* C */ }`).
                // Without this branch, the next rule would attach the comment
                // as trailing on an *external* prev (the previous block's
                // `}`), leaking the comment out of the container.
                map.dangling
                    .entry((c, DanglingPlacement::Inside))
                    .or_default()
                    .push(i);
                return Some(AttachOutcome {
                    target: AttachTarget::Dangling(c, DanglingPlacement::Inside),
                    own_line: true,
                });
            }
            _ => {}
        }
    }

    match (prev_adjacent, next_adjacent) {
        (true, false) => {
            let p = prev.unwrap();
            map.trailing.entry(p).or_default().push(i);
            return Some(AttachOutcome {
                target: AttachTarget::Trailing(p),
                own_line: true,
            });
        }
        (false, true) | (true, true) => {
            let n = next.unwrap();
            map.leading.entry(n).or_default().push(i);
            return Some(AttachOutcome {
                target: AttachTarget::Leading(n),
                own_line: true,
            });
        }
        (false, false) => {}
    }

    if let Some(c) = containing {
        map.dangling
            .entry((c, DanglingPlacement::Inside))
            .or_default()
            .push(i);

        return Some(AttachOutcome {
            target: AttachTarget::Dangling(c, DanglingPlacement::Inside),
            own_line: true,
        });
    }

    if let Some(n) = next {
        map.leading.entry(n).or_default().push(i);
        Some(AttachOutcome {
            target: AttachTarget::Leading(n),
            own_line: true,
        })
    } else if let Some(p) = prev {
        map.trailing.entry(p).or_default().push(i);
        Some(AttachOutcome {
            target: AttachTarget::Trailing(p),
            own_line: true,
        })
    } else {
        None
    }
}

fn collect_spans_ast(
    ast: &Ast,
    out: &mut Vec<Span>,
    brace_starts: &mut HashMap<Span, BracketZone>,
    block_spans: &mut HashSet<Span>,
) {
    if let Some(s) = ast.span() {
        out.push(*s);
    }

    match ast {
        Ast::Document(nodes, _) => {
            for n in nodes {
                collect_spans_ast(n, out, brace_starts, block_spans);
            }
        }
        Ast::Module(decl) => {
            brace_starts.insert(decl.span, (0, decl.brace_start));
            for n in &decl.body {
                collect_spans_ast(n, out, brace_starts, block_spans);
            }
        }
        Ast::Class(decl) => {
            brace_starts.insert(decl.span, (0, decl.brace_start));
            for n in &decl.body {
                collect_spans_ast(n, out, brace_starts, block_spans);
            }
        }
        Ast::Function(decl) => {
            brace_starts.insert(decl.span, (decl.header_end, decl.body.span.start));
            for arg in &decl.args.inner {
                out.push(arg.span);
                if let Some(ty) = &arg.type_ {
                    collect_spans_type(ty, out, block_spans);
                }

                if let Some(init) = &arg.initializer {
                    collect_spans_expr(init, out, block_spans);
                }
            }
            if let Some(ret) = &decl.returns {
                collect_spans_type(ret, out, block_spans);
            }

            for stmt in &decl.body.stmts {
                collect_spans_stmt(stmt, out, brace_starts, block_spans);
            }

            block_spans.insert(decl.body.span);
            out.push(decl.body.span);
        }
        Ast::Enum(decl) => {
            brace_starts.insert(decl.span, (0, decl.brace_start));
            for variant in &decl.variants {
                out.push(variant.span);
                if let Some(v) = &variant.value {
                    collect_spans_expr(v, out, block_spans);
                }
            }
        }
        Ast::Variable(decl) => {
            for b in &decl.bindings {
                out.push(b.span);
                if let Some(ty) = &b.type_ {
                    collect_spans_type(ty, out, block_spans);
                }

                if let Some(init) = &b.initializer {
                    collect_spans_expr(init, out, block_spans);
                }
            }
        }
        Ast::Const(decl) => {
            for b in &decl.bindings {
                out.push(b.span);
                if let Some(ty) = &b.type_ {
                    collect_spans_type(ty, out, block_spans);
                }

                if let Some(init) = &b.initializer {
                    collect_spans_expr(init, out, block_spans);
                }
            }
        }
        Ast::Typedef(decl) => collect_spans_type(&decl.type_, out, block_spans),
        Ast::Import(_) | Ast::Using(_) => {}
        Ast::Annotation(_, _) | Ast::Eof => {}
    }
}

fn collect_spans_stmt(
    stmt: &Stmt,
    out: &mut Vec<Span>,
    brace_starts: &mut HashMap<Span, BracketZone>,
    block_spans: &mut HashSet<Span>,
) {
    out.push(*stmt.span());
    match stmt {
        Stmt::Block(b) => {
            block_spans.insert(b.span);
            for s in &b.stmts {
                collect_spans_stmt(s, out, brace_starts, block_spans);
            }
        }
        Stmt::If(s) => collect_spans_if(s, out, brace_starts, block_spans),
        Stmt::While(s) => {
            brace_starts.insert(s.span, (s.condition.close, s.body.span.start));
            collect_spans_expr(&s.condition.inner, out, block_spans);

            for sub in &s.body.stmts {
                collect_spans_stmt(sub, out, brace_starts, block_spans);
            }

            block_spans.insert(s.body.span);
            out.push(s.body.span);
        }
        Stmt::DoWhile(s) => {
            brace_starts.insert(s.span, (s.header_end, s.body.span.start));
            collect_spans_expr(&s.condition, out, block_spans);

            for sub in &s.body.stmts {
                collect_spans_stmt(sub, out, brace_starts, block_spans);
            }
            block_spans.insert(s.body.span);
            out.push(s.body.span);
        }
        Stmt::For(s) => {
            brace_starts.insert(s.span, (s.header.close, s.body.span.start));
            if let Some(init) = &s.header.inner.init {
                match init {
                    ForInit::Var(_) => {}
                    ForInit::Expr(e) => collect_spans_expr(e, out, block_spans),
                }
            }

            if let Some(c) = &s.header.inner.condition {
                collect_spans_expr(c, out, block_spans);
            }

            if let Some(u) = &s.header.inner.update {
                collect_spans_expr(u, out, block_spans);
            }

            for sub in &s.body.stmts {
                collect_spans_stmt(sub, out, brace_starts, block_spans);
            }

            block_spans.insert(s.body.span);
            out.push(s.body.span);
        }
        Stmt::Switch(s) => {
            brace_starts.insert(s.span, (s.discriminant.close, s.brace_start));
            collect_spans_expr(&s.discriminant.inner, out, block_spans);
            for case in &s.cases {
                out.push(case.span);
                out.push(case.label_span);

                // Treat the region after `:` like a `{` block so comments
                // immediately after the colon attach as `AfterOpenBrace` and
                // own-line comments below attach as `BeforeFirstChild` — the
                // same machinery used for `if`/`while`/`for` bodies.
                let body_span = Span {
                    start: case.label_span.end,
                    end: case.span.end,
                };
                block_spans.insert(body_span);
                out.push(body_span);

                match &case.label {
                    CaseLabel::Value(e) => collect_spans_expr(e, out, block_spans),
                    CaseLabel::InstanceOf(ty) => collect_spans_type(ty, out, block_spans),
                    CaseLabel::Default => {}
                }

                for sub in &case.stmts {
                    collect_spans_stmt(sub, out, brace_starts, block_spans);
                }
            }
        }
        Stmt::Try(s) => {
            brace_starts.insert(s.span, (s.header_end, s.body.span.start));
            for sub in &s.body.stmts {
                collect_spans_stmt(sub, out, brace_starts, block_spans);
            }

            block_spans.insert(s.body.span);
            out.push(s.body.span);

            for catch in &s.catches {
                if let Some(ty) = &catch.type_filter {
                    collect_spans_type(ty, out, block_spans);
                }

                for sub in &catch.body.stmts {
                    collect_spans_stmt(sub, out, brace_starts, block_spans);
                }

                block_spans.insert(catch.body.span);
                out.push(catch.body.span);
            }

            if let Some(f) = &s.finally {
                for sub in &f.stmts {
                    collect_spans_stmt(sub, out, brace_starts, block_spans);
                }

                block_spans.insert(f.span);
                out.push(f.span);
            }
        }
        Stmt::Throw(s) => {
            collect_spans_expr(&s.value, out, block_spans);
        }
        Stmt::Return(s) => {
            if let Some(v) = &s.value {
                collect_spans_expr(v, out, block_spans);
            }
        }
        Stmt::Var(v) => {
            for b in &v.bindings {
                out.push(b.span);
                if let Some(ty) = &b.type_ {
                    collect_spans_type(ty, out, block_spans);
                }

                if let Some(init) = &b.initializer {
                    collect_spans_expr(init, out, block_spans);
                }
            }
        }
        Stmt::Expr(e) => collect_spans_expr(e, out, block_spans),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

/// Collect spans for an `IfStmt`, including its then-branch span, its
/// condition's spans, and (recursively) any else-branch.
fn collect_spans_if(
    s: &crate::ast::IfStmt,
    out: &mut Vec<Span>,
    brace_starts: &mut HashMap<Span, BracketZone>,
    block_spans: &mut HashSet<Span>,
) {
    brace_starts.insert(s.span, (s.condition.close, s.then_branch.span.start));
    collect_spans_expr(&s.condition.inner, out, block_spans);
    block_spans.insert(s.then_branch.span);
    out.push(s.then_branch.span);

    for sub in &s.then_branch.stmts {
        collect_spans_stmt(sub, out, brace_starts, block_spans);
    }

    if let Some(else_branch) = &s.else_branch {
        match else_branch {
            ElseBranch::Block(b) => {
                block_spans.insert(b.span);
                out.push(b.span);
                for sub in &b.stmts {
                    collect_spans_stmt(sub, out, brace_starts, block_spans);
                }
            }
            ElseBranch::If(inner) => {
                out.push(inner.span);
                collect_spans_if(inner, out, brace_starts, block_spans);
            }
        }
    }
}

/// Register comment-anchor spans for everything inside `ty`. Recurses into
/// generic parameters and union alternatives so a comment anywhere in a
/// nested type tree (e.g. an `interface { … }` body) can attach to the
/// inner node rather than landing on the outermost enclosing scope.
fn collect_spans_type(ty: &Type, out: &mut Vec<Span>, block_spans: &mut HashSet<Span>) {
    match &ty.kind {
        TypeKind::Named { generic_params, .. } => {
            for p in generic_params {
                collect_spans_type(p, out, block_spans);
            }
        }
        TypeKind::Dict { .. } => {}
        TypeKind::Tuple { elements } => {
            for el in elements {
                collect_spans_type(el, out, block_spans);
            }
        }
        TypeKind::Method { args, returns, .. } => {
            for arg in args {
                out.push(arg.span);

                if let Some(t) = &arg.type_ {
                    collect_spans_type(t, out, block_spans);
                }
            }

            if let Some(ret) = returns {
                collect_spans_type(ret, out, block_spans);
            }
        }
        TypeKind::Group(group) => collect_spans_type(&group.inner, out, block_spans),
        TypeKind::Interface { members, body_span } => {
            block_spans.insert(*body_span);
            out.push(*body_span);
            for member in members {
                match member {
                    InterfaceMember::Function(m) => {
                        out.push(m.span);
                        for arg in &m.args {
                            out.push(arg.span);
                            if let Some(t) = &arg.type_ {
                                collect_spans_type(t, out, block_spans);
                            }
                        }

                        if let Some(ret) = &m.returns {
                            collect_spans_type(ret, out, block_spans);
                        }
                    }
                    InterfaceMember::Variable(v) => {
                        out.push(v.span);
                        collect_spans_type(&v.type_, out, block_spans);
                    }
                }
            }
        }
    }

    for alt in &ty.alternatives {
        collect_spans_type(alt, out, block_spans);
    }
}

fn collect_spans_expr(expr: &Expr, out: &mut Vec<Span>, block_spans: &mut HashSet<Span>) {
    out.push(*expr.span());

    match expr {
        Expr::Binary(b) => {
            collect_spans_expr(&b.left, out, block_spans);
            collect_spans_expr(&b.right, out, block_spans);
        }
        Expr::Unary(u) => collect_spans_expr(&u.operand, out, block_spans),
        Expr::Ternary(t) => {
            collect_spans_expr(&t.cond, out, block_spans);
            collect_spans_expr(&t.then_expr, out, block_spans);
            collect_spans_expr(&t.else_expr, out, block_spans);
        }
        Expr::Assign(a) => {
            collect_spans_expr(&a.target, out, block_spans);
            collect_spans_expr(&a.value, out, block_spans);
        }
        Expr::Call(c) => {
            collect_spans_expr(&c.callee, out, block_spans);
            let args_span = Span {
                start: c.args_open,
                end: c.span.end,
            };
            block_spans.insert(args_span);
            out.push(args_span);
            for arg in &c.args {
                collect_spans_expr(&arg.value, out, block_spans);
            }
        }
        Expr::Member(m) => collect_spans_expr(&m.object, out, block_spans),
        Expr::Index(i) => {
            collect_spans_expr(&i.object, out, block_spans);
            collect_spans_expr(&i.index, out, block_spans);
        }
        Expr::New(n) => {
            let args_span = Span {
                start: n.args_open,
                end: n.span.end,
            };
            block_spans.insert(args_span);
            out.push(args_span);
            for arg in &n.args {
                collect_spans_expr(&arg.value, out, block_spans);
            }
        }
        Expr::NewArray(n) => collect_spans_expr(&n.size, out, block_spans),
        Expr::TypeCast(t) => {
            collect_spans_expr(&t.expr, out, block_spans);
            collect_spans_type(&t.target_type, out, block_spans);
        }
        Expr::Array(a) => {
            for entry in &a.entries {
                collect_spans_expr(&entry.value, out, block_spans);
            }
        }
        Expr::Dict(d) => {
            for entry in &d.entries {
                collect_spans_expr(&entry.key, out, block_spans);
                collect_spans_expr(&entry.value, out, block_spans);
            }
        }
        Expr::Paren(p) => collect_spans_expr(&p.inner, out, block_spans),
        Expr::Lit(_) | Expr::Ident(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;

    fn attach(src: &str) -> CommentsMap {
        let out = Parser::new(src).parse().expect("parse");
        let line_index = LineIndex::new(src);
        attach_comments(&out.ast, &out.comments, &line_index)
    }

    fn comment_texts<'a>(comments: impl IntoIterator<Item = &'a CommentStmt>) -> Vec<&'a str> {
        comments.into_iter().map(|c| c.text.as_str()).collect()
    }

    #[test]
    fn leading_on_right_operand_inside_binary() {
        // The block comment between `>=` and `1` should attach as leading on
        // the right-hand literal `1`.
        let src = "function f() { if (x >= /* C */ 1) {} }";
        let map = attach(src);
        // Find the `1` literal's span by scanning for it in the source.
        let start = src.find("1)").unwrap();
        let span = Span {
            start,
            end: start + 1,
        };
        assert_eq!(comment_texts(map.leading(span)), vec![" C "]);
    }

    #[test]
    fn standalone_comment_between_stmts_attaches_to_next() {
        let src = "function f() {\n    doX();\n    // step 2\n    doY();\n}\n";
        let map = attach(src);
        // `// step 2` is adjacent to both `doX();` and `doY();` (no blank line on either
        // side), so it attaches as leading on the next statement (`doY();`).
        let doy_start = src.find("doY()").unwrap();
        let doy_end = src[doy_start..].find(';').unwrap() + doy_start;
        let span = Span {
            start: doy_start,
            end: doy_end,
        };
        assert_eq!(comment_texts(map.leading(span)), vec![" step 2"]);
    }
}
