//! `collapsible-if` / `collapsible-else-if` — flag nested `if` statements that
//! add a level of nesting without adding logic.
//!
//! `collapsible-if`: an `if` whose body is a single `if` (and neither has an
//! `else`) can merge into one `if` with the conditions `&&`-combined.
//!
//! ```monkeyc
//! if (x) {
//!     if (y) {
//!         // …
//!     }
//! }
//! // → if (x && y) { … }
//! ```
//!
//! `collapsible-else-if`: an `else` whose body is a single `if` can be written
//! as `else if`.
//!
//! ```monkeyc
//! if (x) {
//! } else {
//!     if (y) {
//!     }
//! }
//! // → if (x) { } else if (y) { }
//! ```
//!
//! Both are left alone when a comment sits around the nested `if` in the outer
//! block: collapsing would discard it.
use monkey_c_parser::ast::{
    BinaryOperator, BlockStmt, ElseBranch, Expr, IfStmt, Parens, Span, Stmt,
};

use crate::visit::LintContext;
use crate::{Diagnostic, Edit, Fix};

pub const RULE_IF: &str = "collapsible-if";
pub const RULE_ELSE_IF: &str = "collapsible-else-if";

pub fn check_if(s: &IfStmt, ctx: &LintContext) -> Option<Diagnostic> {
    check_collapsible_if(s, ctx).or_else(|| check_collapsible_else_if(s, ctx))
}

/// `if (a) { if (b) { … } }` → `if (a && b) { … }`. Requires that neither the
/// outer nor the inner `if` has an `else` — an `else` on either would change
/// which condition guards the fall-through, so the merge wouldn't preserve
/// behaviour.
fn check_collapsible_if(s: &IfStmt, ctx: &LintContext) -> Option<Diagnostic> {
    if s.else_branch.is_some() {
        return None;
    }

    let inner = single_if(&s.then_branch)?;
    if inner.else_branch.is_some() {
        return None;
    }

    if block_has_comment_around(ctx.source, &s.then_branch, inner.span) {
        return None;
    }

    let combined = format!(
        "{} && {}",
        condition_text(&s.condition, ctx.source),
        condition_text(&inner.condition, ctx.source),
    );

    // Two disjoint edits, leaving the (possibly large) body untouched between
    // them: rewrite `if (a) { if (b) {` into `if (a && b) {`, and drop the now
    // redundant outer `}`. The diagnostic points at the same nesting header.
    let header = Span {
        start: s.span.start,
        end: inner.then_branch.span.start + 1,
    };
    let header_edit = Edit {
        span: header,
        replacement: format!("if ({combined}) {{"),
    };
    let outer_brace = Edit {
        span: Span {
            start: inner.then_branch.span.end,
            end: s.span.end,
        },
        replacement: String::new(),
    };

    Some(Diagnostic {
        rule: RULE_IF,
        message: "this `if` and its single nested `if` can be collapsed by combining the conditions with `&&`".into(),
        span: header,
        fix: Some(Fix {
            edits: vec![header_edit, outer_brace],
        }),
    })
}

/// `else { if (b) { … } }` → `else if (b) { … }`. The nested `if` keeps its own
/// `else` chain, so only the wrapping block is removed.
fn check_collapsible_else_if(s: &IfStmt, ctx: &LintContext) -> Option<Diagnostic> {
    let ElseBranch::Block(block) = s.else_branch.as_ref()? else {
        return None;
    };

    let inner = single_if(block)?;
    if block_has_comment_around(ctx.source, block, inner.span) {
        return None;
    }

    // Two disjoint edits that unwrap the block, leaving the nested `if`'s body
    // (and any `else` chain) untouched between them. The first rewrites the
    // `else { … if (b) {` header into `else if (b) {` — copying only the small
    // condition, never the body — so the fix note can show the resulting line.
    // The second drops the block's trailing `}`.
    let else_start = s.else_kw_start.unwrap_or(block.span.start);
    let header = Span {
        start: else_start,
        end: inner.then_branch.span.start + 1,
    };
    let inner_header = &ctx.source[inner.span.start..header.end];
    let header_edit = Edit {
        span: header,
        replacement: format!("else {inner_header}"),
    };
    let close_edit = Edit {
        span: Span {
            start: inner.span.end,
            end: block.span.end,
        },
        replacement: String::new(),
    };

    Some(Diagnostic {
        rule: RULE_ELSE_IF,
        message: "this `else` block wraps a single `if` and can be written as `else if`".into(),
        span: header,
        fix: Some(Fix {
            edits: vec![header_edit, close_edit],
        }),
    })
}

/// The single `if` statement making up `block`'s entire body, if that's all it
/// contains.
fn single_if(block: &BlockStmt) -> Option<&IfStmt> {
    match block.stmts.as_slice() {
        [Stmt::If(inner)] => Some(inner),
        _ => None,
    }
}

/// Whether `block` has a comment in the gap before or after the nested `inner`
/// statement. Those gaps hold only whitespace and comments — the block's sole
/// statement is `inner` — so a raw scan for comment markers is safe here: no
/// string literal can appear in those gaps to fool it.
fn block_has_comment_around(source: &str, block: &BlockStmt, inner: Span) -> bool {
    let before = &source[block.span.start + 1..inner.start];
    let after = &source[inner.end..block.span.end - 1];

    has_comment(before) || has_comment(after)
}

fn has_comment(s: &str) -> bool {
    s.contains("//") || s.contains("/*")
}

/// The text of a parenthesised condition, re-wrapped in parens only when its
/// top-level operator binds looser than `&&` (so `a || b` becomes `(a || b)`
/// before joining). The slice spans the inside of the original parens, so any
/// comment written inside the condition is preserved.
fn condition_text(cond: &Parens<Expr>, source: &str) -> String {
    let inner = source[cond.open + 1..cond.close - 1].trim();

    if needs_parens(&cond.inner) {
        format!("({inner})")
    } else {
        inner.to_string()
    }
}

/// Whether `expr` needs wrapping parens to keep its meaning when it becomes an
/// operand of `&&`. True for operators that bind looser than `&&` (`||` / `or`)
/// and for ternary / assignment, which would otherwise re-associate.
fn needs_parens(expr: &Expr) -> bool {
    match expr {
        Expr::Binary(b) => b.operator.precedence() < BinaryOperator::And.precedence(),
        Expr::Ternary(_) | Expr::Assign(_) => true,
        _ => false,
    }
}

#[cfg(test)]
mod tests {
    use super::{RULE_ELSE_IF, RULE_IF};
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn rule_diags(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
            .into_iter()
            .filter(|d| d.rule == RULE_IF || d.rule == RULE_ELSE_IF)
            .collect()
    }

    fn fixed(src: &str) -> String {
        let fixes = rule_diags(src).into_iter().filter_map(|d| d.fix).collect();
        apply_fixes(src, fixes)
    }

    #[test]
    fn collapses_nested_if() {
        let src = "function f() { if (x) { if (y) { g(); } } }";
        let diags = rule_diags(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, RULE_IF);
        assert_eq!(fixed(src), "function f() { if (x && y) { g(); } }");
    }

    #[test]
    fn fix_does_not_copy_the_body() {
        // The body stays put — no edit's replacement should contain it, so the
        // fix size is independent of how large the collapsed block is.
        let src = "function f() { if (x) { if (y) { big(); another(); more(); } } }";
        let fix = rule_diags(src)[0].fix.clone().expect("fix");
        for edit in &fix.edits {
            assert!(!edit.replacement.contains("big"));
        }
        assert_eq!(
            fixed(src),
            "function f() { if (x && y) { big(); another(); more(); } }"
        );
    }

    #[test]
    fn else_if_fix_does_not_copy_the_body() {
        let src = "function f() { if (x) { g(); } else { if (y) { big(); more(); } } }";
        let fix = rule_diags(src)[0].fix.clone().expect("fix");
        for edit in &fix.edits {
            assert!(!edit.replacement.contains("big"));
        }
        assert_eq!(
            fixed(src),
            "function f() { if (x) { g(); } else if (y) { big(); more(); } }"
        );
    }

    #[test]
    fn diagnostic_points_at_nesting_header() {
        let src = "function f() { if (x) { if (y) { g(); } } }";
        let d = &rule_diags(src)[0];
        assert_eq!(&src[d.span.start..d.span.end], "if (x) { if (y) {");
    }

    #[test]
    fn else_if_diagnostic_points_at_nesting_header() {
        let src = "function f() { if (x) { g(); } else { if (y) { h(); } } }";
        let d = &rule_diags(src)[0];
        assert_eq!(&src[d.span.start..d.span.end], "else { if (y) {");
    }

    #[test]
    fn parenthesizes_loose_conditions() {
        let src = "function f() { if (a || b) { if (c) { g(); } } }";
        assert_eq!(fixed(src), "function f() { if ((a || b) && c) { g(); } }");
    }

    #[test]
    fn parenthesizes_loose_inner_condition() {
        let src = "function f() { if (a) { if (b or c) { g(); } } }";
        assert_eq!(fixed(src), "function f() { if (a && (b or c)) { g(); } }");
    }

    #[test]
    fn keeps_tight_conditions_unwrapped() {
        let src = "function f() { if (a == b) { if (c != d) { g(); } } }";
        assert_eq!(
            fixed(src),
            "function f() { if (a == b && c != d) { g(); } }"
        );
    }

    #[test]
    fn ignores_outer_with_else() {
        let src = "function f() { if (x) { if (y) { g(); } } else { h(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn ignores_inner_with_else() {
        let src = "function f() { if (x) { if (y) { g(); } else { h(); } } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn ignores_block_with_extra_statements() {
        let src = "function f() { if (x) { if (y) { g(); } h(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn ignores_when_comment_before_nested_if() {
        let src = "function f() { if (x) { /* keep */ if (y) { g(); } } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn ignores_when_line_comment_after_nested_if() {
        let src = "function f() { if (x) { if (y) { g(); } // keep\n } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn collapses_else_if() {
        let src = "function f() { if (x) { g(); } else { if (y) { h(); } } }";
        let diags = rule_diags(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, RULE_ELSE_IF);
        assert_eq!(
            fixed(src),
            "function f() { if (x) { g(); } else if (y) { h(); } }"
        );
    }

    #[test]
    fn collapses_else_if_keeping_inner_else() {
        let src = "function f() { if (x) { g(); } else { if (y) { h(); } else { i(); } } }";
        assert_eq!(
            fixed(src),
            "function f() { if (x) { g(); } else if (y) { h(); } else { i(); } }"
        );
    }

    #[test]
    fn ignores_else_block_with_comment() {
        let src = "function f() { if (x) { g(); } else { /* keep */ if (y) { h(); } } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn ignores_else_block_with_extra_statements() {
        let src = "function f() { if (x) { g(); } else { if (y) { h(); } i(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn already_else_if_is_untouched() {
        let src = "function f() { if (x) { g(); } else if (y) { h(); } }";
        assert!(rule_diags(src).is_empty());
    }
}
