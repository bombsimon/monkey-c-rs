//! `unneeded-parens` — flag `(<expr>)` in positions where parentheses don't
//! change parsing.
//!
//! A `(<expr>)` is unneeded when its enclosing position guarantees that any
//! expression is unambiguously parseable: the RHS of an assignment, the
//! initializer of a `var`/`const`, the value of a `return`. In those slots,
//! removing the parens cannot affect operator precedence.
use monkey_c_parser::ast::Expr;

use crate::visit::{ExprPosition, LintContext};
use crate::{Diagnostic, Fix};

pub const RULE: &str = "unneeded-parens";

pub fn check_expr(expr: &Expr, pos: ExprPosition, ctx: &LintContext) -> Option<Diagnostic> {
    let Expr::Paren(p) = expr else {
        return None;
    };

    if !matches!(
        pos,
        ExprPosition::AssignValue | ExprPosition::Initializer | ExprPosition::ReturnValue
    ) {
        return None;
    }

    // Take the source between the parens (exclusive) and trim the
    // surrounding whitespace. This preserves any leading/trailing comments
    // inside the parens — `(/* tag */ 2 + 3)` keeps `/* tag */` rather than
    // dropping it (which would happen if we sliced the inner expression's
    // span directly, since the comment sits *before* the inner expr's start).
    let between = &ctx.source[p.span.start + 1..p.span.end - 1];
    let replacement = between.trim().to_string();

    Some(Diagnostic {
        rule: RULE,
        message: "unneeded parentheses around expression".into(),
        span: p.span,
        fix: Some(Fix {
            span: p.span,
            replacement,
        }),
    })
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    #[test]
    fn flags_parens_around_var_initializer() {
        let src = "var x = (1 + 2);";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "unneeded-parens");
        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(fix.replacement, "1 + 2");
    }

    #[test]
    fn flags_parens_around_const_initializer() {
        let src = "const X = (42);";
        assert_eq!(lints(src).len(), 1);
    }

    #[test]
    fn flags_parens_around_return_value() {
        let src = "function f() { return (x); }";
        assert_eq!(lints(src).len(), 1);
    }

    #[test]
    fn flags_parens_around_assignment_rhs() {
        let src = "function f() { x = (1 + 2); }";
        assert_eq!(lints(src).len(), 1);
    }

    #[test]
    fn ignores_parens_inside_binary() {
        // `(1 + 2) * 3` — the parens *are* needed here; without them the
        // precedence flips. Don't flag.
        let src = "var x = (1 + 2) * 3;";
        assert_eq!(lints(src).len(), 0);
    }

    #[test]
    fn ignores_required_parens_in_call_object() {
        // `(x + 1).foo` — parens are needed to bind `.foo` to the whole sum.
        let src = "function f() { return (x + 1).foo; }";
        assert_eq!(lints(src).len(), 0);
    }

    #[test]
    fn fix_replaces_parens_with_inner_text() {
        let src = "var x = (1 + 2);";
        let diags = lints(src);
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        assert_eq!(apply_fixes(src, fixes), "var x = 1 + 2;");
    }

    #[test]
    fn fix_preserves_inner_block_comment() {
        let src = "var lineWidthPlusMargin = (/* BATTERY_LINE_WIDTH */ 2 + BATTERY_MARGIN);";
        let diags = lints(src);
        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(
            fix.replacement,
            "/* BATTERY_LINE_WIDTH */ 2 + BATTERY_MARGIN"
        );
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        assert_eq!(
            apply_fixes(src, fixes),
            "var lineWidthPlusMargin = /* BATTERY_LINE_WIDTH */ 2 + BATTERY_MARGIN;"
        );
    }

    #[test]
    fn fix_preserves_trailing_inner_comment() {
        let src = "var x = (1 + 2 /* sum */);";
        let diags = lints(src);
        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(fix.replacement, "1 + 2 /* sum */");
    }

    #[test]
    fn fix_preserves_surrounding_whitespace_and_punctuation() {
        let src = "function f() {\n    return  (foo());\n}";
        let diags = lints(src);
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        assert_eq!(
            apply_fixes(src, fixes),
            r#"
function f() {
    return  foo();
}"#
            .trim()
        );
    }
}
