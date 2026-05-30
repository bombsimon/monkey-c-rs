//! `unneeded-parens` — flag `(<expr>)` and `(<Method(…)>)` in positions where
//! the parentheses don't change parsing.
//!
//! ## Expressions
//!
//! A `(<expr>)` is unneeded when its enclosing position guarantees that any
//! expression is unambiguously parseable: the RHS of an assignment, the
//! initializer of a `var`/`const`, the value of a `return`. In those slots,
//! removing the parens cannot affect operator precedence.
//!
//! ## Types
//!
//! The Monkey C grammar only accepts parens around `Method(…)` types. The
//! rule flags `(Method(…))` groupings except when the parens are
//! load-bearing — `(Method(…) as Return)?`, where without the parens the
//! trailing `?` would bind to `Return` instead of to the whole callable.
use monkey_c_parser::ast::{Expr, Span, Type, TypeKind};

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

pub fn check_type(ty: &Type, ctx: &LintContext) -> Option<Diagnostic> {
    let TypeKind::Group(group) = &ty.kind else {
        return None;
    };

    // The grammar only accepts parens around `Method(…)` types — every
    // other `(T)` shape is already a compile error and there's no useful
    // fix to suggest.
    let TypeKind::Method { returns, .. } = &group.inner.kind else {
        return None;
    };

    // Load-bearing case: a nullable Method type *with* a return. Without
    // the parens, `Method(x) as Void?` would put `?` on `Void` rather than
    // on the whole callable.
    if ty.optional && returns.is_some() {
        return None;
    }

    let span = Span {
        start: group.open,
        end: group.close,
    };
    let between = &ctx.source[group.open + 1..group.close - 1];
    let replacement = between.trim().to_string();

    Some(Diagnostic {
        rule: RULE,
        message: "unneeded parentheses around type".into(),
        span,
        fix: Some(Fix { span, replacement }),
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

    fn first_type_fix(src: &str) -> String {
        let diags = lints(src);
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();

        apply_fixes(src, fixes)
    }

    #[test]
    fn flags_parens_around_method_without_return() {
        // No return → nothing for an inner `?` to attach to, so the outer
        // parens add nothing.
        let src = "var cb as (Method(x as Number))?;";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "unneeded-parens");
        assert_eq!(diags[0].message, "unneeded parentheses around type");
        assert_eq!(first_type_fix(src), "var cb as Method(x as Number)?;");
    }

    #[test]
    fn flags_parens_around_method_with_return_but_no_optional() {
        // `(Method(…) as Return)` without a trailing `?` parses the same as
        // `Method(…) as Return` — both forms compile.
        let src = "function f() as (Method() as Boolean) { return method(:y); }";
        assert_eq!(
            first_type_fix(src),
            "function f() as Method() as Boolean { return method(:y); }"
        );
    }

    #[test]
    fn ignores_parens_around_optional_method_with_return() {
        // The one load-bearing case — `Method(x) as Void?` would put `?` on
        // `Void` rather than on the whole callable.
        let src = "var cb as (Method(value as String) as Void)?;";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn ignores_parens_around_non_method_types() {
        // The Monkey C grammar only accepts parens around `Method(…)` types.
        // Even though our parser is permissive enough to accept other shapes,
        // they're compile errors upstream and there's no useful fix to make
        // here.
        let src = "var x as (Number);";
        assert!(lints(src).is_empty());
    }
}
