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
//! It's also unneeded when it sits directly inside a binary expression and
//! its own top-level operator binds at least as tightly as the enclosing
//! one — `((1 + 2) - 3) + 4` flattens to `1 + 2 - 3 + 4` because `+`/`-`
//! share a precedence level and are left-associative, so the left operand of
//! an equal-precedence operator never needs parens. The right operand does,
//! unless its operator binds strictly tighter.
//!
//! ## Types
//!
//! The Monkey C grammar only accepts parens around `Method(…)` types. The
//! rule flags `(Method(…))` groupings except when the parens are
//! load-bearing — `(Method(…) as Return)?`, where without the parens the
//! trailing `?` would bind to `Return` instead of to the whole callable.
use monkey_c_parser::ast::{BinaryExpr, BinaryOperator, Expr, ParenExpr, Span, Type, TypeKind};

use crate::visit::{ExprPosition, LintContext, OperandSide};
use crate::{Diagnostic, Fix};

pub const RULE: &str = "unneeded-parens";

pub fn check_expr(expr: &Expr, pos: ExprPosition, ctx: &LintContext) -> Option<Diagnostic> {
    let Expr::Paren(p) = expr else {
        return None;
    };

    match pos {
        ExprPosition::AssignValue | ExprPosition::Initializer | ExprPosition::ReturnValue => {
            paren_diagnostic(p, ctx)
        }
        ExprPosition::BinaryOperand { parent_op, side } => {
            let Expr::Binary(inner) = p.inner.as_ref() else {
                return None;
            };

            if !is_redundant_operand(inner.operator, parent_op, side)
                || drops_into_adjacent_operator(ctx.source, p.span)
            {
                return None;
            }

            paren_diagnostic(p, ctx)
        }
        _ => None,
    }
}

/// Whether parens around a binary operand with operator `inner_op`, sitting
/// on `side` of a `parent_op` expression, are redundant — either side is
/// fine if `inner_op` binds strictly tighter, and the left side is fine at
/// equal precedence since every level is left-associative.
fn is_redundant_operand(
    inner_op: BinaryOperator,
    parent_op: BinaryOperator,
    side: OperandSide,
) -> bool {
    let inner_prec = inner_op.precedence();
    let parent_prec = parent_op.precedence();

    inner_prec > parent_prec || (inner_prec == parent_prec && side == OperandSide::Left)
}

/// Build the "unneeded parentheses around expression" diagnostic for `p`,
/// replacing it with its (trimmed) inner source.
fn paren_diagnostic(p: &ParenExpr, ctx: &LintContext) -> Option<Diagnostic> {
    let span = p.span;

    Some(Diagnostic {
        rule: RULE,
        message: "unneeded parentheses around expression".into(),
        span,
        fix: Some(Fix::single(span, reduced_text(p, ctx))),
    })
}

/// Return the source text for `p`'s parens, with everything that's redundant
/// stripped: further `(…)` wrappers around `p.inner` (recursively — `(((1 +
/// 2)))` collapses to `1 + 2` in one fix, since at this point *some* layer of
/// parens was already found redundant and every nested layer is too), and,
/// once a non-paren expression is reached, any operand parens within it that
/// are redundant relative to its own operator (via [`reduce`]).
fn reduced_text(p: &ParenExpr, ctx: &LintContext) -> String {
    let between_start = p.span.start + 1;
    let between_end = p.span.end - 1;
    let inner = p.inner.as_ref();

    // Take the source around `inner` (exclusive of the parens) as-is. This
    // preserves any leading/trailing comments inside the parens — `(/* tag
    // */ 2 + 3)` keeps `/* tag */` rather than dropping it (which would
    // happen if we sliced `inner`'s span directly, since the comment sits
    // *before* its start).
    let leading = &ctx.source[between_start..inner.span().start];
    let trailing = &ctx.source[inner.span().end..between_end];

    let inner_text = match inner {
        Expr::Paren(inner_p) => reduced_text(inner_p, ctx),
        Expr::Binary(b) => reduce(b, ctx),
        _ => ctx.source[inner.span().start..inner.span().end].to_string(),
    };

    format!("{leading}{inner_text}{trailing}")
        .trim()
        .to_string()
}

/// Return the source text of `b`, with any operand parens that are redundant
/// relative to `b.operator` (recursively, all the way down) stripped out.
fn reduce(b: &BinaryExpr, ctx: &LintContext) -> String {
    let mut text = ctx.source[b.span.start..b.span.end].to_string();

    // Replace the right operand first so the left operand's byte offsets
    // (computed against the original span) are still valid afterwards.
    for (operand, side) in [(&b.right, OperandSide::Right), (&b.left, OperandSide::Left)] {
        let Expr::Paren(p) = operand.as_ref() else {
            continue;
        };
        let Expr::Binary(inner) = p.inner.as_ref() else {
            continue;
        };

        if !is_redundant_operand(inner.operator, b.operator, side)
            || drops_into_adjacent_operator(ctx.source, p.span)
        {
            continue;
        }

        let reduced = reduce(inner, ctx);
        let start = p.span.start - b.span.start;
        let end = p.span.end - b.span.start;
        text.replace_range(start..end, &reduced);
    }

    text
}

/// Whether removing the parens at `span` would glue one of its boundary
/// characters to its new neighbour and form a different token — e.g.
/// `x-(-a*b)` would become `x--a*b`, where `--` re-lexes as a decrement.
/// Only operator characters can combine this way, so it's enough to check
/// whether both characters on either side of each boundary are operator
/// characters.
fn drops_into_adjacent_operator(source: &str, span: Span) -> bool {
    const OPERATOR_CHARS: &str = "+-*/%=&|^<>!";

    let inner = source[span.start + 1..span.end - 1].trim();
    let mut inner_chars = inner.chars();
    let Some(inner_first) = inner_chars.next() else {
        return false;
    };
    // Single-character `inner` — `next_back` already consumed it via `next`.
    let inner_last = inner_chars.next_back().unwrap_or(inner_first);

    let before = source[..span.start].chars().next_back();
    let after = source[span.end..].chars().next();

    let left_boundary =
        before.is_some_and(|c| OPERATOR_CHARS.contains(c)) && OPERATOR_CHARS.contains(inner_first);
    let right_boundary =
        after.is_some_and(|c| OPERATOR_CHARS.contains(c)) && OPERATOR_CHARS.contains(inner_last);

    left_boundary || right_boundary
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
        fix: Some(Fix::single(span, replacement)),
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
        assert_eq!(fix.edits[0].replacement, "1 + 2");
    }

    #[test]
    fn flags_repeatedly_wrapped_initializer() {
        // `(((1+2)))` — every layer is redundant in initializer position,
        // so the single (outermost) diagnostic's fix collapses all of them.
        let src = "var x = (((1 + 2)));";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);

        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(fix.edits[0].replacement, "1 + 2");
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
    fn flags_same_precedence_left_operand_parens() {
        // `(a + b) - c` — `+`/`-` share a precedence level and are
        // left-associative, so the parens around the left operand never
        // change the parse.
        let src = "var x = (a + b) - c;";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(fix.edits[0].replacement, "a + b");
    }

    #[test]
    fn ignores_same_precedence_right_operand_parens() {
        // `a - (b + c)` — removing the parens would re-group as
        // `(a - b) + c`, which is a different value.
        let src = "var x = a - (b + c);";
        assert_eq!(lints(src).len(), 0);
    }

    #[test]
    fn flags_strictly_tighter_operand_parens_on_either_side() {
        // `a + (b * c)` — `*` already binds tighter than `+`, so the parens
        // are redundant regardless of which side they're on.
        let src = "var x = a + (b * c);";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        let fix = diags[0].fix.as_ref().expect("fix");
        assert_eq!(fix.edits[0].replacement, "b * c");
    }

    #[test]
    fn flags_nested_parens_recursively() {
        // `((1 + 2) - 3) + 4` — both the outer and inner parens are
        // redundant: `+`/`-` are same-precedence and left-associative all
        // the way down.
        let src = "function f() { if (((1 + 2) - 3) + 4) {} }";
        let diags = lints(src);
        assert_eq!(diags.len(), 2);

        // The outer paren's fix already has the inner one collapsed, so a
        // single `apply_fixes` pass fully flattens the chain. The (nested,
        // overlapping) fix for `(1 + 2)` is dropped in favour of it.
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        assert_eq!(
            apply_fixes(src, fixes),
            "function f() { if (1 + 2 - 3 + 4) {} }"
        );
    }

    #[test]
    fn ignores_parens_that_would_form_a_different_token() {
        // `a-(-b*c)` — removing the parens would glue `-` and `-` into `--`,
        // re-lexing as a decrement instead of two separate operators.
        let src = "var x = a-(-b*c);";
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
            fix.edits[0].replacement,
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
        assert_eq!(fix.edits[0].replacement, "1 + 2 /* sum */");
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
