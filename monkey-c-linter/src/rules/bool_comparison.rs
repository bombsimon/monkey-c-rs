//! `bool-comparison` — flag `==` / `!=` comparisons against a boolean literal
//! that can be written as the operand itself (or its negation).
//!
//! Examples:
//! - `x == true`  → `x`
//! - `x != false` → `x`
//! - `x == false` → `!x`
//! - `x != true`  → `!x`
//!
//! The literal may be on either side, so `true == ready()` is treated the same
//! as `ready() == true`. When the surviving operand is a binary or ternary
//! expression and the rewrite negates it, it is wrapped in parentheses so `!`
//! still binds the whole expression: `a < b == false` becomes `!(a < b)`.
use monkey_c_parser::ast::{BinaryOperator, Expr, LiteralValue};

use crate::visit::{ExprPosition, LintContext};
use crate::{Diagnostic, Fix};

pub const RULE: &str = "bool-comparison";

pub fn check_expr(expr: &Expr, _pos: ExprPosition, ctx: &LintContext) -> Option<Diagnostic> {
    let Expr::Binary(binary) = expr else {
        return None;
    };

    let equality = match binary.operator {
        BinaryOperator::Eq => true,
        BinaryOperator::NotEq => false,
        _ => return None,
    };

    // Exactly one side must be a boolean literal; `true == false` is a constant
    // expression this rule has no better form for.
    let (operand, literal) = match (bool_literal(&binary.left), bool_literal(&binary.right)) {
        (Some(_), Some(_)) | (None, None) => return None,
        (Some(value), None) => (binary.right.as_ref(), value),
        (None, Some(value)) => (binary.left.as_ref(), value),
    };

    // `== true` and `!= false` keep the operand; `== false` and `!= true`
    // negate it.
    let negate = equality != literal;

    let operand_span = operand.span();
    let operand_text = ctx.source[operand_span.start..operand_span.end].trim();

    let replacement = if !negate {
        operand_text.to_string()
    } else if needs_parens_under_not(operand) {
        format!("!({operand_text})")
    } else {
        format!("!{operand_text}")
    };

    let full = ctx.source[binary.span.start..binary.span.end].trim();

    Some(Diagnostic {
        rule: RULE,
        message: format!(
            "comparison to boolean literal — `{full}` can be written as `{replacement}`"
        ),
        span: binary.span,
        fix: Some(Fix::single(binary.span, replacement)),
    })
}

fn bool_literal(expr: &Expr) -> Option<bool> {
    match expr {
        Expr::Lit(lit) => match lit.value {
            LiteralValue::Boolean(value) => Some(value),
            _ => None,
        },
        _ => None,
    }
}

/// Unary `!` binds tighter than every binary operator and than `?:`, so an
/// operand that is itself one of those needs parentheses to keep its original
/// grouping once negated. Anything already atomic — an identifier, call, member
/// access, an existing parenthesised group — does not.
fn needs_parens_under_not(expr: &Expr) -> bool {
    matches!(
        expr,
        Expr::Binary(_) | Expr::Ternary(_) | Expr::Assign(_) | Expr::TypeCast(_)
    )
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    fn first_fix(src: &str) -> String {
        let fixes = lints(src).into_iter().filter_map(|d| d.fix).collect();

        apply_fixes(src, fixes)
    }

    #[test]
    fn drops_eq_true() {
        let src = "function f() { return ready == true; }";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "bool-comparison");
        assert_eq!(first_fix(src), "function f() { return ready; }");
    }

    #[test]
    fn drops_ne_false() {
        let src = "function f() { return ready != false; }";
        assert_eq!(first_fix(src), "function f() { return ready; }");
    }

    #[test]
    fn negates_eq_false() {
        let src = "function f() { return ready == false; }";
        assert_eq!(first_fix(src), "function f() { return !ready; }");
    }

    #[test]
    fn negates_ne_true() {
        let src = "function f() { return ready != true; }";
        assert_eq!(first_fix(src), "function f() { return !ready; }");
    }

    #[test]
    fn handles_literal_on_the_left() {
        let src = "function f() { return true == ready; }";
        assert_eq!(first_fix(src), "function f() { return ready; }");
    }

    #[test]
    fn keeps_a_call_operand() {
        let src = "function f() { return isReady() == true; }";
        assert_eq!(first_fix(src), "function f() { return isReady(); }");
    }

    #[test]
    fn parenthesises_a_negated_relational_operand() {
        // `!` binds tighter than `<`, so the operand needs parens to stay a
        // single negated comparison rather than `(!a) < b`.
        let src = "function f() { return a < b == false; }";
        assert_eq!(first_fix(src), "function f() { return !(a < b); }");
    }

    #[test]
    fn does_not_parenthesise_a_negated_call() {
        let src = "function f() { return isReady() == false; }";
        assert_eq!(first_fix(src), "function f() { return !isReady(); }");
    }

    #[test]
    fn fires_inside_a_condition() {
        let src = "function f() { if (ready == true) { go(); } }";
        assert_eq!(first_fix(src), "function f() { if (ready) { go(); } }");
    }

    #[test]
    fn ignores_both_operands_literal() {
        let src = "function f() { return true == false; }";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn ignores_non_equality_operators() {
        // Comparing against a boolean literal with `<` isn't idiomatic, but the
        // rule only rewrites `==` / `!=` and leaves everything else alone.
        let src = "function f() { return count > 0; }";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn ignores_plain_boolean_expression() {
        let src = "function f() { return ready; }";
        assert!(lints(src).is_empty());
    }
}
