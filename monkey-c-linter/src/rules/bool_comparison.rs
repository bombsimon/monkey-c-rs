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

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "bool-comparison";

pub fn check_expr(expr: &Expr, ctx: &LintContext) -> Option<Diagnostic> {
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
    use super::RULE;
    use crate::test_support::{fix_rules, lint_rules};

    #[test]
    fn rewrites_comparisons_with_a_boolean() {
        for (expr, expected) in [
            ("ready == true", "ready"),
            ("ready != false", "ready"),
            ("ready == false", "!ready"),
            ("ready != true", "!ready"),
            ("true == ready", "ready"),
            ("isReady() == true", "isReady()"),
            ("isReady() == false", "!isReady()"),
            // `!` binds tighter than `<`, so the comparison needs parentheses.
            ("a < b == false", "!(a < b)"),
        ] {
            let src = format!("function f() {{ return {expr}; }}");
            let fixed = format!("function f() {{ return {expected}; }}");

            assert_eq!(fix_rules(&src, &[RULE]), fixed, "{expr}");
        }

        assert_eq!(
            fix_rules("function f() { if (ready == true) { go(); } }", &[RULE]),
            "function f() { if (ready) { go(); } }"
        );
    }

    #[test]
    fn ignores_other_comparisons() {
        for expr in ["true == false", "count > 0", "ready"] {
            let src = format!("function f() {{ return {expr}; }}");

            assert!(lint_rules(&src, &[RULE]).is_empty(), "{expr}");
        }
    }
}
