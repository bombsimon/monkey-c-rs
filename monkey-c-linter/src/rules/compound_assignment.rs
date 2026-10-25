//! `compound-assignment` — flag `<lvalue> = <lvalue> <op> <expr>` patterns
//! that can be written with a compound assignment operator or `++` / `--`.
//!
//! Examples:
//! - `x = x + 1;` → `x++;`
//! - `x = x - 1;` → `x--;`
//! - `x = x + n;` → `x += n;`
//! - `x = x * n;` → `x *= n;`
//! - `obj.x = obj.x + 1;` → `obj.x++;`
//! - `arr[i] = arr[i] + 1;` → `arr[i]++;`
//!
//! The target may be an identifier, a member access (`obj.x`), or an index
//! access (`arr[i]`), as long as the same shape appears on the left of the
//! binary RHS and every subexpression in the target is side-effect-free.
//! A target whose receiver or index contains a call, a `new`, an assignment,
//! or a `++` / `--` is skipped — rewriting `arr[next()] = arr[next()] + 1`
//! to `arr[next()] += 1` would call `next()` once instead of twice.
use monkey_c_parser::ast::{AssignOperator, BinaryOperator, Expr, LiteralValue, UnaryOperator};

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "compound-assignment";

pub fn check_expr(expr: &Expr, ctx: &LintContext) -> Option<Diagnostic> {
    let Expr::Assign(assign) = expr else {
        return None;
    };

    if assign.operator != AssignOperator::Assign {
        return None;
    }

    if !is_pure_lvalue(&assign.target) {
        return None;
    }

    let Expr::Binary(binary) = assign.value.as_ref() else {
        return None;
    };

    if !same_lvalue(&assign.target, &binary.left) {
        return None;
    }

    if !has_compound_form(binary.operator) {
        return None;
    }

    let op = binary.operator.as_str();
    let target_span = assign.target.span();
    let target_text = ctx.source[target_span.start..target_span.end].trim();

    let replacement = match (&binary.operator, is_literal_one(&binary.right)) {
        (BinaryOperator::Add, true) => format!("{target_text}++"),
        (BinaryOperator::Sub, true) => format!("{target_text}--"),
        _ => {
            let rhs_span = binary.right.span();
            let rhs_text = ctx.source[rhs_span.start..rhs_span.end].trim();

            format!("{target_text} {op}= {rhs_text}")
        }
    };

    let message =
        format!("`{target_text} = {target_text} {op} …` can be written as `{replacement}`");

    Some(Diagnostic {
        rule: RULE,
        message,
        span: assign.span,
        fix: Some(Fix::single(assign.span, replacement)),
    })
}

/// Is `expr` a writable location with no side effects in evaluating it?
/// Identifiers always are; member and index accesses are when their
/// subexpressions are pure.
fn is_pure_lvalue(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(_) => true,
        Expr::Member(m) => is_pure(&m.object),
        Expr::Index(i) => is_pure(&i.object) && is_pure(&i.index),
        Expr::Paren(p) => is_pure_lvalue(&p.inner),
        _ => false,
    }
}

/// Is `expr` free of observable side effects (no calls, no `new`, no
/// nested assignments, no `++` / `--`)? Used to gate whether the target
/// of a compound-assignment rewrite can safely be evaluated once instead
/// of twice.
fn is_pure(expr: &Expr) -> bool {
    match expr {
        Expr::Ident(_) | Expr::Lit(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => true,
        Expr::Member(m) => is_pure(&m.object),
        Expr::Index(i) => is_pure(&i.object) && is_pure(&i.index),
        Expr::Paren(p) => is_pure(&p.inner),
        Expr::Binary(b) => is_pure(&b.left) && is_pure(&b.right),
        Expr::Unary(u) => {
            !matches!(
                u.operator,
                UnaryOperator::PreInc
                    | UnaryOperator::PreDec
                    | UnaryOperator::PostInc
                    | UnaryOperator::PostDec,
            ) && is_pure(&u.operand)
        }
        Expr::Ternary(t) => is_pure(&t.condition) && is_pure(&t.then_expr) && is_pure(&t.else_expr),
        Expr::TypeCast(t) => is_pure(&t.expr),
        Expr::Call(_)
        | Expr::New(_)
        | Expr::NewArray(_)
        | Expr::Assign(_)
        | Expr::Array(_)
        | Expr::Dict(_) => false,
    }
}

/// Compare two pure lvalue trees for structural equality, ignoring spans.
/// `a` is the assignment target (already known to be a pure lvalue); `b`
/// is the left operand of the binary RHS.
fn same_lvalue(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Ident(x), Expr::Ident(y)) => x.name == y.name,
        (Expr::Member(x), Expr::Member(y)) => {
            x.property == y.property && same_lvalue(&x.object, &y.object)
        }
        (Expr::Index(x), Expr::Index(y)) => {
            same_lvalue(&x.object, &y.object) && same_pure_expr(&x.index, &y.index)
        }
        (Expr::Me(_), Expr::Me(_))
        | (Expr::Self_(_), Expr::Self_(_))
        | (Expr::Bling(_), Expr::Bling(_)) => true,
        (Expr::Paren(x), other) => same_lvalue(&x.inner, other),
        (other, Expr::Paren(y)) => same_lvalue(other, &y.inner),
        _ => false,
    }
}

/// Structural equality for arbitrary pure subtrees — used to compare index
/// expressions, which can be more than just lvalues (e.g. `arr[i + 1]`).
fn same_pure_expr(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Lit(x), Expr::Lit(y)) => x.value == y.value,
        (Expr::Binary(x), Expr::Binary(y)) => {
            x.operator == y.operator
                && same_pure_expr(&x.left, &y.left)
                && same_pure_expr(&x.right, &y.right)
        }
        (Expr::Unary(x), Expr::Unary(y)) => {
            x.operator == y.operator && same_pure_expr(&x.operand, &y.operand)
        }
        (Expr::Ternary(x), Expr::Ternary(y)) => {
            same_pure_expr(&x.condition, &y.condition)
                && same_pure_expr(&x.then_expr, &y.then_expr)
                && same_pure_expr(&x.else_expr, &y.else_expr)
        }
        (Expr::TypeCast(x), Expr::TypeCast(y)) => same_pure_expr(&x.expr, &y.expr),
        (Expr::Paren(x), other) => same_pure_expr(&x.inner, other),
        (other, Expr::Paren(y)) => same_pure_expr(other, &y.inner),
        _ => same_lvalue(a, b),
    }
}

fn is_literal_one(expr: &Expr) -> bool {
    matches!(expr, Expr::Lit(l) if matches!(&l.value, LiteralValue::Number(n) if n == "1"))
}

fn has_compound_form(operator: BinaryOperator) -> bool {
    matches!(
        operator,
        BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::BitAnd
            | BinaryOperator::BitOr
            | BinaryOperator::BitXor
            | BinaryOperator::LeftShift
            | BinaryOperator::RightShift
    )
}

#[cfg(test)]
mod tests {
    use super::RULE;
    use crate::test_support::{fix_rules, lint_rules};

    #[test]
    fn rewrites_self_assignments() {
        for (stmt, expected) in [
            ("x = x + 1;", "x++;"),
            ("x = x - 1;", "x--;"),
            ("x = x + 3;", "x += 3;"),
            ("x = x * n;", "x *= n;"),
            ("x = x / 2;", "x /= 2;"),
            ("x = x % 4;", "x %= 4;"),
            ("x = x & mask;", "x &= mask;"),
            ("x = x | flag;", "x |= flag;"),
            ("x = x ^ flag;", "x ^= flag;"),
            ("x = x << 2;", "x <<= 2;"),
            ("x = x >> 1;", "x >>= 1;"),
            ("obj.x = obj.x + 1;", "obj.x++;"),
            ("obj.a.b = obj.a.b * 2;", "obj.a.b *= 2;"),
            ("arr[i] = arr[i] + 1;", "arr[i]++;"),
            ("arr[0] = arr[0] + 3;", "arr[0] += 3;"),
            // Monkey C doesn't allow an assignment as a value, so `++` changes nothing here.
            ("foo(x = x + 1);", "foo(x++);"),
        ] {
            let src = format!("function f() {{ {stmt} }}");
            let fixed = format!("function f() {{ {expected} }}");

            assert_eq!(fix_rules(&src, &[RULE]), fixed, "{stmt}");
        }

        assert_eq!(
            fix_rules(
                "function f() { for (i = 0; i < 10; i = i + 1) {} }",
                &[RULE]
            ),
            "function f() { for (i = 0; i < 10; i++) {} }"
        );
    }

    #[test]
    fn ignores_other_assignments() {
        for stmt in [
            "x += 1;",
            "y = x + 1;",
            "obj.x = obj2.x + 1;",
            "obj.x = obj.y + 1;",
            // Rewriting would evaluate the call or `++` in the target once instead of twice.
            "arr[next()] = arr[next()] + 1;",
            "arr[i++] = arr[i++] + 1;",
            "getObj().x = getObj().x + 1;",
            // Only the left operand counts, keeping the rule purely syntactic.
            "x = 1 + x;",
            "b = b == 1;",
        ] {
            let src = format!("function f() {{ {stmt} }}");

            assert!(lint_rules(&src, &[RULE]).is_empty(), "{stmt}");
        }
    }
}
