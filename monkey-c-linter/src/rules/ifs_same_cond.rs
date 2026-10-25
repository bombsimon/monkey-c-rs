//! `ifs-same-cond` - flag an `if`/`else if` chain where two arms test the same condition.
//!
//! ```monkeyc
//! if (a == b) {
//!     …
//! } else if (a == b) { // second arm can never run
//!     …
//! }
//! ```
//!
//! A repeated condition is almost always a copy-paste error: the later arm is unreachable because
//! the earlier one already matched.
//!
//! Conditions are compared structurally, so formatting differences don't hide a duplicate — `a ==
//! b` matches `a==b`. Conditions containing a call (`foo()`) or a `new` are ignored: a call may
//! have side effects, so two structurally-identical calls need not evaluate to the same value.
use monkey_c_parser::ast::{CallArg, ElseBranch, Expr, IfStmt, Type, TypeKind};

use crate::Diagnostic;
use crate::visit::LintContext;

pub const RULE: &str = "ifs-same-cond";

pub fn check_if(head: &IfStmt, _ctx: &LintContext) -> Vec<Diagnostic> {
    let mut seen: Vec<&Expr> = Vec::new();
    let mut diags = Vec::new();

    let mut current = head;
    loop {
        let condition = &current.condition.inner;

        // A call/`new` may have side effects, so a structural match does not imply the same value —
        // skip those conditions entirely.
        if !contains_call(condition) {
            if seen.iter().any(|earlier| expr_eq(earlier, condition)) {
                diags.push(Diagnostic {
                    rule: RULE,
                    message:
                        "this `if` has the same condition as an earlier arm in the chain - likely a copy-paste error"
                            .to_string(),
                    span: *condition.span(),
                    fix: None,
                });
            } else {
                seen.push(condition);
            }
        }

        match &current.else_branch {
            Some(ElseBranch::If(inner)) => current = inner,
            _ => break,
        }
    }

    diags
}

/// Structural equality of two expressions, ignoring source spans (and other position-only fields).
/// The derived `PartialEq` can't be used because it compares spans, which differ for two copies at
/// different offsets.
fn expr_eq(a: &Expr, b: &Expr) -> bool {
    match (a, b) {
        (Expr::Binary(x), Expr::Binary(y)) => {
            x.operator == y.operator && expr_eq(&x.left, &y.left) && expr_eq(&x.right, &y.right)
        }
        (Expr::Unary(x), Expr::Unary(y)) => {
            x.operator == y.operator && expr_eq(&x.operand, &y.operand)
        }
        (Expr::Ternary(x), Expr::Ternary(y)) => {
            expr_eq(&x.condition, &y.condition)
                && expr_eq(&x.then_expr, &y.then_expr)
                && expr_eq(&x.else_expr, &y.else_expr)
        }
        (Expr::Assign(x), Expr::Assign(y)) => {
            x.operator == y.operator && expr_eq(&x.target, &y.target) && expr_eq(&x.value, &y.value)
        }
        (Expr::Call(x), Expr::Call(y)) => {
            expr_eq(&x.callee, &y.callee) && args_eq(&x.args, &y.args)
        }
        (Expr::Member(x), Expr::Member(y)) => {
            x.property == y.property && expr_eq(&x.object, &y.object)
        }
        (Expr::Index(x), Expr::Index(y)) => {
            expr_eq(&x.object, &y.object) && expr_eq(&x.index, &y.index)
        }
        (Expr::New(x), Expr::New(y)) => x.class == y.class && args_eq(&x.args, &y.args),
        (Expr::NewArray(x), Expr::NewArray(y)) => {
            x.is_byte_array == y.is_byte_array
                && expr_eq(&x.size, &y.size)
                && match (&x.element_type, &y.element_type) {
                    (None, None) => true,
                    (Some(p), Some(q)) => type_eq(p, q),
                    _ => false,
                }
        }
        (Expr::TypeCast(x), Expr::TypeCast(y)) => {
            type_eq(&x.target_type, &y.target_type) && expr_eq(&x.expr, &y.expr)
        }
        (Expr::Array(x), Expr::Array(y)) => {
            x.is_byte_array == y.is_byte_array
                && x.entries.len() == y.entries.len()
                && x.entries
                    .iter()
                    .zip(&y.entries)
                    .all(|(p, q)| expr_eq(&p.value, &q.value))
        }
        (Expr::Dict(x), Expr::Dict(y)) => {
            x.entries.len() == y.entries.len()
                && x.entries
                    .iter()
                    .zip(&y.entries)
                    .all(|(p, q)| expr_eq(&p.key, &q.key) && expr_eq(&p.value, &q.value))
        }
        (Expr::Lit(x), Expr::Lit(y)) => x.value == y.value,
        (Expr::Ident(x), Expr::Ident(y)) => x.name == y.name,
        (Expr::Paren(x), Expr::Paren(y)) => expr_eq(&x.inner, &y.inner),
        (Expr::Me(_), Expr::Me(_))
        | (Expr::Self_(_), Expr::Self_(_))
        | (Expr::Bling(_), Expr::Bling(_)) => true,
        _ => false,
    }
}

fn args_eq(a: &[CallArg], b: &[CallArg]) -> bool {
    a.len() == b.len() && a.iter().zip(b).all(|(x, y)| expr_eq(&x.value, &y.value))
}

/// Structural equality of two type annotations, ignoring spans. Used for the `Type` carried by
/// `as`-casts and typed `new` allocations inside a condition.
fn type_eq(a: &Type, b: &Type) -> bool {
    a.optional == b.optional
        && type_kind_eq(&a.kind, &b.kind)
        && a.alternatives.len() == b.alternatives.len()
        && a.alternatives
            .iter()
            .zip(&b.alternatives)
            .all(|(p, q)| type_eq(&p.type_, &q.type_))
}

fn type_kind_eq(a: &TypeKind, b: &TypeKind) -> bool {
    match (a, b) {
        (
            TypeKind::Named {
                ident: ia,
                generic_params: ga,
            },
            TypeKind::Named {
                ident: ib,
                generic_params: gb,
            },
        ) => ia == ib && ga.len() == gb.len() && ga.iter().zip(gb).all(|(p, q)| type_eq(p, q)),
        (TypeKind::Tuple { elements: ea }, TypeKind::Tuple { elements: eb }) => {
            ea.len() == eb.len() && ea.iter().zip(eb).all(|(p, q)| type_eq(p, q))
        }
        (TypeKind::Group(ga), TypeKind::Group(gb)) => type_eq(&ga.inner, &gb.inner),
        // `Dict` / `Interface` / `Method` carry richer payloads (members, parameters) that are
        // vanishingly rare as cast targets inside a condition. Treat them as not-equal so the lint
        // never warns on a pair it can't cheaply prove identical.
        _ => false,
    }
}

/// Whether `expr` contains a function call or `new` anywhere in its subtree —
/// either of which can carry side effects.
fn contains_call(expr: &Expr) -> bool {
    match expr {
        Expr::Call(_) | Expr::New(_) => true,
        Expr::Unary(u) => contains_call(&u.operand),
        Expr::Binary(b) => contains_call(&b.left) || contains_call(&b.right),
        Expr::Ternary(t) => {
            contains_call(&t.condition)
                || contains_call(&t.then_expr)
                || contains_call(&t.else_expr)
        }
        Expr::Assign(a) => contains_call(&a.target) || contains_call(&a.value),
        Expr::Member(m) => contains_call(&m.object),
        Expr::Index(i) => contains_call(&i.object) || contains_call(&i.index),
        Expr::NewArray(n) => contains_call(&n.size),
        Expr::TypeCast(t) => contains_call(&t.expr),
        Expr::Array(a) => a.entries.iter().any(|e| contains_call(&e.value)),
        Expr::Dict(d) => d
            .entries
            .iter()
            .any(|e| contains_call(&e.key) || contains_call(&e.value)),
        Expr::Paren(p) => contains_call(&p.inner),
        Expr::Lit(_) | Expr::Ident(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => false,
    }
}

#[cfg(test)]
mod tests {
    use super::RULE;
    use crate::test_support::lint_rules;

    fn findings(chain: &str) -> usize {
        lint_rules(&format!("function f() {{ {chain} }}"), &[RULE]).len()
    }

    #[test]
    fn flags_a_repeated_condition_in_a_chain() {
        for chain in [
            "if (a == b) { x(); } else if (a == b) { y(); }",
            "if (a == b) { x(); } else if (a==b) { y(); }",
            "if (a) { x(); } else if (b) { y(); } else if (a) { z(); }",
            "if (x as Boolean) { a(); } else if (x as Boolean) { b(); }",
            "if (a) { if (b) { x(); } else if (b) { y(); } }",
        ] {
            assert_eq!(findings(chain), 1, "{chain}");
        }

        assert_eq!(
            findings("if (a) { x(); } else if (a) { y(); } else if (a) { z(); }"),
            2
        );
    }

    #[test]
    fn allows_distinct_or_impure_conditions() {
        for chain in [
            "if (a == b) { x(); } else if (a == c) { y(); }",
            "if (x as Boolean) { a(); } else if (x as Number) { b(); }",
            "if (a == b) { x(); }",
            // A call or `new` may give a different result the second time.
            "if (foo()) { x(); } else if (foo()) { y(); }",
            "if (a && foo()) { x(); } else if (a && foo()) { y(); }",
            "if (new Foo()) { x(); } else if (new Foo()) { y(); }",
            // Separate `if`s aren't a chain, since the first can fall through to the second.
            "if (a) { x(); } if (a) { y(); }",
        ] {
            assert_eq!(findings(chain), 0, "{chain}");
        }
    }
}
