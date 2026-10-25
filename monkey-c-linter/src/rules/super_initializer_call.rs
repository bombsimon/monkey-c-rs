//! `super-initializer-call` — flag a derived class whose `initialize`
//! doesn't call the parent's `initialize`.
//!
//! Monkey C doesn't automatically chain to the superclass constructor when
//! a subclass overrides `initialize` — a derived `initialize` that omits
//! the `Parent.initialize(...)` call leaves the base in an uninitialised
//! state. Forgetting this is a common, hard-to-debug bug.
//!
//! The rule fires when:
//! 1. The class declares an `extends Base`.
//! 2. The class body defines its own `initialize` function.
//! 3. No call to `<Base>.initialize(...)` appears anywhere in that body.
//!
//! A class that extends a base but does *not* override `initialize` is
//! left alone — Monkey C's implicit no-op constructor still chains.
use monkey_c_parser::ast::{Ast, BlockStmt, ElseBranch, Expr, IfStmt, Stmt};

use crate::Diagnostic;
use crate::visit::LintContext;

pub const RULE: &str = "super-initializer-call";

pub fn check_ast(ast: &Ast, _ctx: &LintContext) -> Option<Diagnostic> {
    let Ast::Class(class) = ast else {
        return None;
    };

    let extends = class.extends.as_ref().map(|e| e.node.as_str())?;
    let parent_last = extends.rsplit('.').next().unwrap_or(extends);

    let init = class.body.iter().find_map(|n| match n {
        Ast::Function(f) if f.name.node == "initialize" => Some(f),
        _ => None,
    })?;

    let Some(body) = &init.body else {
        return None;
    };

    if block_contains_super_init(body, parent_last) {
        return None;
    }

    Some(Diagnostic {
        rule: RULE,
        message: format!(
            "`{}.initialize` should call `{}.initialize(...)` to chain to the superclass",
            class.name.node, parent_last
        ),
        span: init.span,
        fix: None,
    })
}

fn block_contains_super_init(body: &BlockStmt, parent: &str) -> bool {
    body.stmts
        .iter()
        .any(|s| stmt_contains_super_init(s, parent))
}

fn stmt_contains_super_init(stmt: &Stmt, parent: &str) -> bool {
    match stmt {
        Stmt::Expr(s) => is_super_init_call(&s.expr, parent),
        Stmt::Block(b) => block_contains_super_init(b, parent),
        Stmt::If(s) => if_contains_super_init(s, parent),
        _ => false,
    }
}

fn if_contains_super_init(s: &IfStmt, parent: &str) -> bool {
    if block_contains_super_init(&s.then_branch, parent) {
        return true;
    }

    match &s.else_branch {
        Some(ElseBranch::Block(b)) => block_contains_super_init(b, parent),
        Some(ElseBranch::If(inner)) => if_contains_super_init(inner, parent),
        None => false,
    }
}

fn is_super_init_call(expr: &Expr, parent: &str) -> bool {
    let Expr::Call(call) = expr else {
        return false;
    };

    let Expr::Member(m) = call.callee.as_ref() else {
        return false;
    };

    m.property == "initialize" && rightmost_name(&m.object) == Some(parent)
}

fn rightmost_name(expr: &Expr) -> Option<&str> {
    match expr {
        Expr::Ident(i) => Some(&i.name),
        Expr::Member(m) => Some(&m.property),
        Expr::Paren(p) => rightmost_name(&p.inner),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::RULE;
    use crate::test_support::lint_rules;

    #[test]
    fn allows_an_initialize_that_calls_the_parent() {
        for src in [
            "class Foo { function initialize() {} }",
            "class Foo extends Base { function bar() {} }",
            "class Foo extends Base { function initialize(x) { Base.initialize(x); } }",
            "class Foo extends WatchUi.View { function initialize() { View.initialize(); } }",
            "class Foo extends WatchUi.View { function initialize() { WatchUi.View.initialize(); } }",
            "class Foo extends Base { function initialize(x) { if (x) { Base.initialize(x); } } }",
        ] {
            assert!(lint_rules(src, &[RULE]).is_empty(), "{src}");
        }
    }

    #[test]
    fn flags_an_initialize_that_skips_the_parent() {
        let diags = lint_rules(
            "class Foo extends Base { function initialize() { var x = 1; } }",
            &[RULE],
        );
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Foo.initialize"));
        assert!(diags[0].message.contains("Base.initialize"));

        for call in ["Other.initialize();", "initialize();"] {
            let src = format!("class Foo extends Base {{ function initialize() {{ {call} }} }}");

            assert_eq!(lint_rules(&src, &[RULE]).len(), 1, "{call}");
        }
    }
}
