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

    let extends = class.extends.as_deref()?;
    let parent_last = extends.rsplit('.').next().unwrap_or(extends);

    let init = class.body.iter().find_map(|n| match n {
        Ast::Function(f) if f.name == "initialize" => Some(f),
        _ => None,
    })?;

    if block_contains_super_init(&init.body, parent_last) {
        return None;
    }

    Some(Diagnostic {
        rule: RULE,
        message: format!(
            "`{}.initialize` should call `{}.initialize(...)` to chain to the superclass",
            class.name, parent_last
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
        Stmt::Expr(e) => is_super_init_call(e, parent),
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
    use crate::{Diagnostic, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    fn rule_diags(src: &str) -> Vec<Diagnostic> {
        lints(src)
            .into_iter()
            .filter(|d| d.rule == "super-initializer-call")
            .collect()
    }

    #[test]
    fn allows_class_without_extends() {
        let src = "class Foo { function initialize() {} }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_derived_class_without_initialize() {
        // Implicit no-op constructor chains automatically.
        let src = "class Foo extends Base { function bar() {} }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_derived_class_with_super_init_call() {
        let src = "class Foo extends Base { function initialize() { Base.initialize(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_super_init_with_args() {
        let src = "class Foo extends Base { function initialize(x) { Base.initialize(x); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_dotted_parent() {
        // extends WatchUi.View; call is View.initialize(...).
        let src = "class Foo extends WatchUi.View { function initialize() { View.initialize(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_fully_qualified_super_init_call() {
        let src = "class Foo extends WatchUi.View { function initialize() { WatchUi.View.initialize(); } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_super_init_inside_conditional() {
        let src = "class Foo extends Base { function initialize(x) { if (x != null) { Base.initialize(x); } else { Base.initialize(0); } } }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn flags_derived_initialize_without_super_call() {
        let src = "class Foo extends Base { function initialize() { var x = 1; } }";
        let diags = rule_diags(src);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Foo.initialize"));
        assert!(diags[0].message.contains("Base.initialize"));
    }

    #[test]
    fn flags_when_calling_wrong_parent() {
        let src = "class Foo extends Base { function initialize() { Other.initialize(); } }";
        assert_eq!(rule_diags(src).len(), 1);
    }

    #[test]
    fn flags_when_calling_same_named_function() {
        // `initialize();` (no receiver) doesn't count — it'd be self-recursion.
        let src = "class Foo extends Base { function initialize() { initialize(); } }";
        assert_eq!(rule_diags(src).len(), 1);
    }
}
