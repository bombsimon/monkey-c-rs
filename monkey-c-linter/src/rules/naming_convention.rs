//! `naming-convention` — flag identifiers that don't follow Garmin's
//! Monkey C [coding conventions][conventions]:
//!
//! - **Modules / Classes**: `PascalCase` — first letter uppercase.
//! - **Functions / parameters**: `camelCase` — first letter lowercase.
//! - **Public class members**: `camelCase`.
//! - **Private / protected / hidden class members**: `_camelCase` —
//!   underscore prefix followed by `camelCase`.
//! - **Module-scope variables** (top-level or inside `module { … }`):
//!   `camelCase`.
//! - **Local variables** (`var` inside a function body or a `for`-loop
//!   init): `camelCase`.
//! - **Enum variants**: `SCREAMING_SNAKE_CASE`. Every variant in one
//!   enum must share a `<PREFIX>_` so call sites read as
//!   `Color.COLOR_RED` rather than `Color.RED`.
//!
//! [conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
use convert_case::{Case, Casing};
use monkey_c_parser::ast::{Ast, EnumDecl, FunctionDecl, Span, Stmt, VarDecl, Visibility};

use crate::Diagnostic;
use crate::visit::LintContext;

pub const RULE: &str = "naming-convention";

pub fn check_ast(ast: &Ast, _ctx: &LintContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    match ast {
        Ast::Class(c) => {
            check_pascal(
                &c.name.node,
                header_span(c.span, c.brace_start),
                "class",
                &mut diags,
            );

            for member in &c.body {
                check_class_member(member, &mut diags);
            }
        }
        Ast::Module(m) => {
            check_pascal(
                &m.name.node,
                header_span(m.span, m.brace_start),
                "module",
                &mut diags,
            );

            for member in &m.body {
                check_module_member(member, &mut diags);
            }
        }
        Ast::Document(nodes, _) => {
            for n in nodes {
                check_module_member(n, &mut diags);
            }
        }
        Ast::Function(f) => check_function(f, &mut diags),
        Ast::Enum(e) => check_enum(e, &mut diags),
        _ => {}
    }

    diags
}

/// Checks statement-level nodes. Only `var` declarations carry names to
/// check; every other statement kind is a no-op here.
pub fn check_stmt(stmt: &Stmt, _ctx: &LintContext) -> Vec<Diagnostic> {
    let mut diags = Vec::new();

    if let Stmt::Var(v) = stmt {
        check_var_decl(v, &mut diags);
    }

    diags
}

/// Checks a local `var` declaration's binding names, whether it's a plain
/// statement (`Stmt::Var`) or a `for`-loop init (`ForInit::Var`) — both
/// share the same [`VarDecl`] shape.
pub fn check_var_decl(v: &VarDecl, diags: &mut Vec<Diagnostic>) {
    for b in &v.bindings {
        check_camel(&b.name.node, b.span, "local variable", diags);
    }
}

fn check_class_member(member: &Ast, diags: &mut Vec<Diagnostic>) {
    if let Ast::Variable(v) = member {
        check_class_var(v, diags);
    }
}

fn check_module_member(member: &Ast, diags: &mut Vec<Diagnostic>) {
    if let Ast::Variable(v) = member {
        for b in &v.bindings {
            check_camel(&b.name.node, b.span, "module variable", diags);
        }
    }
}

fn check_class_var(v: &VarDecl, diags: &mut Vec<Diagnostic>) {
    let private = is_non_public(v.modifiers.visibility());
    let label = if private {
        "private class member"
    } else {
        "public class member"
    };

    for b in &v.bindings {
        if private {
            check_underscore_camel(&b.name.node, b.span, label, diags);
        } else {
            check_camel(&b.name.node, b.span, label, diags);
        }
    }
}

fn check_function(f: &FunctionDecl, diags: &mut Vec<Diagnostic>) {
    check_camel(
        &f.name.node,
        header_span(f.span, f.parameters.open),
        "function",
        diags,
    );

    for arg in &f.parameters.inner {
        check_camel(&arg.name.node, arg.span, "function parameter", diags);
    }
}

fn check_enum(e: &EnumDecl, diags: &mut Vec<Diagnostic>) {
    let mut all_screaming = true;
    for v in &e.variants {
        if !is_screaming_snake(&v.name) {
            let suggested = v.name.to_case(Case::UpperSnake);
            diags.push(Diagnostic {
                rule: RULE,
                message: format!(
                    "enum variant `{}` should be SCREAMING_SNAKE_CASE, e.g. `{suggested}`",
                    v.name
                ),
                span: v.span,
                fix: None,
            });
            all_screaming = false;
        }
    }

    // Common-prefix check only makes sense when there are at least two
    // variants and they each already pass the SCREAMING_SNAKE check — a
    // malformed variant would dominate any prefix comparison.
    if e.variants.len() < 2 || !all_screaming {
        return;
    }

    let Some(prefix_end) = e.variants[0].name.find('_') else {
        diags.push(Diagnostic {
            rule: RULE,
            message: "enum variants should share a common `<PREFIX>_` prefix".into(),
            span: e.span,
            fix: None,
        });

        return;
    };

    let prefix = &e.variants[0].name[..=prefix_end];
    if !e.variants.iter().all(|v| v.name.starts_with(prefix)) {
        diags.push(Diagnostic {
            rule: RULE,
            message: format!("enum variants should all share the same `{prefix}` prefix"),
            span: e.span,
            fix: None,
        });
    }
}

fn check_pascal(name: &str, span: Span, label: &str, diags: &mut Vec<Diagnostic>) {
    if is_pascal(name) {
        return;
    }

    let suggested = name.to_case(Case::Pascal);
    diags.push(Diagnostic {
        rule: RULE,
        message: format!("{label} `{name}` should be PascalCase, e.g. `{suggested}`"),
        span,
        fix: None,
    });
}

fn check_camel(name: &str, span: Span, label: &str, diags: &mut Vec<Diagnostic>) {
    if is_camel(name) {
        return;
    }

    let suggested = name.trim_start_matches('_').to_case(Case::Camel);
    diags.push(Diagnostic {
        rule: RULE,
        message: format!("{label} `{name}` should be camelCase, e.g. `{suggested}`"),
        span,
        fix: None,
    });
}

fn check_underscore_camel(name: &str, span: Span, label: &str, diags: &mut Vec<Diagnostic>) {
    if is_underscore_camel(name) {
        return;
    }

    let suggested = format!("_{}", name.trim_start_matches('_').to_case(Case::Camel));
    diags.push(Diagnostic {
        rule: RULE,
        message: format!("{label} `{name}` should be `_camelCase`, e.g. `{suggested}`"),
        span,
        fix: None,
    });
}

fn is_pascal(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_uppercase()) && chars.all(|c| c.is_ascii_alphanumeric())
}

fn is_camel(name: &str) -> bool {
    let mut chars = name.chars();
    chars.next().is_some_and(|c| c.is_ascii_lowercase()) && chars.all(|c| c.is_ascii_alphanumeric())
}

fn is_underscore_camel(name: &str) -> bool {
    name.starts_with('_') && is_camel(&name[1..])
}

fn is_screaming_snake(name: &str) -> bool {
    let mut chars = name.chars();
    let Some(first) = chars.next() else {
        return false;
    };

    first.is_ascii_uppercase()
        && chars.all(|c| c.is_ascii_uppercase() || c.is_ascii_digit() || c == '_')
}

fn is_non_public(vis: Option<&Visibility>) -> bool {
    matches!(
        vis,
        Some(Visibility::Private | Visibility::Protected | Visibility::Hidden)
    )
}

/// Span covering the declaration header (from the keyword to just before
/// the opening brace), used to anchor a diagnostic on the name itself
/// rather than the whole body.
fn header_span(span: Span, brace_start: usize) -> Span {
    Span {
        start: span.start,
        end: brace_start,
    }
}

#[cfg(test)]
mod tests {
    use super::RULE;
    use crate::Diagnostic;
    use crate::test_support::lint_rules;

    fn rule_diags(src: &str) -> Vec<Diagnostic> {
        lint_rules(src, &[RULE])
    }

    #[test]
    fn allows_conventional_names() {
        for src in [
            "class MyClass {}",
            "module MyModule { var myVar = 1; }",
            "function myFunction(myArg as Number) {}",
            "var myVar = 1;",
            "class Foo { var myField; public var otherField; }",
            "class Foo { private var _a; protected var _b; hidden var _c; }",
            "function f() { var myTotal = 0; for (var i = 0; i < 1; i += 1) {} }",
            "enum Color { COLOR_RED, COLOR_GREEN }",
            "enum { LONELY }",
            // Constants are left alone, since SDK code writes them in SCREAMING_SNAKE_CASE.
            "const myConst = 1; class Foo { private const SOME_CONST = 1; }",
        ] {
            assert!(rule_diags(src).is_empty(), "{src}");
        }
    }

    #[test]
    fn flags_unconventional_names() {
        for (src, expected) in [
            ("class myClass {}", "PascalCase"),
            ("module myModule {}", "PascalCase"),
            ("function MyFunction() {}", "camelCase"),
            ("function my_function() {}", "camelCase"),
            ("function f(MyArg as Number) {}", "camelCase"),
            ("var MyVar = 1;", "camelCase"),
            // The `_` prefix is for private class members.
            ("var _myVar = 1;", "camelCase"),
            ("class Foo { var MyField; }", "camelCase"),
            ("class Foo { public var _myField; }", "camelCase"),
            ("class Foo { private var myField; }", "_camelCase"),
            ("class Foo { private var _MyField; }", "_camelCase"),
            ("class Foo { private var mValue; }", "_camelCase"),
            ("function f() { var _myTotal = 0; }", "local variable"),
            (
                "function f() { if (true) { var MyTotal = 0; } }",
                "local variable",
            ),
            (
                "function f() { for (var Index = 0; Index < 1; Index += 1) {} }",
                "local variable",
            ),
            ("enum { firstThing, SECOND_THING }", "SCREAMING_SNAKE_CASE"),
            ("enum { FIRST, SECOND }", "prefix"),
            ("enum { COLOR_RED, FIELD_BAR }", "COLOR_"),
        ] {
            let diags = rule_diags(src);
            assert_eq!(diags.len(), 1, "{src}");
            assert!(
                diags[0].message.contains(expected),
                "{src}: {}",
                diags[0].message
            );
        }
    }

    #[test]
    fn flags_every_binding_of_a_declaration() {
        assert_eq!(
            rule_diags("function f() { var MyA = 1, MyB = 2; }").len(),
            2
        );
    }
}
