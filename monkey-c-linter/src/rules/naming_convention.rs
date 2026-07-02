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
//! - **Enum variants**: `SCREAMING_SNAKE_CASE`. Every variant in one
//!   enum must share a `<PREFIX>_` so call sites read as
//!   `Color.COLOR_RED` rather than `Color.RED`.
//!
//! Local variables inside function bodies are intentionally not checked
//! here yet — that walker exists as `Stmt::Var` and can be added later.
//!
//! [conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
use convert_case::{Case, Casing};
use monkey_c_parser::ast::{Ast, EnumDecl, FunctionDecl, Span, VarDecl, Visibility};

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
    let private = is_non_public(v.visibility.as_ref());
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
        header_span(f.span, f.args.open),
        "function",
        diags,
    );

    for arg in &f.args.inner {
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
    use crate::{Diagnostic, lint};
    use monkey_c_parser::parser::Parser;

    fn rule_diags(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
            .into_iter()
            .filter(|d| d.rule == "naming-convention")
            .collect()
    }

    #[test]
    fn allows_pascal_class() {
        assert!(rule_diags("class MyClass {}").is_empty());
    }

    #[test]
    fn flags_lowercase_class() {
        let diags = rule_diags("class myClass {}");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("class"));
        assert!(diags[0].message.contains("PascalCase"));
    }

    #[test]
    fn allows_pascal_module() {
        assert!(rule_diags("module MyModule {}").is_empty());
    }

    #[test]
    fn flags_lowercase_module() {
        assert_eq!(rule_diags("module myModule {}").len(), 1);
    }

    #[test]
    fn allows_camel_function() {
        assert!(rule_diags("function myFunction() {}").is_empty());
    }

    #[test]
    fn flags_pascal_function() {
        let diags = rule_diags("function MyFunction() {}");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("camelCase"));
    }

    #[test]
    fn flags_snake_function() {
        assert_eq!(rule_diags("function my_function() {}").len(), 1);
    }

    #[test]
    fn allows_camel_function_param() {
        assert!(rule_diags("function f(myArg as Number) {}").is_empty());
    }

    #[test]
    fn flags_pascal_function_param() {
        assert_eq!(rule_diags("function f(MyArg as Number) {}").len(), 1);
    }

    #[test]
    fn allows_camel_module_variable() {
        assert!(rule_diags("var myVar = 1;").is_empty());
    }

    #[test]
    fn flags_pascal_module_variable() {
        assert_eq!(rule_diags("var MyVar = 1;").len(), 1);
    }

    #[test]
    fn flags_underscore_module_variable() {
        // _-prefix is reserved for private class members.
        assert_eq!(rule_diags("var _myVar = 1;").len(), 1);
    }

    #[test]
    fn ignores_const_declarations() {
        // Constants conventionally use SCREAMING_SNAKE_CASE in Monkey C
        // SDK code; the rule leaves all `const` declarations alone.
        assert!(rule_diags("const myConst = 1;").is_empty());
        assert!(rule_diags("const COLOR_RED = 0xFF0000;").is_empty());
        assert!(rule_diags("class Foo { const SOME_CONST = 1; }").is_empty());
        assert!(rule_diags("class Foo { private const SOME_CONST = 1; }").is_empty());
    }

    #[test]
    fn allows_camel_var_inside_module() {
        assert!(rule_diags("module M { var myVar = 1; }").is_empty());
    }

    #[test]
    fn allows_public_class_field_camel() {
        assert!(rule_diags("class Foo { var myField; }").is_empty());
    }

    #[test]
    fn allows_explicit_public_class_field() {
        assert!(rule_diags("class Foo { public var myField; }").is_empty());
    }

    #[test]
    fn flags_public_class_field_pascal() {
        assert_eq!(rule_diags("class Foo { var MyField; }").len(), 1);
    }

    #[test]
    fn flags_public_class_field_with_underscore() {
        let diags = rule_diags("class Foo { public var _myField; }");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("camelCase"));
    }

    #[test]
    fn allows_private_class_field() {
        assert!(rule_diags("class Foo { private var _myField; }").is_empty());
    }

    #[test]
    fn allows_protected_class_field() {
        assert!(rule_diags("class Foo { protected var _myField; }").is_empty());
    }

    #[test]
    fn allows_hidden_class_field() {
        assert!(rule_diags("class Foo { hidden var _myField; }").is_empty());
    }

    #[test]
    fn flags_private_field_without_underscore() {
        let diags = rule_diags("class Foo { private var myField; }");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("_camelCase"));
    }

    #[test]
    fn flags_private_field_with_uppercase_after_underscore() {
        assert_eq!(rule_diags("class Foo { private var _MyField; }").len(), 1);
    }

    #[test]
    fn flags_hungarian_m_prefix_private_field() {
        // The Garmin convention is `_camelCase`; `mValue` is the older
        // Hungarian convention and is rejected.
        assert_eq!(rule_diags("class Foo { private var mValue; }").len(), 1);
    }

    #[test]
    fn allows_enum_with_consistent_prefix() {
        let src = "enum Color { COLOR_RED, COLOR_GREEN, COLOR_BLUE }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_anonymous_enum_with_consistent_prefix() {
        let src = "enum { FIELD_A, FIELD_B }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn allows_single_variant_enum() {
        // No prefix check makes sense with only one variant.
        let src = "enum { LONELY }";
        assert!(rule_diags(src).is_empty());
    }

    #[test]
    fn flags_enum_variant_not_screaming_snake() {
        let diags = rule_diags("enum { firstThing, SECOND_THING }");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("firstThing"));
        assert!(diags[0].message.contains("SCREAMING_SNAKE_CASE"));
    }

    #[test]
    fn flags_enum_variants_missing_underscore_prefix() {
        let diags = rule_diags("enum { FIRST, SECOND }");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("prefix"));
    }

    #[test]
    fn flags_enum_variants_with_different_prefixes() {
        let diags = rule_diags("enum { COLOR_RED, FIELD_BAR }");
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("COLOR_"));
    }
}
