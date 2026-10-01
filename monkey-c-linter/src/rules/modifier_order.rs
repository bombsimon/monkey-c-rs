//! `modifier-order` — flag `static` written before the visibility modifier.
//!
//! The compiler accepts both orders and the formatter keeps them as written, so a codebase easily
//! ends up mixing them. Visibility first matches Java and C# and is required in TypeScript. The
//! fix swaps the two keywords in place, but is skipped when a comment sits between them since it
//! can't tell which keyword the comment belongs to.
use monkey_c_parser::ast::{Ast, Modifiers, Span};

use crate::visit::LintContext;
use crate::{Diagnostic, Edit, Fix};

pub const RULE: &str = "modifier-order";

const STATIC_KEYWORD: &str = "static";

pub fn check_ast(ast: &Ast, ctx: &LintContext) -> Option<Diagnostic> {
    let modifiers = declaration_modifiers(ast)?;
    let static_start = modifiers.static_kw_start?;
    let visibility = modifiers.visibility.as_ref()?;

    if static_start > visibility.span.start {
        return None;
    }

    let static_span = Span {
        start: static_start,
        end: static_start + STATIC_KEYWORD.len(),
    };
    let visibility_text = &ctx.source[visibility.span.start..visibility.span.end];
    let between = &ctx.source[static_span.end..visibility.span.start];

    let fix = if between.contains("//") || between.contains("/*") {
        None
    } else {
        Some(Fix {
            edits: vec![
                Edit {
                    span: static_span,
                    replacement: visibility_text.to_string(),
                },
                Edit {
                    span: visibility.span,
                    replacement: STATIC_KEYWORD.to_string(),
                },
            ],
        })
    };

    Some(Diagnostic {
        rule: RULE,
        message: format!("`{visibility_text}` should come before `static`"),
        span: Span {
            start: static_span.start,
            end: visibility.span.end,
        },
        fix,
    })
}

fn declaration_modifiers(ast: &Ast) -> Option<&Modifiers> {
    match ast {
        Ast::Class(decl) => Some(&decl.modifiers),
        Ast::Const(decl) => Some(&decl.modifiers),
        Ast::Enum(decl) => Some(&decl.modifiers),
        Ast::Function(decl) => Some(&decl.modifiers),
        Ast::Module(decl) => Some(&decl.modifiers),
        Ast::Variable(decl) => Some(&decl.modifiers),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
            .into_iter()
            .filter(|diagnostic| diagnostic.rule == "modifier-order")
            .collect()
    }

    fn fixed(src: &str) -> String {
        let fixes = lints(src).into_iter().filter_map(|d| d.fix).collect();

        apply_fixes(src, fixes)
    }

    #[test]
    fn flags_static_before_visibility() {
        let src = "class C { static private const MAX = 10; }";
        let diags = lints(src);

        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].message, "`private` should come before `static`");
        assert_eq!(
            &src[diags[0].span.start..diags[0].span.end],
            "static private"
        );
    }

    #[test]
    fn fix_swaps_keywords() {
        let src = r#"
class C {
    static private const MAX = 10;
    static public function create() as Void {}
    static hidden var count = 0;
    static protected enum { A }
}
"#;
        let expected = r#"
class C {
    private static const MAX = 10;
    public static function create() as Void {}
    hidden static var count = 0;
    protected static enum { A }
}
"#;

        assert_eq!(fixed(src), expected);
    }

    #[test]
    fn fix_keeps_whitespace_between_keywords() {
        let src = "class C { static\n    private var x; }";

        assert_eq!(fixed(src), "class C { private\n    static var x; }");
    }

    #[test]
    fn flags_module_and_class_declarations() {
        let src = "static public module M { static private class C {} }";

        assert_eq!(
            fixed(src),
            "public static module M { private static class C {} }"
        );
    }

    #[test]
    fn ignores_visibility_before_static() {
        let src = "class C { private static const MAX = 10; public static function f() {} }";

        assert!(lints(src).is_empty());
    }

    #[test]
    fn ignores_single_modifier() {
        let src = "class C { static var a; private var b; }";

        assert!(lints(src).is_empty());
    }

    #[test]
    fn comment_between_keywords_skips_fix() {
        for src in [
            "class C { static /* note */ private var x; }",
            "class C { static // note\n private var x; }",
        ] {
            let diags = lints(src);

            assert_eq!(diags.len(), 1, "in `{src}`");
            assert!(diags[0].fix.is_none(), "in `{src}`");
        }
    }
}
