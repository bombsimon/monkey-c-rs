//! `one-class-per-file` — flag files that declare more than one class.
//!
//! Each Monkey C file should hold a single class so the file's name maps
//! cleanly to its primary type, related code stays grouped per file, and
//! consumers don't have to guess which class lives where.
//!
//! Counting is done across the whole file — top-level classes and classes
//! nested in modules are all tallied. Anything after the first class is
//! flagged with a suggestion to move it to its own file. No auto-fix; the
//! linter can't decide where the extra classes should go.
use monkey_c_parser::ast::{Ast, ClassDecl, Span};

use crate::Diagnostic;
use crate::visit::LintContext;

pub const RULE: &str = "one-class-per-file";

pub fn check_document(nodes: &[Ast], _ctx: &LintContext) -> Vec<Diagnostic> {
    let mut classes: Vec<&ClassDecl> = Vec::new();
    for n in nodes {
        collect_classes(n, &mut classes);
    }

    if classes.len() <= 1 {
        return Vec::new();
    }

    classes
        .iter()
        .skip(1)
        .map(|c| Diagnostic {
            rule: RULE,
            message: format!(
                "more than one class declared in this file; move `{}` to its own file",
                c.name.node
            ),
            span: Span {
                start: c.span.start,
                end: c.brace_start,
            },
            fix: None,
        })
        .collect()
}

fn collect_classes<'a>(ast: &'a Ast, out: &mut Vec<&'a ClassDecl>) {
    match ast {
        Ast::Class(c) => out.push(c),
        Ast::Module(m) => {
            for n in &m.body {
                collect_classes(n, out);
            }
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::RULE;
    use crate::test_support::lint_rules;

    #[test]
    fn allows_a_single_class() {
        assert!(lint_rules("class Foo {}", &[RULE]).is_empty());
        assert!(lint_rules("module M { class Foo {} }", &[RULE]).is_empty());
    }

    #[test]
    fn flags_a_second_class_anywhere_in_the_file() {
        for src in [
            "class Foo {} class Bar {}",
            "module M { class Foo {} class Bar {} }",
            "class Foo {} module M { class Bar {} }",
            "module Outer { module InnerA { class Foo {} } module InnerB { class Bar {} } }",
        ] {
            let diags = lint_rules(src, &[RULE]);
            assert_eq!(diags.len(), 1, "{src}");
            assert!(diags[0].message.contains("Bar"), "{src}");
            assert!(diags[0].fix.is_none());
        }
    }

    #[test]
    fn flags_each_extra_class() {
        let diags = lint_rules("class Foo {} class Bar {} class Baz {}", &[RULE]);
        assert_eq!(diags.len(), 2);
        assert!(diags[0].message.contains("Bar"));
        assert!(diags[1].message.contains("Baz"));
    }
}
