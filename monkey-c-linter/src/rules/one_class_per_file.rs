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
    use crate::{Diagnostic, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    #[test]
    fn allows_single_class_at_top_level() {
        let src = "class Foo {}";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn allows_single_class_in_module() {
        let src = "module M { class Foo {} }";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn flags_two_top_level_classes() {
        let src = "class Foo {} class Bar {}";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "one-class-per-file");
        assert!(diags[0].message.contains("Bar"));
    }

    #[test]
    fn flags_two_classes_inside_same_module() {
        let src = "module M { class Foo {} class Bar {} }";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Bar"));
    }

    #[test]
    fn flags_class_at_top_level_plus_class_in_module() {
        let src = "class Foo {} module M { class Bar {} }";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Bar"));
    }

    #[test]
    fn flags_classes_across_nested_modules() {
        let src = "module Outer { module InnerA { class Foo {} } module InnerB { class Bar {} } }";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert!(diags[0].message.contains("Bar"));
    }

    #[test]
    fn flags_each_extra_class() {
        let src = "class Foo {} class Bar {} class Baz {}";
        let diags: Vec<_> = lints(src)
            .into_iter()
            .filter(|d| d.rule == "one-class-per-file")
            .collect();
        assert_eq!(diags.len(), 2);
        assert!(diags[0].message.contains("Bar"));
        assert!(diags[1].message.contains("Baz"));
    }

    #[test]
    fn no_fix_offered() {
        let src = "class Foo {} class Bar {}";
        let diags = lints(src);
        assert!(diags[0].fix.is_none());
    }
}
