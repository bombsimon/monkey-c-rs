//! `annotation-comma` — flag a comma between the entries of an annotation, like `(:a, :b)`.
//!
//! Monkey C accepts both commas and whitespace between annotations, but the only documented form,
//! and the one Garmin uses, is space separated. The fix replaces the comma and the whitespace
//! after it with a single space, stopping at any comment so the comment is kept.
use monkey_c_parser::ast::{AnnotationEntry, Span};

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "annotation-comma";

pub fn check_annotation(entries: &[AnnotationEntry], ctx: &LintContext) -> Vec<Diagnostic> {
    entries
        .iter()
        .filter_map(|entry| {
            let comma_start = entry.preceding_comma_pos?;
            let after_comma = &ctx.source[comma_start + 1..entry.span.start];
            let span = Span {
                start: comma_start,
                end: entry.span.start - after_comma.trim_start().len(),
            };

            Some(Diagnostic {
                rule: RULE,
                message: "separate annotations with a space instead of a comma".to_string(),
                span,
                fix: Some(Fix::single(span, " ".to_string())),
            })
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn annotation_comma_lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src)
            .into_iter()
            .filter(|d| d.rule == super::RULE)
            .collect()
    }

    fn fixed(src: &str) -> String {
        let fixes = annotation_comma_lints(src)
            .into_iter()
            .filter_map(|d| d.fix)
            .collect();
        let fixed = apply_fixes(src, fixes);
        Parser::new(&fixed).parse().expect("fixed source parses");

        fixed
    }

    #[test]
    fn flags_comma() {
        let src = "(:first, :second)";
        let diags = annotation_comma_lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].message,
            "separate annotations with a space instead of a comma"
        );
        assert_eq!(fixed(src), "(:first :second)");
    }

    #[test]
    fn flags_comma_followed_by_many_spaces() {
        assert_eq!(fixed("(:first,     :second)"), "(:first :second)");
    }

    #[test]
    fn flags_comma_without_space() {
        assert_eq!(fixed("(:first,:second)"), "(:first :second)");
    }

    #[test]
    fn flags_comma_followed_by_newline() {
        assert_eq!(fixed("(:first,\n    :second)"), "(:first :second)");
    }

    #[test]
    fn flags_every_comma() {
        let src = "(:first, :second :third, :fourth)";
        assert_eq!(annotation_comma_lints(src).len(), 2);
        assert_eq!(fixed(src), "(:first :second :third :fourth)");
    }

    #[test]
    fn flags_comma_after_arguments() {
        let src = "(:first(true), :second,   :third)";
        assert_eq!(annotation_comma_lints(src).len(), 2);
        assert_eq!(fixed(src), "(:first(true) :second :third)");
    }

    #[test]
    fn keeps_commas_between_arguments() {
        let src = "(:first(true, 666), :second)";
        assert_eq!(annotation_comma_lints(src).len(), 1);
        assert_eq!(fixed(src), "(:first(true, 666) :second)");
    }

    #[test]
    fn keeps_block_comment_after_comma() {
        assert_eq!(
            fixed("(:first, /* second */ :second)"),
            "(:first /* second */ :second)"
        );
    }

    #[test]
    fn keeps_block_comment_hugging_comma() {
        assert_eq!(
            fixed("(:first,/* second */:second)"),
            "(:first /* second */:second)"
        );
    }

    #[test]
    fn keeps_line_comment_after_comma() {
        assert_eq!(
            fixed("(:first, // second\n :second)"),
            "(:first // second\n :second)"
        );
    }

    #[test]
    fn flags_comma_after_non_ascii_source() {
        // Spans are byte offsets, so multi-byte characters earlier in the file must not shift the
        // fix.
        assert_eq!(
            fixed("// åäö\n(:first,  :second)"),
            "// åäö\n(:first :second)"
        );
    }

    #[test]
    fn flags_annotation_in_class() {
        let src = "class Foo {\n    (:first, :second)\n    function bar() {}\n}";
        assert_eq!(
            fixed(src),
            "class Foo {\n    (:first :second)\n    function bar() {}\n}"
        );
    }

    #[test]
    fn ignores_space_separated_annotations() {
        assert!(annotation_comma_lints("(:first :second)").is_empty());
    }

    #[test]
    fn ignores_single_annotation() {
        assert!(annotation_comma_lints("(:only)").is_empty());
    }
}
