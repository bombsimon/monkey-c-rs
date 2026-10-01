//! `pipe-union` — flag a union type that separates its members with `|`
//! instead of `or`.
//!
//! Examples:
//! - `Number | Float`          → `Number or Float`
//! - `String or Number | Null` → `String or Number or Null`
//!
//! `or` is what Garmin's documentation uses and what nearly all real Monkey C
//! code writes, and it can't be mistaken for a bitwise or logical OR.
use monkey_c_parser::ast::{Type, UnionSeparator};

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "pipe-union";

pub fn check_type(ty: &Type, ctx: &LintContext) -> Vec<Diagnostic> {
    ty.alternatives
        .iter()
        .filter(|alternative| alternative.separator.node == UnionSeparator::Pipe)
        .map(|alternative| {
            let span = alternative.separator.span;

            // `Number|Float` has nothing around the `|`, so `or` needs its own spacing to stay a
            // separate word.
            let spaced_before = ctx.source[..span.start].ends_with(char::is_whitespace);
            let spaced_after = ctx.source[span.end..].starts_with(char::is_whitespace);
            let replacement = format!(
                "{}or{}",
                if spaced_before { "" } else { " " },
                if spaced_after { "" } else { " " },
            );

            Diagnostic {
                rule: RULE,
                message: "use `or` instead of `|` to separate union types".to_string(),
                span,
                fix: Some(Fix::single(span, replacement)),
            }
        })
        .collect()
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn pipe_union_lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src)
            .into_iter()
            .filter(|d| d.rule == "pipe-union")
            .collect()
    }

    fn fixed(src: &str) -> String {
        let fixes = pipe_union_lints(src)
            .into_iter()
            .filter_map(|d| d.fix)
            .collect();
        let fixed = apply_fixes(src, fixes);
        Parser::new(&fixed).parse().expect("fixed source parses");

        fixed
    }

    #[test]
    fn flags_typedef() {
        let src = "typedef Numeric as Number | Float;";
        let diags = pipe_union_lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(
            diags[0].message,
            "use `or` instead of `|` to separate union types"
        );
        assert_eq!(fixed(src), "typedef Numeric as Number or Float;");
    }

    #[test]
    fn flags_every_pipe() {
        let src = "typedef Numeric as Number | Float | Long;";
        assert_eq!(pipe_union_lints(src).len(), 2);
        assert_eq!(fixed(src), "typedef Numeric as Number or Float or Long;");
    }

    #[test]
    fn flags_union_mixing_separators() {
        let src = "function f(value as String or Number | Float) as Void {}";
        assert_eq!(pipe_union_lints(src).len(), 1);
        assert_eq!(
            fixed(src),
            "function f(value as String or Number or Float) as Void {}"
        );
    }

    #[test]
    fn flags_function_return_type() {
        let src = "function f() as Number | String {}";
        assert_eq!(fixed(src), "function f() as Number or String {}");
    }

    #[test]
    fn flags_var_declaration() {
        let src = "function f() { var x as Number | String = 1; }";
        assert_eq!(
            fixed(src),
            "function f() { var x as Number or String = 1; }"
        );
    }

    #[test]
    fn flags_generic_parameter_and_outer_union() {
        let src = "var x as Array<Number | String> | Boolean;";
        assert_eq!(pipe_union_lints(src).len(), 2);
        assert_eq!(fixed(src), "var x as Array<Number or String> or Boolean;");
    }

    #[test]
    fn flags_dict_tuple_and_method_types() {
        let src =
            "var x as { :a as Number | Float } | [Number | String] | Method(b as Long | Float);";
        assert_eq!(pipe_union_lints(src).len(), 5);
        assert_eq!(
            fixed(src),
            "var x as { :a as Number or Float } or [Number or String] or Method(b as Long or Float);"
        );
    }

    #[test]
    fn flags_cast_target() {
        let src = "function f() { var x = y as Number | String; }";
        assert_eq!(
            fixed(src),
            "function f() { var x = y as Number or String; }"
        );
    }

    #[test]
    fn adds_spacing_around_tight_pipe() {
        let src = "typedef Numeric as Number|Float;";
        assert_eq!(fixed(src), "typedef Numeric as Number or Float;");
    }

    #[test]
    fn keeps_comments_around_pipe() {
        let src = "typedef Numeric as Number /* a */ | /* b */ Float;";
        assert_eq!(
            fixed(src),
            "typedef Numeric as Number /* a */ or /* b */ Float;"
        );

        let src = "typedef Numeric as Number/* a */|/* b */Float;";
        assert_eq!(
            fixed(src),
            "typedef Numeric as Number/* a */ or /* b */Float;"
        );

        let src = "typedef Numeric as Number // a\n    | Float;";
        assert_eq!(fixed(src), "typedef Numeric as Number // a\n    or Float;");
    }

    #[test]
    fn ignores_or_union() {
        assert!(pipe_union_lints("typedef Numeric as Number or Float;").is_empty());
    }

    #[test]
    fn ignores_bitwise_or_after_cast() {
        assert!(pipe_union_lints("function f() { var x = 0x00 as Number | 0xFF; }").is_empty());
        assert!(pipe_union_lints("function f() { var x = 0x00 as Number | (a | b); }").is_empty());
    }

    #[test]
    fn ignores_bitwise_or_expression() {
        let src = "function f() { var x = Graphics.TEXT_JUSTIFY_CENTER | Graphics.TEXT_JUSTIFY_VCENTER; }";
        assert!(pipe_union_lints(src).is_empty());
    }
}
