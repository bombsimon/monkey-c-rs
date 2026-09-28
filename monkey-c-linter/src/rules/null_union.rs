//! `null-union` — flag a union of a single type with `Null` that can use the
//! `?` shorthand instead.
//!
//! Examples:
//! - `String or Null` → `String?`
//! - `Null or String` → `String?`
//! - `String | Null`  → `String?`
//!
//! Only two-member unions are flagged. `String or Number or Null` has no
//! shorthand since `?` applies to a single type, and wrapping the rest in
//! parentheses isn't something the grammar accepts for named types.
use monkey_c_parser::ast::{Type, TypeKind, UnionAlternative};

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "null-union";

pub fn check_type(ty: &Type, ctx: &LintContext) -> Option<Diagnostic> {
    let [alternative] = ty.alternatives.as_slice() else {
        return None;
    };

    let (non_null_text, non_null_is_optional) = match (is_null(ty), is_null(&alternative.type_)) {
        (false, true) => (primary_text(ty, alternative, ctx)?, ty.optional),
        (true, false) => (
            &ctx.source[alternative.type_.span.start..alternative.type_.span.end],
            alternative.type_.optional,
        ),
        (true, true) | (false, false) => return None,
    };

    // `String? or Null` is already nullable, so the `?` must not be doubled.
    let replacement = if non_null_is_optional {
        non_null_text.to_string()
    } else {
        format!("{non_null_text}?")
    };

    Some(Diagnostic {
        rule: RULE,
        message: format!("union with `Null` can be written as `{replacement}`"),
        span: ty.span,
        fix: Some(Fix::single(ty.span, replacement)),
    })
}

/// Whether `ty` is a plain `Null`, including the fully qualified forms.
fn is_null(ty: &Type) -> bool {
    let TypeKind::Named {
        ident,
        generic_params,
    } = &ty.kind
    else {
        return false;
    };

    generic_params.is_empty() && matches!(ident.as_str(), "Null" | "Lang.Null" | "Toybox.Lang.Null")
}

/// The source text of the first member of a union, without the separator
/// that joins it to `alternative`. The union's own span covers every member,
/// so the first member ends where the separator before `alternative` starts.
fn primary_text<'a>(
    ty: &Type,
    alternative: &UnionAlternative,
    ctx: &LintContext<'a>,
) -> Option<&'a str> {
    let text = ctx.source[ty.span.start..alternative.type_.span.start].trim_end();
    let text = text.strip_suffix(alternative.separator.as_str())?;

    Some(text.trim_end())
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    fn null_union_lints(src: &str) -> Vec<Diagnostic> {
        lints(src)
            .into_iter()
            .filter(|d| d.rule == "null-union")
            .collect()
    }

    fn fixed(src: &str) -> String {
        let fixes = null_union_lints(src)
            .into_iter()
            .filter_map(|d| d.fix)
            .collect();

        apply_fixes(src, fixes)
    }

    #[test]
    fn flags_function_parameter() {
        let src = "function f(n as String or Null) {}";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "null-union");
        assert_eq!(
            diags[0].message,
            "union with `Null` can be written as `String?`"
        );
        assert_eq!(fixed(src), "function f(n as String?) {}");
    }

    #[test]
    fn flags_every_function_parameter() {
        let src = "function f(n as String or Null, y as Number or Null) {}";
        assert_eq!(null_union_lints(src).len(), 2);
        assert_eq!(fixed(src), "function f(n as String?, y as Number?) {}");
    }

    #[test]
    fn flags_function_return_type() {
        let src = "function f() as Number or Null {}";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f() as Number? {}");
    }

    #[test]
    fn flags_var_declaration() {
        let src = "var foo as Number or Null = 2;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Number? = 2;");
    }

    #[test]
    fn flags_local_var_declaration() {
        let src = "function f() { var foo as Number or Null = 2; }";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f() { var foo as Number? = 2; }");
    }

    #[test]
    fn flags_null_written_first() {
        let src = "var foo as Null or Number = 2;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Number? = 2;");
    }

    #[test]
    fn flags_pipe_separator() {
        let src = "var foo as Number | Null = 2;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Number? = 2;");
    }

    #[test]
    fn flags_qualified_null() {
        let src = "var foo as Number or Toybox.Lang.Null = 2;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Number? = 2;");
    }

    #[test]
    fn flags_already_optional_type_without_doubling_question_mark() {
        let src = "var a as Number? or Null = 2;\nvar b as Null or Number? = 2;";
        assert_eq!(null_union_lints(src).len(), 2);
        assert_eq!(fixed(src), "var a as Number? = 2;\nvar b as Number? = 2;");
    }

    #[test]
    fn flags_generic_type_and_keeps_its_parameters() {
        let src = "var foo as Dictionary<String, Number> or Null = {};";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Dictionary<String, Number>? = {};");
    }

    #[test]
    fn flags_generic_parameter() {
        let src = "var foo as Array<Number or Null> = [];";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var foo as Array<Number?> = [];");
    }

    #[test]
    fn flags_outer_and_generic_parameter_together() {
        let src = "var foo as Array<Number or Null> or Null = [];";
        assert_eq!(null_union_lints(src).len(), 2);
    }

    #[test]
    fn flags_dict_value() {
        let src = "function f(opts as { :flag as Boolean or Null }) {}";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f(opts as { :flag as Boolean? }) {}");
    }

    #[test]
    fn flags_dict_type() {
        let src = "function f(opts as { :flag as Boolean } or Null) {}";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f(opts as { :flag as Boolean }?) {}");
    }

    #[test]
    fn flags_tuple_type() {
        let src = "function f() as [Number, String] or Null {}";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f() as [Number, String]? {}");
    }

    #[test]
    fn flags_tuple_element() {
        let src = "function f() as [Number, String or Null] {}";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f() as [Number, String?] {}");
    }

    #[test]
    fn flags_method_type_without_return() {
        let src = "var callback as Method(x as Number) or Null;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var callback as Method(x as Number)?;");
    }

    #[test]
    fn flags_method_parameter() {
        let src = "var callback as Method(x as Number or Null);";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var callback as Method(x as Number?);");
    }

    #[test]
    fn flags_method_return_type_that_owns_the_null() {
        // Without parens `or Null` belongs to the return type, and the fix
        // keeps it there.
        let src = "var callback as Method() as Boolean or Null;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "var callback as Method() as Boolean?;");
    }

    #[test]
    fn flags_grouped_method_type() {
        let src = "var callback as (Method() as Boolean) or Null;";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "null-union");
        assert_eq!(fixed(src), "var callback as (Method() as Boolean)?;");
    }

    #[test]
    fn flags_interface_members() {
        let src = r#"
typedef Td as interface {
    var name as String or Null;

    function f(n as Number or Null) as String or Null;
};
"#;
        assert_eq!(null_union_lints(src).len(), 3);
        assert_eq!(
            fixed(src),
            r#"
typedef Td as interface {
    var name as String?;

    function f(n as Number?) as String?;
};
"#
        );
    }

    #[test]
    fn flags_typedef() {
        let src = "typedef MaybeNumber as Number or Null;";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "typedef MaybeNumber as Number?;");
    }

    #[test]
    fn flags_cast_target() {
        let src = "function f() { var x = y as Number or Null; }";
        assert_eq!(null_union_lints(src).len(), 1);
        assert_eq!(fixed(src), "function f() { var x = y as Number?; }");
    }

    #[test]
    fn flags_cast_target_inside_ternary() {
        let src = "function f() { var x = c ? y as Number or Null : z; }";
        assert_eq!(null_union_lints(src).len(), 1);

        let fixed = fixed(src);
        assert_eq!(fixed, "function f() { var x = c ? y as Number? : z; }");
        Parser::new(&fixed).parse().expect("fixed source parses");
    }

    #[test]
    fn ignores_nullable_shorthand() {
        assert!(lints("function f(n as String?) as Number? {}").is_empty());
    }

    #[test]
    fn ignores_non_null_union() {
        assert!(lints("function f(n as String or Number) {}").is_empty());
    }

    #[test]
    fn ignores_union_with_more_than_two_members() {
        assert!(lints("function f(n as String or Number or Null) {}").is_empty());
        assert!(lints("function f(n as String or Null or Number) {}").is_empty());
        assert!(lints("function f(n as Null or String or Number) {}").is_empty());
    }

    #[test]
    fn ignores_plain_null() {
        assert!(lints("function f() as Null {}").is_empty());
    }

    #[test]
    fn ignores_null_or_null() {
        assert!(lints("var foo as Null or Null;").is_empty());
    }

    #[test]
    fn ignores_type_merely_named_like_null() {
        assert!(lints("var foo as Number or Nullable;").is_empty());
        assert!(lints("var foo as Number or MyModule.Null;").is_empty());
    }

    #[test]
    fn ignores_untyped_declarations() {
        assert!(lints("function f(n) { var x = null; }").is_empty());
    }
}
