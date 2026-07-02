//! `redundant-resource-ref` — flag the legacy `@` prefix on resource
//! references (`@Rez.Strings.foo`).
//!
//! `@Rez.Strings.foo` and `Rez.Strings.foo` compile to the same thing; the
//! `@` is a vestigial marker from older Monkey C syntax.
use monkey_c_parser::ast::{Expr, UnaryOperator};

use crate::visit::{ExprPosition, LintContext};
use crate::{Diagnostic, Fix};

pub const RULE: &str = "redundant-resource-ref";

pub fn check_expr(expr: &Expr, _pos: ExprPosition, ctx: &LintContext) -> Option<Diagnostic> {
    let Expr::Unary(u) = expr else {
        return None;
    };

    if u.operator != UnaryOperator::ResourceRef {
        return None;
    }

    let operand_span = u.operand.span();
    let replacement = ctx.source[operand_span.start..operand_span.end].to_string();

    Some(Diagnostic {
        rule: RULE,
        message: "`@` prefix on resource reference is redundant".into(),
        span: u.span,
        fix: Some(Fix::single(u.span, replacement)),
    })
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    #[test]
    fn flags_resource_ref_and_drops_at_prefix() {
        let src = "function f() { dc.drawText(@Rez.Strings.AppName); }";
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "redundant-resource-ref");

        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        assert_eq!(
            apply_fixes(src, fixes),
            "function f() { dc.drawText(Rez.Strings.AppName); }"
        );
    }

    #[test]
    fn ignores_other_unary_operators() {
        let src = "function f() { return -x; }";
        assert!(lints(src).is_empty());
    }
}
