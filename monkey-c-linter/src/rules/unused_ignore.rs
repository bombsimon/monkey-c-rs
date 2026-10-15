//! `unused-ignore` — flag a rule in a `rafiki: ignore` comment that has nothing to silence.
//!
//! A stale suppression hides the next real finding of that rule, so it's worth removing as soon
//! as the code it was written for changes. Detection lives in `suppression`, which knows what each
//! comment covers.
use monkey_c_parser::ast::Span;

use crate::{Diagnostic, Fix};

pub const RULE: &str = "unused-ignore";

/// A finding for `rule`, listed at `span`. `fix` is shared by every unused rule in the same
/// comment, so applying any of them leaves the comment with only the rules still in use.
pub(crate) fn diagnostic(rule: &str, span: Span, fix: Fix) -> Diagnostic {
    Diagnostic {
        rule: RULE,
        message: format!("`{rule}` is never reported here, so ignoring it has no effect"),
        span,
        fix: Some(fix),
    }
}
