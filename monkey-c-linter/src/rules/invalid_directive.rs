//! `invalid-directive` — flag a `rafiki:` comment that can't be read.
//!
//! A typo in a directive would otherwise do nothing without saying so, like leaving the finding a
//! suppression was meant to hide reported. Detection lives in `suppression`, which parses the
//! comments.
use monkey_c_parser::ast::Span;

use crate::Diagnostic;

pub const RULE: &str = "invalid-directive";

pub(crate) fn diagnostic(message: impl Into<String>, span: Span) -> Diagnostic {
    Diagnostic {
        rule: RULE,
        message: message.into(),
        span,
        fix: None,
    }
}
