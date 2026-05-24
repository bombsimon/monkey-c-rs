//! Monkey C linter — reads a parsed [`ParseOutput`] together with the
//! original source and emits [`Diagnostic`]s, some of which carry [`Fix`]
//! suggestions that can be applied directly to the source bytes.
//!
//! The linter is intentionally independent of `monkey-c-formatter`: fixes
//! are text-range replacements (route A — like clippy and ruff), so they
//! survive the user's formatting choices outside the fix region. If
//! whitespace normalization is desired after applying fixes, the formatter
//! is run as a separate pass.
//!
//! Adding a rule:
//! 1. Add `pub mod my_rule;` to [`rules`] and write
//!    `pub fn check_expr(expr, pos, ctx) -> Option<Diagnostic>` (or
//!    `check_stmt`, …).
//! 2. Add one line to the matching `dispatch_*` in [`visit`].
pub mod rules;
pub mod visit;

use monkey_c_parser::ast::{ParseOutput, Span};

use crate::visit::LintContext;

/// A lint finding.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    /// Stable identifier for the rule that produced this diagnostic.
    pub rule: &'static str,
    /// Human-readable message describing what's wrong.
    pub message: String,
    /// Source range the diagnostic refers to.
    pub span: Span,
    /// Optional machine-applicable fix. `None` means the diagnostic is
    /// advisory only.
    pub fix: Option<Fix>,
}

/// A source-byte-range replacement. Applying a fix means: take `span` in the
/// original source and replace it with `replacement`.
#[derive(Debug, Clone, PartialEq)]
pub struct Fix {
    pub span: Span,
    pub replacement: String,
}

/// Run all rules over `output` and return their diagnostics, in source order.
/// `source` is the original source text — rules use it to build replacement
/// strings for their fixes.
pub fn lint(output: &ParseOutput, source: &str) -> Vec<Diagnostic> {
    let ctx = LintContext { source };
    let mut diags = Vec::new();
    visit::walk(&output.ast, &ctx, &mut diags);

    diags.sort_by_key(|d| d.span.start);
    diags
}

/// Apply `fixes` to `source` and return the patched text. Overlapping fixes
/// are detected and dropped (earliest wins).
pub fn apply_fixes(source: &str, mut fixes: Vec<Fix>) -> String {
    fixes.sort_by_key(|f| f.span.start);

    let mut out = String::with_capacity(source.len());
    let mut cursor = 0;

    for fix in fixes {
        if fix.span.start < cursor {
            continue;
        }

        out.push_str(&source[cursor..fix.span.start]);
        out.push_str(&fix.replacement);
        cursor = fix.span.end;
    }

    out.push_str(&source[cursor..]);
    out
}
