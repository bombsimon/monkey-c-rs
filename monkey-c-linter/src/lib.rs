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

use monkey_c_parser::ast::Span;
use monkey_c_parser::parser::ParseOutput;

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

/// A single source-byte-range replacement: replace `span` with `replacement`.
/// An empty `replacement` is a pure deletion.
#[derive(Debug, Clone, PartialEq)]
pub struct Edit {
    pub span: Span,
    pub replacement: String,
}

/// A machine-applicable fix — one or more [`Edit`]s applied together as a unit.
///
/// Most fixes are a single edit ([`Fix::single`]). Multiple edits let a fix
/// touch disjoint ranges — e.g. rewrite a header and delete a now-redundant
/// closing brace — without copying the untouched span between them into a
/// replacement string, which matters when that span is large.
#[derive(Debug, Clone, PartialEq)]
pub struct Fix {
    pub edits: Vec<Edit>,
}

impl Fix {
    /// A fix consisting of a single edit — the common case.
    pub fn single(span: Span, replacement: impl Into<String>) -> Self {
        Self {
            edits: vec![Edit {
                span,
                replacement: replacement.into(),
            }],
        }
    }
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

/// Apply `fixes` to `source` and return the patched text.
///
/// Fixes are processed left to right by their earliest edit. Each fix is
/// applied atomically: if any of its edits would overlap already-emitted output
/// the whole fix is skipped, so a fix never lands half-applied. Edits within a
/// fix must be disjoint and are applied in source order.
pub fn apply_fixes(source: &str, mut fixes: Vec<Fix>) -> String {
    fixes.sort_by_key(|f| f.edits.iter().map(|e| e.span.start).min().unwrap_or(0));

    let mut out = String::with_capacity(source.len());
    let mut cursor = 0;

    for fix in &fixes {
        let mut edits: Vec<&Edit> = fix.edits.iter().collect();
        edits.sort_by_key(|e| e.span.start);

        // Skip the whole fix unless its first (earliest) edit starts at or after
        // the cursor. Since edits are sorted and disjoint, that guarantees none
        // overlap what's already been emitted.
        match edits.first() {
            Some(first) if first.span.start >= cursor => {}
            _ => continue,
        }

        for edit in edits {
            out.push_str(&source[cursor..edit.span.start]);
            out.push_str(&edit.replacement);
            cursor = edit.span.end;
        }
    }

    out.push_str(&source[cursor..]);
    out
}
