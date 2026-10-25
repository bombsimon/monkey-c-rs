//! Monkey C linter, reporting [`Diagnostic`]s for a parsed file.
//!
//! Fixes are plain text replacements, like clippy's and ruff's, so they leave the formatting
//! around them alone and don't depend on the formatter.
//!
//! # Adding a rule
//!
//! 1. Add a module under [`rules`] with a `RULE` name and a check function for the nodes it looks
//!    at, such as `check_expr(&Expr, &LintContext) -> Option<Diagnostic>`.
//! 2. Call the check from the matching `dispatch_*` function in [`visit`].
//! 3. List the rule in [`rules::ALL`], and give it a page under `docs/src/linter/rules` linked
//!    from `SUMMARY.md`. Run `just update-docs` to refresh the rules table.
//!
//! Findings silenced by a `rafiki: ignore` comment are dropped by [`lint`], so a rule never has to
//! look at comments for that.
pub mod rules;
pub mod suppression;
pub mod visit;

use monkey_c_parser::ast::Span;
use monkey_c_parser::parser::ParseOutput;

use crate::suppression::Suppressions;
use crate::visit::LintContext;

/// A finding of one rule.
#[derive(Debug, Clone, PartialEq)]
pub struct Diagnostic {
    /// The rule's `RULE` name.
    pub rule: &'static str,
    pub message: String,
    /// What the finding is about. Only its start decides which `rafiki: ignore` comment covers it.
    pub span: Span,
    pub fix: Option<Fix>,
}

/// Replace `span` with `replacement`, which deletes it when empty.
#[derive(Debug, Clone, PartialEq)]
pub struct Edit {
    pub span: Span,
    pub replacement: String,
}

/// [`Edit`]s applied together or not at all. Several edits let a fix touch disjoint ranges, like a
/// header and a closing brace, without copying everything between them.
#[derive(Debug, Clone, PartialEq)]
pub struct Fix {
    pub edits: Vec<Edit>,
}

impl Fix {
    /// A fix of a single edit, which is what most rules need.
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
pub fn lint(output: &ParseOutput, source: &str) -> Vec<Diagnostic> {
    lint_selected(output, source, |_| true)
}

/// Like [`lint`], but only reports the rules `selected` accepts. Findings silenced by a
/// `rafiki: ignore` comment are dropped either way, so their fixes are never applied.
pub fn lint_selected(
    output: &ParseOutput,
    source: &str,
    selected: impl Fn(&str) -> bool,
) -> Vec<Diagnostic> {
    let ctx = LintContext { source };
    let mut diags = Vec::new();
    visit::walk(&output.ast, &ctx, &mut diags);

    // Findings about the comments are added after suppression, so no comment can silence them.
    let suppressions = Suppressions::new(output, source);
    let directive_findings = suppressions.findings(&diags, source, &selected);

    diags.retain(|d| !suppressions.covers(d));
    diags.extend(directive_findings);
    diags.retain(|d| selected(d.rule));

    diags.sort_by_key(|d| d.span.start);
    diags
}

/// Apply `fixes` to `source`, skipping a fix that overlaps one applied before it. Edits within a
/// fix must be disjoint.
pub fn apply_fixes(source: &str, mut fixes: Vec<Fix>) -> String {
    fixes.sort_by_key(|f| f.edits.iter().map(|e| e.span.start).min().unwrap_or(0));

    let mut out = String::with_capacity(source.len());
    let mut cursor = 0;

    for fix in &fixes {
        let mut edits: Vec<&Edit> = fix.edits.iter().collect();
        edits.sort_by_key(|e| e.span.start);

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

#[cfg(test)]
mod test_support {
    use monkey_c_parser::parser::Parser;

    use crate::{Diagnostic, apply_fixes, lint};

    /// The findings of `rules` in `src`.
    pub(crate) fn lint_rules(src: &str, rules: &[&str]) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");

        lint(&output, src)
            .into_iter()
            .filter(|diagnostic| rules.contains(&diagnostic.rule))
            .collect()
    }

    /// `src` with the fixes for the findings of `rules` applied.
    pub(crate) fn fix_rules(src: &str, rules: &[&str]) -> String {
        let fixes = lint_rules(src, rules)
            .into_iter()
            .filter_map(|diagnostic| diagnostic.fix)
            .collect();

        apply_fixes(src, fixes)
    }
}
