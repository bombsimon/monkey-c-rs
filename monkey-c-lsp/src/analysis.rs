//! Bridges the parser/linter/formatter to LSP results for a single document.

use gen_lsp_types::{Code, Diagnostic, DiagnosticSeverity};
use monkey_c_formatter::Formatter;
use monkey_c_linter::lint;
use monkey_c_parser::ast::Span;
use monkey_c_parser::parser::Parser;

use crate::position::PositionMapper;

const SOURCE: &str = "monkey-c";

/// Parse and lint `text`, returning LSP diagnostics. A parse error yields a single error
/// diagnostic; otherwise every lint finding becomes a warning tagged with its rule name.
pub fn diagnostics(text: &str) -> Vec<Diagnostic> {
    let mapper = PositionMapper::new(text);

    match Parser::new(text).parse() {
        Err(error) => vec![Diagnostic {
            range: mapper.range(Span {
                start: error.span.start,
                end: error.span.end,
            }),
            severity: Some(DiagnosticSeverity::Error),
            source: Some(SOURCE.to_string()),
            message: error.message.into(),
            ..Default::default()
        }],
        Ok(output) => lint(&output, text)
            .iter()
            .map(|finding| lint_diagnostic(&mapper, finding))
            .collect(),
    }
}

/// The raw linter findings for `text` (empty when it doesn't parse). Unlike [`diagnostics`], these
/// retain the byte [`Span`] and [`Fix`] needed to build code actions.
///
/// [`Fix`]: monkey_c_linter::Fix
pub fn lints(text: &str) -> Vec<monkey_c_linter::Diagnostic> {
    Parser::new(text)
        .parse()
        .map(|output| lint(&output, text))
        .unwrap_or_default()
}

/// Convert one linter finding into its LSP diagnostic — the same shape [`diagnostics`] publishes,
/// so a code action can attach the diagnostic it resolves and the editor can clear it on apply.
pub fn lint_diagnostic(
    mapper: &PositionMapper,
    finding: &monkey_c_linter::Diagnostic,
) -> Diagnostic {
    Diagnostic {
        range: mapper.range(finding.span),
        severity: Some(DiagnosticSeverity::Warning),
        code: Some(Code::String(finding.rule.to_string())),
        source: Some(SOURCE.to_string()),
        message: finding.message.clone().into(),
        ..Default::default()
    }
}

/// Format `text`, or `None` when it doesn't parse (a broken document can't be re-rendered from its
/// AST).
pub fn format(text: &str) -> Option<String> {
    let output = Parser::new(text).parse().ok()?;

    Some(Formatter::new(text).format(&output))
}
