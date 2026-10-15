//! Bridges the parser/linter/formatter to LSP results for a single document.

use gen_lsp_types::{Code, Diagnostic, DiagnosticSeverity};
use monkey_c_formatter::Formatter;
use monkey_c_linter::lint_selected;
use monkey_c_parser::ast::Span;
use monkey_c_parser::parser::{ParseOutput, Parser};

use crate::position::PositionMapper;
use monkey_c_config::{FormatSettings, LintSettings};

const SOURCE: &str = "monkey-c";

/// Parse and lint `text`, returning LSP diagnostics. A parse error yields a single error
/// diagnostic; otherwise every lint finding becomes a warning tagged with its rule name.
pub fn diagnostics(text: &str, settings: &LintSettings) -> Vec<Diagnostic> {
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
        Ok(output) => lint_selected(&output, text, |rule| settings.selects(rule))
            .iter()
            .map(|finding| lint_diagnostic(&mapper, finding))
            .collect(),
    }
}

/// The parsed `text` with its raw linter findings, or `None` when it doesn't parse. Unlike
/// [`diagnostics`], the findings retain the byte [`Span`] and [`Fix`] needed to build code actions,
/// and the parse lets an action see the comments around a finding.
///
/// [`Fix`]: monkey_c_linter::Fix
pub fn lints(
    text: &str,
    settings: &LintSettings,
) -> Option<(ParseOutput, Vec<monkey_c_linter::Diagnostic>)> {
    let output = Parser::new(text).parse().ok()?;
    let findings = lint_selected(&output, text, |rule| settings.selects(rule));

    Some((output, findings))
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

/// Format `text` with `settings`, or `None` when it doesn't parse (a broken
/// document can't be re-rendered from its AST).
pub fn format(text: &str, settings: &FormatSettings) -> Option<String> {
    let output = Parser::new(text).parse().ok()?;

    let formatted = Formatter::new(text)
        .with_line_width(settings.line_width)
        .with_alignment(settings.alignment)
        .with_short_array_element_width(settings.short_array_element_width)
        .format(&output);

    Some(formatted)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn disabled_lints_are_not_published_or_offered_as_actions() {
        let text = "class Foo_bar {\n}\n";
        let settings = LintSettings {
            disable: vec!["naming-convention".to_string()],
            ..LintSettings::default()
        };

        assert!(diagnostics(text, &settings).is_empty());

        let (_, findings) = lints(text, &settings).expect("parses");
        assert!(findings.is_empty());
    }
}
