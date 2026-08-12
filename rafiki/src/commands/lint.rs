//! `rafiki lint` — report, and optionally fix, lint findings.

use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use monkey_c_config::{FilesSettings, LintSettings};
use monkey_c_linter::{Diagnostic, Fix, apply_fixes, lint, rules};

use std::fs;
use std::io;

use crate::cli::{GlobalArgs, LintArgs};
use crate::diagnostics::{self, Renderer};
use crate::runner::{self, Target};
use crate::settings;

pub fn run(global: &GlobalArgs, args: &LintArgs) -> io::Result<bool> {
    if args.list_rules {
        for rule in rules::ALL {
            println!("{rule}");
        }

        return Ok(true);
    }

    let renderer = global.renderer();
    let loaded = settings::load(global, args.paths.first().map(AsRef::as_ref))?;
    let config = &loaded.config;

    let lint_settings = LintSettings::default()
        .with_overrides(&config.lint)
        .with_overrides(&args.to_config());
    let files = FilesSettings::default()
        .with_overrides(&config.files)
        .with_overrides(&args.files.to_config());

    validate_rule_names(&lint_settings)?;

    let exclude_root = loaded.exclude_root(&args.files.exclude)?;

    runner::each_target(&args.paths, &files, &exclude_root, |target| {
        lint_target(target, args.fix, &lint_settings, &renderer)
    })
}

/// Reject rule names no rule answers to. A misspelling in `enable` would
/// otherwise silence every rule and look like a clean run.
fn validate_rule_names(settings: &LintSettings) -> io::Result<()> {
    let unknown: Vec<&str> = settings
        .named_rules()
        .filter(|name| !rules::ALL.contains(name))
        .collect();

    if unknown.is_empty() {
        return Ok(());
    }

    let mut message = format!(
        "unknown lint rule{}: {}",
        plural(unknown.len()),
        unknown.join(", ")
    );
    message.push_str("\n\nAvailable rules:\n");
    for rule in rules::ALL {
        message.push_str(&format!("  {rule}\n"));
    }

    Err(io::Error::new(io::ErrorKind::InvalidInput, message))
}

/// Lint one target, returning whether it was clean. With `--fix`, fixes are
/// applied in place first; the still-outstanding findings (unfixable ones, plus
/// any a fix's own edits exposed) are then re-linted from the fixed source, so
/// they're reported and reflected in the exit status just like an unfixed run.
fn lint_target(
    target: Target<'_>,
    fix: bool,
    settings: &LintSettings,
    renderer: &Renderer,
) -> io::Result<bool> {
    let label = target.label();
    let source = match target {
        Target::Stdin => renderer.read_stdin_source()?,
        Target::File(path) => renderer.read_source(path)?,
    };

    let initial_findings = findings(&source, settings, &label, renderer)?;

    // Fixing is meaningless for stdin: there is no file to rewrite, so the
    // findings are reported instead.
    let (source, findings) = if let (true, Target::File(path)) = (fix, target) {
        let fixes: Vec<Fix> = initial_findings
            .iter()
            .filter_map(|f| f.fix.clone())
            .collect();
        let fixed = apply_fixes(&source, fixes);
        if fixed != source {
            fs::write(path, &fixed)?;
        }

        let remaining = self::findings(&fixed, settings, &label, renderer)?;

        (fixed, remaining)
    } else {
        (source, initial_findings)
    };

    for finding in &findings {
        render_finding(&label, &source, finding, renderer);
    }

    Ok(findings.is_empty())
}

fn findings(
    source: &str,
    settings: &LintSettings,
    label: &str,
    renderer: &Renderer,
) -> io::Result<Vec<Diagnostic>> {
    let parser = monkey_c_parser::parser::Parser::new(source);
    let output = parser.parse().map_err(|e| {
        renderer.parse_error(label, source, &e);

        diagnostics::already_reported()
    })?;

    let findings = lint(&output, source)
        .into_iter()
        .filter(|finding| settings.selects(finding.rule))
        .collect();

    Ok(findings)
}

/// Render a single finding to stderr — coloured caret with file:line:col, source
/// snippet, and the fix replacement (if any) shown as a note.
fn render_finding(file: &str, source: &str, finding: &Diagnostic, renderer: &Renderer) {
    // Ariadne indexes its `Source` by character, not byte. Convert our byte
    // spans to char offsets so multibyte UTF-8 anywhere earlier in the file
    // doesn't shift the rendered position.
    let range = diagnostics::byte_range_to_char_range(source, finding.span.start, finding.span.end);
    let mut builder = Report::build(ReportKind::Warning, (file, range.clone()))
        .with_config(renderer.config())
        .with_code(finding.rule)
        .with_message(&finding.message)
        .with_label(
            Label::new((file, range))
                .with_message(&finding.message)
                .with_color(Color::Yellow),
        );

    if let Some(fix) = &finding.fix {
        builder = builder.with_note(fix_note(fix, renderer.color()));
    }

    builder
        .finish()
        .eprint((file, Source::from(source)))
        .expect("ariadne write to stderr");
}

/// The `fix:` note for a finding. A fix's first non-empty replacement is the
/// headline; any remaining edits (e.g. a brace removal) are summarised as a
/// count so the note stays compact even when the untouched body is large.
fn fix_note(fix: &Fix, color: bool) -> String {
    let headline = fix.edits.iter().find(|edit| !edit.replacement.is_empty());

    match headline {
        Some(edit) => {
            let replacement = highlight(&edit.replacement, color);
            let others = fix.edits.len() - 1;

            if others == 0 {
                format!("fix: replace with `{replacement}`")
            } else {
                format!(
                    "fix: replace with `{replacement}` (+{others} more edit{})",
                    plural(others)
                )
            }
        }
        None => format!(
            "fix: remove {} range{}",
            fix.edits.len(),
            plural(fix.edits.len())
        ),
    }
}

/// Tint a fix replacement. Each line is wrapped separately because ariadne's
/// per-line note decoration resets the active colour at the line break, so a
/// single open/close around the whole string only colours the first line.
fn highlight(replacement: &str, color: bool) -> String {
    if !color {
        return replacement.to_string();
    }

    replacement
        .lines()
        .map(|line| line.fg(Color::BrightGreen).to_string())
        .collect::<Vec<_>>()
        .join("\n")
}

fn plural(n: usize) -> &'static str {
    if n == 1 { "" } else { "s" }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn settings(enable: &[&str], disable: &[&str]) -> LintSettings {
        LintSettings {
            enable: enable.iter().map(|s| s.to_string()).collect(),
            disable: disable.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn every_real_rule_name_validates() {
        let all: Vec<&str> = rules::ALL.to_vec();
        validate_rule_names(&settings(&all, &[])).expect("the real rule list is accepted");
    }

    #[test]
    fn an_unknown_rule_name_is_rejected() {
        let error = validate_rule_names(&settings(&["import-ordering"], &[]))
            .expect_err("a misspelled rule is rejected");

        assert_eq!(error.kind(), io::ErrorKind::InvalidInput);
        assert!(error.to_string().contains("import-ordering"), "{error}");
        // The message lists what the user could have meant.
        assert!(error.to_string().contains("import-order"), "{error}");
    }

    #[test]
    fn an_unknown_disabled_rule_is_also_rejected() {
        validate_rule_names(&settings(&[], &["nope"])).expect_err("disable is validated too");
    }

    #[test]
    fn no_selection_validates() {
        validate_rule_names(&LintSettings::default()).expect("an empty selection is fine");
    }

    #[test]
    fn a_fix_note_is_monochrome_when_colour_is_off() {
        assert_eq!(highlight("a\nb", false), "a\nb");
        assert!(highlight("a", true).contains("\u{1b}["));
    }

    #[test]
    fn a_parse_error_stops_linting_the_target() {
        let renderer = Renderer::new(false);
        let error = findings("class {{{", &LintSettings::default(), "test", &renderer)
            .expect_err("broken source fails");

        assert!(diagnostics::is_already_reported(&error));
    }

    #[test]
    fn fix_reports_unclean_when_an_unfixable_finding_remains() {
        let renderer = Renderer::new(false);
        // `one-class-per-file` has no automatic fix, so `--fix` leaves the file
        // exactly as it found it and must still report the run as unclean.
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("Two.mc");
        fs::write(&path, "class A {\n}\nclass B {\n}\n").expect("write fixture");

        let clean = lint_target(
            Target::File(&path),
            true,
            &LintSettings::default(),
            &renderer,
        )
        .expect("lints");

        assert!(!clean, "an unfixable finding must fail the run");
    }

    #[test]
    fn disabled_rules_are_filtered_out() {
        let renderer = Renderer::new(false);
        // `Foo_bar` violates the naming convention for a class.
        let source = "class Foo_bar {\n}\n";

        let all = findings(source, &LintSettings::default(), "test", &renderer).expect("lints");
        assert!(
            all.iter().any(|f| f.rule == "naming-convention"),
            "the fixture triggers naming-convention"
        );

        let filtered = findings(
            source,
            &settings(&[], &["naming-convention"]),
            "test",
            &renderer,
        )
        .expect("lints");
        assert!(filtered.iter().all(|f| f.rule != "naming-convention"));
    }
}
