//! `rafiki fmt` — rewrite source files in the formatter's canonical style.

use ariadne::{Color, Fmt};
use monkey_c_config::{FilesSettings, FormatSettings};
use monkey_c_formatter::Formatter;
use similar::{ChangeTag, TextDiff};

use std::fs;
use std::io::{self, Write};

use crate::cli::{FmtArgs, GlobalArgs};
use crate::diagnostics::{self, Renderer};
use crate::runner::{self, Target};
use crate::settings;

/// What to do with a file whose formatted text differs from its current text.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Mode {
    Write,
    Check,
    Diff,
}

pub fn run(global: &GlobalArgs, args: &FmtArgs) -> io::Result<bool> {
    let renderer = global.renderer();
    let loaded = settings::load(global, args.paths.first().map(AsRef::as_ref))?;
    let config = &loaded.config;

    let format = FormatSettings::default()
        .with_overrides(&config.format)
        .with_overrides(&args.format.to_config());
    let files = FilesSettings::default()
        .with_overrides(&config.files)
        .with_overrides(&args.files.to_config());

    // `--diff` implies `--check`: it reports rather than writes, and adds the
    // diff on top.
    let mode = match (args.diff, args.check) {
        (true, _) => Mode::Diff,
        (_, true) => Mode::Check,
        _ => Mode::Write,
    };

    let color = global.stdout_color();
    let exclude_root = loaded.exclude_root(&args.files.exclude)?;

    runner::each_target(&args.paths, &files, &exclude_root, |target| {
        format_target(target, mode, &format, &renderer, color)
    })
}

/// Format one target, returning whether it was already formatted (or was
/// rewritten successfully).
fn format_target(
    target: Target<'_>,
    mode: Mode,
    settings: &FormatSettings,
    renderer: &Renderer,
    color: bool,
) -> io::Result<bool> {
    let label = target.label();
    let source = match target {
        Target::Stdin => renderer.read_stdin_source()?,
        Target::File(path) => renderer.read_source(path)?,
    };

    let formatted = format_source(&source, settings, &label, renderer)?;
    let changed = formatted != source;

    match mode {
        Mode::Write => {
            match target {
                // Stdin has nowhere to be written back to, so the formatted text
                // is the command's output whether or not it changed.
                Target::Stdin => print!("{formatted}"),
                Target::File(path) if changed => fs::write(path, &formatted)?,
                Target::File(_) => {}
            }

            Ok(true)
        }
        Mode::Check if changed => {
            eprintln!("{label}: not formatted");

            Ok(false)
        }
        Mode::Diff if changed => {
            print_diff(&label, &source, &formatted, color)?;

            Ok(false)
        }
        Mode::Check | Mode::Diff => Ok(true),
    }
}

fn format_source(
    source: &str,
    settings: &FormatSettings,
    label: &str,
    renderer: &Renderer,
) -> io::Result<String> {
    let parser = monkey_c_parser::parser::Parser::new(source);
    let output = parser.parse().map_err(|e| {
        renderer.parse_error(label, source, &e);

        diagnostics::already_reported()
    })?;

    let formatted = Formatter::new(source)
        .with_line_width(settings.line_width)
        .with_alignment(settings.alignment)
        .with_decl_wrap(settings.wrap_declarations)
        .format(&output);

    // Warn (without failing) about any comment the formatter could not carry
    // into the output, pointing at the original comment via ariadne.
    for comment in Formatter::lost_comments(&output, &formatted) {
        renderer.lost_comment(label, source, comment.span.start, comment.span.end);
    }

    Ok(formatted)
}

/// Write a unified diff of the reformatting to stdout.
fn print_diff(label: &str, before: &str, after: &str, color: bool) -> io::Result<()> {
    let diff = TextDiff::from_lines(before, after);
    let mut unified = diff.unified_diff();
    let unified = unified.context_radius(3);

    let mut out = io::stdout().lock();
    writeln!(out, "{}", paint(&format!("--- {label}"), None, color))?;
    writeln!(out, "{}", paint(&format!("+++ {label}"), None, color))?;

    for hunk in unified.iter_hunks() {
        writeln!(
            out,
            "{}",
            paint(&hunk.header().to_string(), Some(Color::Cyan), color)
        )?;

        for change in hunk.iter_changes() {
            let (sign, tint) = match change.tag() {
                ChangeTag::Delete => ('-', Some(Color::Red)),
                ChangeTag::Insert => ('+', Some(Color::Green)),
                ChangeTag::Equal => (' ', None),
            };

            // `value` carries its own line ending; strip it so the colour reset
            // lands before the newline rather than after it.
            let text = change.value().trim_end_matches(['\n', '\r']);
            writeln!(out, "{}", paint(&format!("{sign}{text}"), tint, color))?;

            if change.missing_newline() {
                writeln!(out, "\\ No newline at end of file")?;
            }
        }
    }

    Ok(())
}

fn paint(text: &str, tint: Option<Color>, color: bool) -> String {
    match tint.filter(|_| color) {
        Some(tint) => text.fg(tint).to_string(),
        None => text.to_string(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_formatted_source_is_unchanged() {
        let renderer = Renderer::new(false);
        let source = "class Foo {\n}\n";
        let formatted =
            format_source(source, &FormatSettings::default(), "test", &renderer).expect("formats");

        assert_eq!(
            format_source(&formatted, &FormatSettings::default(), "test", &renderer)
                .expect("re-formats"),
            formatted,
            "formatting is idempotent"
        );
    }

    #[test]
    fn a_parse_error_is_reported_once() {
        let renderer = Renderer::new(false);
        let error = format_source("class {{{", &FormatSettings::default(), "test", &renderer)
            .expect_err("broken source fails");

        assert!(diagnostics::is_already_reported(&error));
    }

    #[test]
    fn settings_reach_the_formatter() {
        let renderer = Renderer::new(false);
        let narrow = FormatSettings {
            line_width: 20,
            ..FormatSettings::default()
        };
        let wide = FormatSettings {
            line_width: 200,
            ..FormatSettings::default()
        };

        let source =
            "class Foo {\n    function bar(alpha, beta, gamma, delta, epsilon) {\n    }\n}\n";
        let narrow_output = format_source(source, &narrow, "test", &renderer).expect("formats");
        let wide_output = format_source(source, &wide, "test", &renderer).expect("formats");

        assert_ne!(
            narrow_output, wide_output,
            "line width changes the rendering"
        );
    }

    #[test]
    fn a_diff_is_monochrome_when_colour_is_off() {
        let mut painted = paint("-gone", Some(Color::Red), false);
        assert_eq!(painted, "-gone");

        painted = paint("-gone", Some(Color::Red), true);
        assert!(painted.contains("\u{1b}["), "colour emits an escape");
    }
}
