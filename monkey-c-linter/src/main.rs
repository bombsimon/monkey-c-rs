use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;
use monkey_c_linter::{Diagnostic, apply_fixes, lint};

use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process::ExitCode;

/// Lint Monkey C source code.
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// File to lint. If omitted, source is read from stdin.
    path: Option<PathBuf>,

    /// Apply machine-applicable fixes in place. Requires PATH.
    #[arg(long, requires = "path")]
    fix: bool,
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(e) => {
            eprintln!("{e}");
            ExitCode::from(2)
        }
    }
}

/// Returns `true` if there were no findings (after fixing, if `--fix` was
/// requested), `false` if findings remain.
fn run(cli: &Cli) -> io::Result<bool> {
    let source = match &cli.path {
        Some(p) => fs::read_to_string(p)?,
        None => {
            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            buf
        }
    };

    let parser = monkey_c_parser::parser::Parser::new(&source);
    let output = parser
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {e}")))?;
    let diagnostics = lint(&output, &source);

    if cli.fix {
        let fixes = diagnostics
            .iter()
            .filter_map(|d| d.fix.clone())
            .collect::<Vec<_>>();
        if !fixes.is_empty() {
            let patched = apply_fixes(&source, fixes);
            fs::write(cli.path.as_ref().expect("checked"), patched)?;
        }

        return Ok(true);
    }

    let label = cli
        .path
        .as_deref()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "<stdin>".to_string());

    for d in &diagnostics {
        render_diagnostic(&label, &source, d);
    }

    Ok(diagnostics.is_empty())
}

/// Render a single diagnostic to stderr using ariadne — coloured caret with
/// file:line:col, source snippet, and the fix replacement (if any) shown as
/// a note.
fn render_diagnostic(file: &str, source: &str, d: &Diagnostic) {
    // Ariadne indexes its `Source` by character, not byte. Convert our byte
    // spans to char offsets so multibyte UTF-8 anywhere earlier in the file
    // doesn't shift the rendered position.
    let range = byte_range_to_char_range(source, d.span.start, d.span.end);
    let mut builder = Report::build(ReportKind::Warning, (file, range.clone()))
        .with_code(d.rule)
        .with_message(&d.message)
        .with_label(
            Label::new((file, range))
                .with_message(&d.message)
                .with_color(Color::Yellow),
        );

    if let Some(fix) = &d.fix {
        // Wrap each line with its own ANSI codes — ariadne's per-line note
        // decoration resets the active color at the line break, so a single
        // open/close around the whole string only colours the first line.
        let colored = fix
            .replacement
            .lines()
            .map(|line| line.fg(Color::BrightGreen).to_string())
            .collect::<Vec<_>>()
            .join("\n");
        builder = builder.with_note(format!("fix: replace with `{colored}`"));
    }

    builder
        .finish()
        .eprint((file, Source::from(source)))
        .expect("ariadne write to stderr");
}

/// Convert a `[byte_start, byte_end)` range in `source` into the matching
/// `[char_start, char_end)` range. Ariadne's `Source` indexes by char, so
/// passing byte offsets unchanged misplaces labels whenever the source
/// contains any multibyte UTF-8 character earlier in the file.
fn byte_range_to_char_range(
    source: &str,
    byte_start: usize,
    byte_end: usize,
) -> std::ops::Range<usize> {
    let mut char_start = 0;
    let mut char_end = 0;
    let mut byte_pos = 0;

    for c in source.chars() {
        if byte_pos == byte_start {
            char_start = char_end;
        }

        if byte_pos == byte_end {
            return char_start..char_end;
        }

        byte_pos += c.len_utf8();
        char_end += 1;
    }

    if byte_pos == byte_start {
        char_start = char_end;
    }

    char_start..char_end
}
