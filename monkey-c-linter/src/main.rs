use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;
use monkey_c_linter::{Diagnostic, apply_fixes, lint};

use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// Lint Monkey C source code.
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Files or directories to lint. Directories are walked recursively
    /// for `.mc` files; hidden directories (those starting with `.`) are
    /// skipped. Use `-` to read source from stdin. Defaults to the
    /// current directory.
    #[arg(default_value = ".")]
    paths: Vec<PathBuf>,

    /// Apply machine-applicable fixes in place. Ignored for stdin input.
    #[arg(long)]
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

/// Returns `true` when no findings remain (after fixing, if `--fix` was
/// requested), `false` otherwise.
fn run(cli: &Cli) -> io::Result<bool> {
    if cli.paths.iter().any(|p| p == Path::new("-")) {
        if cli.paths.len() != 1 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "cannot mix `-` (stdin) with file or directory paths",
            ));
        }

        return run_stdin();
    }

    let files = collect_mc_files(&cli.paths)?;
    let mut all_clean = true;
    for file in &files {
        match lint_file(file, cli.fix) {
            Ok(true) => {}
            Ok(false) => all_clean = false,
            Err(e) => {
                eprintln!("{}: {e}", file.display());
                all_clean = false;
            }
        }
    }

    Ok(all_clean)
}

/// Lint a single file. With `--fix`, applies any machine-applicable fixes
/// in place and returns `true`. Without `--fix`, returns `true` when no
/// findings remain.
fn lint_file(file: &Path, fix: bool) -> io::Result<bool> {
    let source = fs::read_to_string(file)?;
    let parser = monkey_c_parser::parser::Parser::new(&source);
    let output = parser
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {e}")))?;
    let diagnostics = lint(&output, &source);

    if fix {
        let fixes = diagnostics
            .iter()
            .filter_map(|d| d.fix.clone())
            .collect::<Vec<_>>();
        if !fixes.is_empty() {
            let patched = apply_fixes(&source, fixes);
            fs::write(file, patched)?;
        }

        return Ok(true);
    }

    let label = file.display().to_string();
    for d in &diagnostics {
        render_diagnostic(&label, &source, d);
    }

    Ok(diagnostics.is_empty())
}

fn run_stdin() -> io::Result<bool> {
    let mut source = String::new();
    io::stdin().read_to_string(&mut source)?;

    let parser = monkey_c_parser::parser::Parser::new(&source);
    let output = parser
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("parse error: {e}")))?;
    let diagnostics = lint(&output, &source);

    let label = "<stdin>";
    for d in &diagnostics {
        render_diagnostic(label, &source, d);
    }

    Ok(diagnostics.is_empty())
}

/// Resolve `paths` into a deterministic, sorted list of `.mc` files.
/// Files are accepted as-is regardless of extension (so explicit per-file
/// invocations always work); directories are walked recursively, skipping
/// hidden ones.
fn collect_mc_files(paths: &[PathBuf]) -> io::Result<Vec<PathBuf>> {
    let mut files = Vec::new();
    for p in paths {
        let meta = fs::metadata(p)
            .map_err(|e| io::Error::new(e.kind(), format!("{}: {e}", p.display())))?;

        if meta.is_file() {
            files.push(p.clone());
        } else if meta.is_dir() {
            walk_dir(p, &mut files)?;
        } else {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!("{}: not a file or directory", p.display()),
            ));
        }
    }

    files.sort();

    Ok(files)
}

fn walk_dir(dir: &Path, out: &mut Vec<PathBuf>) -> io::Result<()> {
    for entry in fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        let file_type = entry.file_type()?;

        let name = entry.file_name();
        let name = name.to_string_lossy();
        if name.starts_with('.') {
            continue;
        }

        if file_type.is_dir() {
            walk_dir(&path, out)?;
        } else if file_type.is_file() && path.extension().is_some_and(|e| e == "mc") {
            out.push(path);
        }
    }

    Ok(())
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
