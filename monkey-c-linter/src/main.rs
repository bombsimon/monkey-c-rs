use ariadne::{Color, Fmt, Label, Report, ReportKind, Source};
use clap::Parser;
use monkey_c_diagnostics::{
    already_reported, byte_range_to_char_range, read_source, read_stdin_source, render_parse_error,
};
use monkey_c_linter::{Diagnostic, apply_fixes, lint};

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// Lint Monkey C source code.
#[derive(Parser)]
#[command(version, about, long_about = None, after_help = available_lints_help())]
struct Cli {
    /// Files or directories to lint. Directories are walked recursively
    /// for `.mc` files; hidden directories (those starting with `.`) are
    /// skipped. Use `-` to read source from stdin. Defaults to the
    /// current directory.
    #[arg(default_value = "-")]
    paths: Vec<PathBuf>,

    /// Apply machine-applicable fixes in place. Ignored for stdin input.
    #[arg(long)]
    fix: bool,

    /// Comma-separated list of rule names to run. When set, only these rules
    /// are enabled; all others are silenced. Combined with `--disable`, a rule
    /// must be enabled *and* not disabled to run. See the list below.
    #[arg(long, value_delimiter = ',')]
    enable: Vec<String>,

    /// Comma-separated list of rule names to silence. See the list below.
    #[arg(long, value_delimiter = ',')]
    disable: Vec<String>,
}

/// The `Available lints:` block appended to `--help`, built from the single
/// source of truth in [`monkey_c_linter::rules::ALL`].
fn available_lints_help() -> String {
    let mut help = String::from("Available lints:\n");
    for rule in monkey_c_linter::rules::ALL {
        help.push_str("  ");
        help.push_str(rule);
        help.push('\n');
    }

    help
}

/// Whether a rule runs given the `--enable`/`--disable` selections. An empty
/// `enable` means "all rules"; a non-empty `enable` is a whitelist. `disable`
/// always subtracts, so a rule in both is silenced.
fn rule_is_selected(rule: &str, enable: &[&str], disable: &[&str]) -> bool {
    (enable.is_empty() || enable.contains(&rule)) && !disable.contains(&rule)
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(e) => {
            // Diagnostics for `io::ErrorKind::Other` are already printed via
            // ariadne by `read_source`/`run_lint`.
            if e.kind() != io::ErrorKind::Other {
                eprintln!("{e}");
            }
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

        return run_stdin(cli);
    }

    let files = collect_mc_files(&cli.paths)?;
    let mut all_clean = true;
    for file in &files {
        match lint_file(file, cli) {
            Ok(true) => {}
            Ok(false) => all_clean = false,
            Err(e) => {
                // Invalid-UTF-8 diagnostics are already printed via ariadne
                // by `read_source`.
                if e.kind() != io::ErrorKind::Other {
                    eprintln!("{}: {e}", file.display());
                }
                all_clean = false;
            }
        }
    }

    Ok(all_clean)
}

/// Lint a single file. With `--fix`, applies any machine-applicable fixes
/// in place and returns `true`. Without `--fix`, returns `true` when no
/// findings remain.
fn lint_file(file: &Path, cli: &Cli) -> io::Result<bool> {
    let original = read_source(file)?;
    let label = file.display().to_string();

    let diagnostics = run_lint(&original, cli, &label)?;

    if cli.fix {
        let fixes = diagnostics
            .iter()
            .filter_map(|d| d.fix.clone())
            .collect::<Vec<_>>();
        let fixed = apply_fixes(&original, fixes);
        if fixed != original {
            fs::write(file, fixed)?;
        }

        return Ok(true);
    }

    for d in &diagnostics {
        render_diagnostic(&label, &original, d);
    }

    Ok(diagnostics.is_empty())
}

fn run_stdin(cli: &Cli) -> io::Result<bool> {
    let source = read_stdin_source()?;
    let label = "<stdin>";
    let diagnostics = run_lint(&source, cli, label)?;

    for d in &diagnostics {
        render_diagnostic(label, &source, d);
    }

    Ok(diagnostics.is_empty())
}

fn run_lint(source: &str, cli: &Cli, label: &str) -> io::Result<Vec<Diagnostic>> {
    let enabled: Vec<&str> = cli.enable.iter().map(String::as_str).collect();
    let disabled: Vec<&str> = cli.disable.iter().map(String::as_str).collect();

    let parser = monkey_c_parser::parser::Parser::new(source);
    let output = parser.parse().map_err(|e| {
        render_parse_error(label, source, &e);
        already_reported()
    })?;

    let diagnostics = lint(&output, source)
        .into_iter()
        .filter(|d| rule_is_selected(d.rule, &enabled, &disabled))
        .collect();

    Ok(diagnostics)
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
        builder = builder.with_note(fix_note(fix));
    }

    builder
        .finish()
        .eprint((file, Source::from(source)))
        .expect("ariadne write to stderr");
}

/// The `fix:` note for a diagnostic. A fix's first non-empty replacement is the
/// headline; any remaining edits (e.g. a brace removal) are summarised as a
/// count so the note stays compact even when the untouched body is large.
fn fix_note(fix: &monkey_c_linter::Fix) -> String {
    let headline = fix.edits.iter().find(|e| !e.replacement.is_empty());

    match headline {
        Some(edit) => {
            // Wrap each line with its own ANSI codes — ariadne's per-line note
            // decoration resets the active color at the line break, so a single
            // open/close around the whole string only colours the first line.
            let colored = edit
                .replacement
                .lines()
                .map(|line| line.fg(Color::BrightGreen).to_string())
                .collect::<Vec<_>>()
                .join("\n");

            let others = fix.edits.len() - 1;
            if others == 0 {
                format!("fix: replace with `{colored}`")
            } else {
                format!(
                    "fix: replace with `{colored}` (+{others} more edit{})",
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

fn plural(n: usize) -> &'static str {
    if n == 1 { "" } else { "s" }
}

#[cfg(test)]
mod tests {
    use super::rule_is_selected;

    #[test]
    fn no_flags_runs_every_rule() {
        assert!(rule_is_selected("import-order", &[], &[]));
    }

    #[test]
    fn enable_is_a_whitelist() {
        assert!(rule_is_selected("import-order", &["import-order"], &[]));
        assert!(!rule_is_selected(
            "naming-convention",
            &["import-order"],
            &[]
        ));
    }

    #[test]
    fn disable_subtracts() {
        assert!(!rule_is_selected("import-order", &[], &["import-order"]));
        assert!(rule_is_selected(
            "naming-convention",
            &[],
            &["import-order"]
        ));
    }

    #[test]
    fn disable_wins_over_enable() {
        // A rule listed in both is silenced.
        assert!(!rule_is_selected(
            "import-order",
            &["import-order", "naming-convention"],
            &["import-order"],
        ));
        assert!(rule_is_selected(
            "naming-convention",
            &["import-order", "naming-convention"],
            &["import-order"],
        ));
    }
}
