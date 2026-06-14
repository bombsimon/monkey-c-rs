use clap::Parser;
use monkey_c_diagnostics::{already_reported, read_source, read_stdin_source, render_parse_error};
use monkey_c_formatter::Formatter;

use std::fs;
use std::io;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

/// Format Monkey C source code.
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// Files or directories to format. Directories are walked recursively
    /// for `.mc` files; hidden directories (those starting with `.`) are
    /// skipped. Use `-` to read source from stdin and write the formatted
    /// result to stdout. Defaults to the current directory.
    #[arg(default_value = "-")]
    paths: Vec<PathBuf>,

    /// Check whether each file is already formatted. Exits 0 when all
    /// files are formatted, 1 when any differ. No files are rewritten.
    #[arg(short, long)]
    check: bool,

    /// Wrap multiple  declarations similar to the Prettier formatter.
    #[arg(short, long, default_value_t = false)]
    wrap_declarations: bool,

    /// Align enums and fat-commans (=>)
    #[arg(short, long = "no-alignment", action = clap::ArgAction::SetFalse, default_value_t = true)]
    alignment: bool,

    /// Target line width before wrapping.
    #[arg(short = 'l', long, default_value_t = 111)]
    line_width: usize,
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli) {
        Ok(true) => ExitCode::SUCCESS,
        Ok(false) => ExitCode::from(1),
        Err(e) => {
            // Diagnostics for `io::ErrorKind::Other` are already printed via
            // ariadne by `read_source`/`format_source`.
            if e.kind() != io::ErrorKind::Other {
                eprintln!("{e}");
            }

            ExitCode::from(2)
        }
    }
}

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
    let mut all_ok = true;
    for file in &files {
        match format_file(file, cli) {
            Ok(true) => {}
            Ok(false) => all_ok = false,
            Err(e) => {
                // Invalid-UTF-8 diagnostics are already printed via ariadne
                // by `read_source`.
                if e.kind() != io::ErrorKind::Other {
                    eprintln!("{}: {e}", file.display());
                }
                all_ok = false;
            }
        }
    }

    Ok(all_ok)
}

/// Format a single file. With `--check`, returns `false` (without writing)
/// when the file would change; otherwise returns `true` and writes the
/// formatted text in place. Returns `true` for a no-op match too.
fn format_file(file: &Path, cli: &Cli) -> io::Result<bool> {
    let source = read_source(file)?;
    let formatted = format_source(&source, cli, &file.display().to_string())?;

    if cli.check {
        if formatted != source {
            eprintln!("{}: not formatted", file.display());

            return Ok(false);
        }

        return Ok(true);
    }

    if formatted != source {
        fs::write(file, formatted)?;
    }

    Ok(true)
}

fn run_stdin(cli: &Cli) -> io::Result<bool> {
    let source = read_stdin_source()?;
    let formatted = format_source(&source, cli, "<stdin>")?;

    if cli.check {
        let ok = formatted == source;
        if !ok {
            eprintln!("<stdin>: not formatted");
        }

        return Ok(ok);
    }

    print!("{formatted}");

    Ok(true)
}

fn format_source(source: &str, cli: &Cli, label: &str) -> io::Result<String> {
    let parser = monkey_c_parser::parser::Parser::new(source);
    let output = parser.parse().map_err(|e| {
        render_parse_error(label, source, &e);
        already_reported()
    })?;

    let formatter = Formatter::new(source)
        .with_line_width(cli.line_width)
        .with_alignment(cli.alignment)
        .with_decl_wrap(cli.wrap_declarations);

    Ok(formatter.format(&output))
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
