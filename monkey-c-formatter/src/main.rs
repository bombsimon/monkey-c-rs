use clap::Parser;
use monkey_c_formatter::Formatter;

use std::fs;
use std::io::{self, Read};
use std::path::PathBuf;
use std::process::ExitCode;

/// Format Monkey C source code.
#[derive(Parser)]
#[command(version, about, long_about = None)]
struct Cli {
    /// File to format. If omitted, source is read from stdin.
    path: Option<PathBuf>,

    /// Rewrite the file in place instead of printing to stdout. Requires PATH.
    #[arg(short, long, requires = "path", conflicts_with = "check")]
    write: bool,

    /// Check whether the file is already formatted. Exits 0 if formatted,
    /// 1 if not. Prints nothing on success.
    #[arg(short, long, conflicts_with = "write")]
    check: bool,

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
            eprintln!("{e}");
            ExitCode::from(2)
        }
    }
}

/// Returns `true` when everything succeeded *and* (for `--check`) the source
/// was already formatted; `false` when `--check` finds a difference.
fn run(cli: &Cli) -> io::Result<bool> {
    let source = match &cli.path {
        Some(p) => fs::read_to_string(p)?,
        None => {
            if !cli.check {
                eprintln!("Reading from stdin");
            }

            let mut buf = String::new();
            io::stdin().read_to_string(&mut buf)?;
            buf
        }
    };

    let parser = monkey_c_parser::parser::Parser::new(&source);
    let output = parser
        .parse()
        .map_err(|e| io::Error::new(io::ErrorKind::InvalidData, format!("error: {e}")))?;

    let formatter = Formatter::new(&source)
        .with_line_width(cli.line_width)
        .with_alignment()
        .with_decl_wrap();
    let formatted = formatter.format(&output);

    if cli.check {
        let formatted_already = formatted == source;
        if !formatted_already {
            let label = cli
                .path
                .as_deref()
                .map(|p| p.display().to_string())
                .unwrap_or_else(|| "<stdin>".to_string());

            eprintln!("{label}: not formatted");
        }

        return Ok(formatted_already);
    }

    if cli.write {
        let path = cli.path.as_ref().expect("checked in main");
        if formatted != source {
            fs::write(path, formatted)?;
        }
    } else {
        print!("{formatted}");
    }

    Ok(true)
}
