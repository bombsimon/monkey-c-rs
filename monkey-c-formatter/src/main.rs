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
    #[arg(short, long, requires = "path")]
    write: bool,
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match run(&cli) {
        Ok(()) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("{e}");
            ExitCode::from(1)
        }
    }
}

fn run(cli: &Cli) -> io::Result<()> {
    let source = match &cli.path {
        Some(p) => fs::read_to_string(p)?,
        None => {
            eprintln!("Reading from stdin");
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
        .with_line_width(80)
        .with_aligned_dict_pairs();
    let formatted = formatter.format(&output);

    if cli.write {
        let path = cli.path.as_ref().expect("checked in main");
        if formatted != source {
            fs::write(path, formatted)?;
        }
    } else {
        print!("{formatted}");
    }

    Ok(())
}
