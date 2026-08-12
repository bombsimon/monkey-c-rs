//! `rafiki`, the Monkey C toolchain: a formatter, a linter and a language
//! server behind a single command.
//!
//! Everything is linked into this one binary rather than dispatched to
//! per-tool executables, so an install is a single file and every subcommand
//! is guaranteed to share one version of the parser.

mod cli;
mod color;
mod diagnostics;
mod discovery;
mod runner;
mod settings;

mod commands;

use clap::Parser;

use std::io;
use std::process::ExitCode;

use crate::cli::{Cli, Command};

/// Exit codes. Separating "the check found something" from "the tool could not
/// run" lets CI distinguish a failing check from a broken invocation.
const SUCCESS: u8 = 0;
const FINDINGS: u8 = 1;
const ERROR: u8 = 2;

fn main() -> ExitCode {
    let cli = Cli::parse();

    match run(&cli) {
        Ok(true) => ExitCode::from(SUCCESS),
        Ok(false) => ExitCode::from(FINDINGS),
        Err(error) => {
            // Parse errors, lost comments and invalid UTF-8 have already been
            // rendered as source snippets; printing the error again would just
            // repeat them as bare text.
            if !diagnostics::is_already_reported(&error) {
                eprintln!("rafiki: {error}");
            }

            ExitCode::from(ERROR)
        }
    }
}

/// Returns whether the run was clean — `false` means the command completed but
/// found something to report (unformatted files, lint findings).
fn run(cli: &Cli) -> io::Result<bool> {
    match &cli.command {
        Command::Fmt(args) => commands::fmt::run(&cli.global, args),
        Command::Lint(args) => commands::lint::run(&cli.global, args),
        Command::Coverage(command) => commands::coverage::run(&cli.global, command),
        Command::Server => commands::lsp::run(&cli.global),
        Command::Completions(args) => commands::completions::run(args),
    }
}
