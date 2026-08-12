//! `rafiki completions` — print a shell completion script.
//!
//! Generated from the same clap definitions the parser uses, so completions
//! cannot drift from the real flags.

use clap::CommandFactory;

use std::io;

use crate::cli::{Cli, CompletionsArgs};

pub fn run(args: &CompletionsArgs) -> io::Result<bool> {
    let mut command = Cli::command();
    let name = command.get_name().to_string();

    clap_complete::generate(args.shell, &mut command, name, &mut io::stdout());

    Ok(true)
}

#[cfg(test)]
mod tests {
    use super::*;
    use clap_complete::Shell;

    #[test]
    fn a_script_is_generated_for_every_supported_shell() {
        for shell in [
            Shell::Bash,
            Shell::Zsh,
            Shell::Fish,
            Shell::Elvish,
            Shell::PowerShell,
        ] {
            let mut command = Cli::command();
            let mut script = Vec::new();
            clap_complete::generate(shell, &mut command, "rafiki", &mut script);

            let script = String::from_utf8(script).expect("scripts are utf-8");
            assert!(script.contains("rafiki"), "{shell} script names the binary");
            assert!(!script.is_empty());
        }
    }
}
