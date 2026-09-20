//! Whether to emit ANSI colour, decided per output stream.
//!
//! `--color` is authoritative when given. Otherwise the widely-implemented
//! environment conventions apply before falling back to terminal detection, so
//! that piping into a file or a pager produces plain text.

use std::env;
use std::io::{self, IsTerminal};

use crate::cli::ColorWhen;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Stream {
    Stdout,
    Stderr,
}

pub fn enabled(when: ColorWhen, stream: Stream) -> bool {
    match when {
        ColorWhen::Always => true,
        ColorWhen::Never => false,
        ColorWhen::Auto => auto(is_terminal(stream), &env_overrides()),
    }
}

fn is_terminal(stream: Stream) -> bool {
    match stream {
        Stream::Stdout => io::stdout().is_terminal(),
        Stream::Stderr => io::stderr().is_terminal(),
    }
}

/// The environment's colour preferences, read once so the decision logic is
/// testable without mutating process state.
#[derive(Debug, Default, Clone, Copy)]
struct EnvOverrides {
    no_color: bool,
    force_color: bool,
    dumb_terminal: bool,
}

fn env_overrides() -> EnvOverrides {
    EnvOverrides {
        no_color: is_set("NO_COLOR"),
        force_color: is_set("CLICOLOR_FORCE"),
        dumb_terminal: env::var_os("TERM").is_some_and(|term| term == "dumb"),
    }
}

/// Whether a variable is set to anything other than the empty string or `0`,
/// which is how both the `NO_COLOR` and `CLICOLOR` conventions define "set".
fn is_set(name: &str) -> bool {
    env::var_os(name).is_some_and(|value| !value.is_empty() && value != "0")
}

/// The environment-independent part of the `Auto` decision, taking whether
/// the stream is a terminal as a plain `bool` rather than querying it
/// directly — `io::stdout()`/`io::stderr()` reflect the real process's file
/// descriptors regardless of how a test harness captures `print!` output, so
/// a test asserting on `Stream::Stdout` directly would pass or fail based on
/// whether `cargo test` itself happened to run with a terminal attached,
/// rather than on this function's own logic.
fn auto(is_terminal: bool, env: &EnvOverrides) -> bool {
    if env.force_color {
        return true;
    }

    if env.no_color || env.dumb_terminal {
        return false;
    }

    is_terminal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_explicit_choice_ignores_the_environment() {
        // Tests run with stdout captured, so `auto` would say false regardless.
        assert!(enabled(ColorWhen::Always, Stream::Stdout));
        assert!(!enabled(ColorWhen::Never, Stream::Stdout));
    }

    #[test]
    fn no_color_disables_auto() {
        let env = EnvOverrides {
            no_color: true,
            ..EnvOverrides::default()
        };
        assert!(!auto(true, &env));
    }

    #[test]
    fn a_dumb_terminal_disables_auto() {
        let env = EnvOverrides {
            dumb_terminal: true,
            ..EnvOverrides::default()
        };
        assert!(!auto(true, &env));
    }

    #[test]
    fn force_color_wins_over_no_color() {
        let env = EnvOverrides {
            no_color: true,
            force_color: true,
            dumb_terminal: true,
        };
        assert!(auto(false, &env));
    }

    #[test]
    fn a_non_terminal_stream_is_plain() {
        assert!(!auto(false, &EnvOverrides::default()));
    }

    #[test]
    fn a_terminal_stream_with_no_overrides_is_colour() {
        assert!(auto(true, &EnvOverrides::default()));
    }
}
