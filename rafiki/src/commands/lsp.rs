//! `rafiki server` — serve the parser, linter and formatter over LSP on stdio.

use std::io;

use crate::cli::GlobalArgs;

pub fn run(global: &GlobalArgs) -> io::Result<bool> {
    // The server is handed the source rather than a resolved configuration: only
    // it learns the workspace root, from the client's `initialize`, and that is
    // the directory a `rafiki.toml` has to be discovered from.
    monkey_c_lsp::serve(global.config_source()).map_err(io::Error::other)?;

    Ok(true)
}

#[cfg(test)]
mod tests {
    use crate::cli::Cli;
    use clap::Parser;
    use monkey_c_config::ConfigSource;

    use std::path::PathBuf;

    fn source(args: &[&str]) -> ConfigSource {
        Cli::try_parse_from(args)
            .expect("arguments parse")
            .global
            .config_source()
    }

    #[test]
    fn the_workspace_is_searched_by_default() {
        assert_eq!(source(&["rafiki", "server"]), ConfigSource::Discover);
    }

    #[test]
    fn no_config_pins_the_defaults() {
        assert_eq!(
            source(&["rafiki", "server", "--no-config"]),
            ConfigSource::Defaults
        );
    }

    #[test]
    fn an_explicit_config_is_passed_through() {
        assert_eq!(
            source(&["rafiki", "server", "--config", "custom.toml"]),
            ConfigSource::File(PathBuf::from("custom.toml"))
        );
    }
}
