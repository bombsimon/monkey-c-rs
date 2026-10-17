//! Resolving `rafiki.toml` for a run.
//!
//! Only the file layer lives here; each command overlays its own flags on top,
//! since which flags exist differs per command.

use monkey_c_config::ResolvedConfig;

use std::io;
use std::path::{Path, PathBuf};

use crate::cli::GlobalArgs;

/// Load the configuration for a run: the file named by `--config`, or the
/// nearest `rafiki.toml` at or above `target`, or the defaults.
///
/// Discovery starts from the first target rather than the working directory so
/// that `rafiki fmt some/project/File.mc` honours that project's settings. A run
/// spanning several projects resolves one configuration from the first of them,
/// which keeps a single invocation internally consistent.
pub fn load(global: &GlobalArgs, target: Option<&Path>) -> io::Result<ResolvedConfig> {
    Ok(global.config_source().resolve(&discovery_root(target))?)
}

/// Where to start walking up from. Stdin (`-`) has no location of its own, so
/// the working directory stands in for it.
fn discovery_root(target: Option<&Path>) -> PathBuf {
    match target {
        Some(path) if path != Path::new("-") => path.to_path_buf(),
        _ => PathBuf::from("."),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cli::{Cli, Command};
    use clap::Parser;
    use monkey_c_config::Config;

    use std::fs;

    fn global(args: &[&str]) -> GlobalArgs {
        Cli::try_parse_from(args).expect("arguments parse").global
    }

    #[test]
    fn no_config_yields_defaults() {
        let config = load(&global(&["rafiki", "fmt", "--no-config"]), None).expect("loads");
        assert_eq!(config.config, Config::default());
    }

    #[test]
    fn an_explicit_config_is_read() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("custom.toml");
        fs::write(&path, "[format]\nline-width = 42\n").expect("write config");

        let args = global(&["rafiki", "fmt", "--config", path.to_str().expect("utf-8")]);
        let config = load(&args, None).expect("loads");

        assert_eq!(config.config.format.line_width, Some(42));
    }

    #[test]
    fn a_missing_explicit_config_is_an_error() {
        let args = global(&["rafiki", "fmt", "--config", "definitely/not/here.toml"]);
        load(&args, None).expect_err("missing file is reported");
    }

    #[test]
    fn discovery_walks_up_from_the_target() {
        let dir = tempfile::tempdir().expect("temp dir");
        fs::write(dir.path().join("rafiki.toml"), "[format]\nline-width = 7\n")
            .expect("write config");

        let nested = dir.path().join("src").join("deep");
        fs::create_dir_all(&nested).expect("create dirs");
        let file = nested.join("File.mc");
        fs::write(&file, "").expect("write source");

        let config = load(&global(&["rafiki", "fmt"]), Some(&file)).expect("loads");

        assert_eq!(config.config.format.line_width, Some(7));
        assert_eq!(config.directory(), dir.path());
    }

    #[test]
    fn stdin_falls_back_to_the_working_directory() {
        assert_eq!(discovery_root(Some(Path::new("-"))), PathBuf::from("."));
        assert_eq!(discovery_root(None), PathBuf::from("."));
    }

    #[test]
    fn a_broken_config_is_an_error() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("rafiki.toml");
        fs::write(&path, "[format]\nline-width = \"wide\"\n").expect("write config");

        let args = global(&["rafiki", "fmt", "--config", path.to_str().expect("utf-8")]);
        let error = load(&args, None).expect_err("type mismatch is reported");

        assert!(error.to_string().contains("rafiki.toml"), "{error}");
    }

    #[test]
    fn the_parsed_command_is_unaffected_by_global_flags() {
        let cli = Cli::try_parse_from(["rafiki", "--no-config", "lint"]).expect("parses");
        assert!(matches!(cli.command, Command::Lint(_)));
    }
}
