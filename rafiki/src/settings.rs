//! Resolving `rafiki.toml` for a run.
//!
//! Only the file layer lives here; each command overlays its own flags on top,
//! since which flags exist differs per command.

use monkey_c_config::{FilesSettings, PathMatcher, PathRules, ResolvedConfig};

use std::env;
use std::io;
use std::path::{Path, PathBuf};

use crate::cli::GlobalArgs;

/// The settings that depend on which file is checked, with `files` already resolved from
/// `loaded` and the command line. Globs in the file resolve against its directory, but once
/// `--exclude` has replaced the file's `[files]` excludes (see `FilesSettings::with_overrides`),
/// those resolve against the working directory.
pub fn path_rules(
    loaded: &ResolvedConfig,
    files: &FilesSettings,
    cli_excludes: &[String],
) -> io::Result<PathRules> {
    let files_exclude = if cli_excludes.is_empty() {
        PathMatcher::new(
            &loaded.directory(),
            &files.exclude,
            "files.exclude",
            loaded.path.as_deref(),
        )?
    } else {
        PathMatcher::new(&env::current_dir()?, &files.exclude, "--exclude", None)?
    };

    Ok(PathRules::new(loaded, files_exclude)?)
}

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
    use monkey_c_config::{Config, FilesConfig};

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

    #[test]
    fn cli_excludes_replace_the_files_and_resolve_against_the_working_directory() {
        let dir = tempfile::tempdir().expect("temp dir");
        let loaded = ResolvedConfig {
            config: toml_config("[files]\nexclude = [\"bin/**\"]\n"),
            path: Some(dir.path().join("rafiki.toml")),
        };
        let cwd = env::current_dir().expect("cwd");
        let resolve = |flags: &[String]| {
            let files = FilesSettings::default()
                .with_overrides(&loaded.config.files)
                .with_overrides(&FilesConfig {
                    exclude: (!flags.is_empty()).then(|| flags.to_vec()),
                    ..FilesConfig::default()
                });

            path_rules(&loaded, &files, flags).expect("resolves")
        };

        let from_file = resolve(&[]);
        assert!(!from_file.formats(&dir.path().join("bin/A.mc"), false));

        let from_flag = resolve(&["gen/**".to_string()]);
        assert!(from_flag.formats(&dir.path().join("bin/A.mc"), false));
        assert!(!from_flag.formats(&cwd.join("gen/A.mc"), false));
    }

    #[test]
    fn cli_excludes_get_around_an_invalid_glob_in_the_file() {
        let loaded = ResolvedConfig {
            config: toml_config("[files]\nexclude = [\"a{b\"]\n"),
            path: None,
        };
        let files = FilesSettings {
            exclude: vec!["gen/**".to_string()],
            ..FilesSettings::default()
        };

        path_rules(&loaded, &files, &files.exclude).expect("the file's globs are replaced");
    }

    #[test]
    fn an_invalid_glob_names_its_file_and_setting() {
        let loaded = ResolvedConfig {
            config: toml_config("[files]\nexclude = [\"a{b\"]\n"),
            path: Some(PathBuf::from("project/rafiki.toml")),
        };
        let files = FilesSettings::default().with_overrides(&loaded.config.files);

        let error = path_rules(&loaded, &files, &[]).expect_err("rejected");
        assert!(
            error
                .to_string()
                .starts_with("project/rafiki.toml: invalid glob `a{b` in `files.exclude`"),
            "{error}"
        );

        let error = path_rules(&loaded, &files, &["a{b".to_string()]).expect_err("rejected");
        assert!(
            error
                .to_string()
                .starts_with("invalid glob `a{b` in `--exclude`"),
            "{error}"
        );
    }

    fn toml_config(text: &str) -> Config {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("rafiki.toml");
        fs::write(&path, text).expect("write config");

        Config::load(&path).expect("parses")
    }
}
