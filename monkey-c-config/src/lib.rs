//! `rafiki.toml`, the project configuration shared by the `rafiki` CLI and the
//! language server.
//!
//! Every key is optional so that a value can fall through a precedence chain:
//! an explicit request (a CLI flag, or an LSP client's
//! `initializationOptions`) beats the file, and the file beats the built-in
//! defaults. That is expressed by starting from a resolved `*Settings` value
//! and overlaying `*Config` layers onto it in increasing order of priority.
//!
//! Keys are kebab-case to match `Cargo.toml` and the CLI's own flag spelling,
//! and unknown keys are rejected so a typo is reported instead of silently
//! ignored.

mod error;
mod files;
mod format;
mod lint;
mod source;

pub use crate::error::Error;
pub use crate::files::{FilesConfig, FilesSettings};
pub use crate::format::{FormatConfig, FormatSettings};
pub use crate::lint::{LintConfig, LintSettings};
pub use crate::source::ConfigSource;

use serde::Deserialize;

use std::fs;
use std::path::{Path, PathBuf};

/// The configuration file name looked for when walking up from a target path.
pub const FILE_NAME: &str = "rafiki.toml";

/// A parsed `rafiki.toml`. Absent tables behave exactly like empty ones, so a
/// file that only sets `[format]` leaves lint and file selection at their
/// defaults.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct Config {
    #[serde(default)]
    pub format: FormatConfig,

    #[serde(default)]
    pub lint: LintConfig,

    #[serde(default)]
    pub files: FilesConfig,
}

impl Config {
    /// Parse `rafiki.toml` from `path`.
    pub fn load(path: &Path) -> Result<Self, Error> {
        let text = fs::read_to_string(path).map_err(|source| Error::Read {
            path: path.to_path_buf(),
            source,
        })?;

        toml::from_str(&text).map_err(|source| Error::Parse {
            path: path.to_path_buf(),
            source: Box::new(source),
        })
    }

    /// Find and parse the nearest `rafiki.toml` at or above `start`, returning
    /// the file's path alongside it. `start` may be a file or a directory.
    ///
    /// Walking up (rather than requiring the file in the current directory)
    /// means `rafiki fmt src/Foo.mc` from anywhere inside a project picks up the
    /// project's settings, which is also what the language server needs when
    /// the editor's working directory is unrelated to the workspace.
    pub fn discover(start: &Path) -> Result<Option<(PathBuf, Self)>, Error> {
        let Some(path) = find_config_file(start) else {
            return Ok(None);
        };

        let config = Self::load(&path)?;

        Ok(Some((path, config)))
    }
}

/// The nearest existing `rafiki.toml` at or above `start`.
fn find_config_file(start: &Path) -> Option<PathBuf> {
    let start = if start.is_dir() {
        start
    } else {
        start.parent()?
    };

    start.ancestors().find_map(|dir| {
        let candidate = dir.join(FILE_NAME);
        candidate.is_file().then_some(candidate)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn an_empty_file_is_all_defaults() {
        let config: Config = toml::from_str("").expect("empty config parses");
        assert_eq!(config, Config::default());
    }

    #[test]
    fn tables_are_parsed_independently() {
        let config: Config = toml::from_str(
            r#"
            [format]
            line-width = 80

            [lint]
            disable = ["import-order"]
            "#,
        )
        .expect("config parses");

        assert_eq!(config.format.line_width, Some(80));
        assert_eq!(config.lint.disable, Some(vec!["import-order".to_string()]));
        assert_eq!(config.files, FilesConfig::default());
    }

    #[test]
    fn an_unknown_key_is_an_error() {
        let error = toml::from_str::<Config>("[format]\nline_width = 80\n")
            .expect_err("snake_case key is rejected");
        assert!(error.to_string().contains("line_width"), "{error}");
    }

    #[test]
    fn an_unknown_table_is_an_error() {
        toml::from_str::<Config>("[formatter]\n").expect_err("unknown table is rejected");
    }
}
