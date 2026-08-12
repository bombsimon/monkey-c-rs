use std::path::{Path, PathBuf};

use crate::{Config, Error};

/// Where a run's configuration comes from.
///
/// Naming the source instead of passing a resolved [`Config`] lets a caller that
/// does not yet know which directory to search decide the policy up front and
/// resolve it later — the language server learns its workspace root only once
/// the client has sent `initialize`, long after it was told how to configure
/// itself.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum ConfigSource {
    /// Walk up from the target looking for a [`FILE_NAME`](crate::FILE_NAME).
    #[default]
    Discover,

    /// Read exactly this file, and fail if it cannot be read.
    File(PathBuf),

    /// Use the built-in defaults and read no file at all.
    Defaults,
}

impl ConfigSource {
    /// The source described by a `--config PATH` / `--no-config` pair. The two
    /// are mutually exclusive on the command line, so `no_config` wins here only
    /// to keep the function total.
    pub fn from_flags(path: Option<PathBuf>, no_config: bool) -> Self {
        match (no_config, path) {
            (true, _) => Self::Defaults,
            (_, Some(path)) => Self::File(path),
            _ => Self::Discover,
        }
    }

    /// Resolve to a [`Config`], searching at or above `start` when discovering.
    ///
    /// Finding no file is not an error — it is the same as an empty one — but a
    /// file that exists and cannot be read or parsed is.
    pub fn resolve(&self, start: &Path) -> Result<Config, Error> {
        match self {
            Self::Defaults => Ok(Config::default()),
            Self::File(path) => Config::load(path),
            Self::Discover => Ok(Config::discover(start)?
                .map(|(_, config)| config)
                .unwrap_or_default()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use std::fs;

    #[test]
    fn no_config_beats_a_named_file() {
        let source = ConfigSource::from_flags(Some(PathBuf::from("x.toml")), true);
        assert_eq!(source, ConfigSource::Defaults);
    }

    #[test]
    fn a_named_file_is_used_when_given() {
        let source = ConfigSource::from_flags(Some(PathBuf::from("x.toml")), false);
        assert_eq!(source, ConfigSource::File(PathBuf::from("x.toml")));
    }

    #[test]
    fn neither_flag_discovers() {
        assert_eq!(
            ConfigSource::from_flags(None, false),
            ConfigSource::Discover
        );
    }

    #[test]
    fn defaults_read_no_file() {
        let dir = tempfile::tempdir().expect("temp dir");
        fs::write(
            dir.path().join(crate::FILE_NAME),
            "[format]\nline-width = 7\n",
        )
        .expect("write config");

        let config = ConfigSource::Defaults
            .resolve(dir.path())
            .expect("resolves");

        assert_eq!(
            config,
            Config::default(),
            "the neighbouring file is ignored"
        );
    }

    #[test]
    fn a_named_file_is_read() {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join("custom.toml");
        fs::write(&path, "[format]\nline-width = 42\n").expect("write config");

        let config = ConfigSource::File(path)
            .resolve(Path::new("."))
            .expect("resolves");

        assert_eq!(config.format.line_width, Some(42));
    }

    #[test]
    fn a_missing_named_file_is_an_error() {
        ConfigSource::File(PathBuf::from("definitely/not/here.toml"))
            .resolve(Path::new("."))
            .expect_err("a file the user named must exist");
    }

    #[test]
    fn discovery_walks_up_from_the_target() {
        let dir = tempfile::tempdir().expect("temp dir");
        fs::write(
            dir.path().join(crate::FILE_NAME),
            "[format]\nline-width = 7\n",
        )
        .expect("write config");

        let nested = dir.path().join("src").join("deep");
        fs::create_dir_all(&nested).expect("create dirs");

        let config = ConfigSource::Discover.resolve(&nested).expect("resolves");

        assert_eq!(config.format.line_width, Some(7));
    }

    #[test]
    fn discovering_nothing_yields_defaults() {
        let dir = tempfile::tempdir().expect("temp dir");

        let config = ConfigSource::Discover
            .resolve(dir.path())
            .expect("resolves");

        assert_eq!(config, Config::default());
    }

    #[test]
    fn a_discovered_file_that_is_broken_is_an_error() {
        let dir = tempfile::tempdir().expect("temp dir");
        fs::write(
            dir.path().join(crate::FILE_NAME),
            "[format]\nline-width = []\n",
        )
        .expect("write config");

        ConfigSource::Discover
            .resolve(dir.path())
            .expect_err("a file that exists must parse");
    }
}
