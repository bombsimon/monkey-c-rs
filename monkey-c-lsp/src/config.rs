//! Where the server's settings come from.
//!
//! Three layers, lowest priority first: the built-in defaults, the workspace's
//! `rafiki.toml`, and the `initializationOptions` the client sent. That is the
//! same precedence the CLI applies to its flags, so a project's `rafiki.toml`
//! means the same thing whether a file is formatted from an editor or from a
//! terminal, while a client that deliberately sets a key still wins.
//!
//! `initializationOptions` keys are camelCase (`lineWidth`, `wrapDeclarations`)
//! to match LSP/JSON convention rather than the file's kebab-case. Unknown keys
//! are ignored and missing keys fall through, so old and new clients
//! interoperate.

use monkey_c_config::{Config, ConfigSource, FormatConfig, FormatSettings, LintSettings};
use monkey_c_linter::rules;
use serde::Deserialize;

use std::path::PathBuf;

use crate::uri;

/// The client's `initializationOptions`, which may set any formatter key.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(default, rename_all = "camelCase")]
struct InitializationOptions {
    line_width: Option<usize>,
    alignment: Option<bool>,
    wrap_declarations: Option<bool>,
}

/// Settings used throughout one language-server session.
#[derive(Debug, PartialEq, Eq)]
pub struct Settings {
    pub format: FormatSettings,
    pub lint: LintSettings,
}

impl InitializationOptions {
    fn to_config(self) -> FormatConfig {
        FormatConfig {
            line_width: self.line_width,
            alignment: self.alignment,
            wrap_declarations: self.wrap_declarations,
        }
    }
}

/// Resolve the formatter settings for a session from the raw `initialize`
/// params. Configuration problems are reported to the caller as a message to
/// surface rather than as a failure: a malformed `rafiki.toml` should leave a
/// usable editor session, not a dead language server.
pub fn resolve(source: &ConfigSource, params: &serde_json::Value) -> (Settings, Vec<String>) {
    let mut warnings = Vec::new();

    let file = match file_config(source, params) {
        Ok(config) => config,
        Err(message) => {
            warnings.push(message);

            Config::default()
        }
    };

    let options: InitializationOptions = params
        .get("initializationOptions")
        .and_then(|options| serde_json::from_value(options.clone()).ok())
        .unwrap_or_default();

    let format = FormatSettings::default()
        .with_overrides(&file.format)
        .with_overrides(&options.to_config());
    let mut lint = LintSettings::default().with_overrides(&file.lint);

    let unknown: Vec<_> = lint
        .named_rules()
        .filter(|name| !rules::ALL.contains(name))
        .collect();
    if !unknown.is_empty() {
        warnings.push(format!(
            "unknown lint rules in rafiki.toml: {}",
            unknown.join(", ")
        ));
        lint = LintSettings::default();
    }

    (Settings { format, lint }, warnings)
}

/// The `[format]` table `source` resolves to, searching from the workspace root
/// the client reported.
///
/// A session with no workspace root has nothing to search: the server's working
/// directory belongs to whoever spawned the editor and may have no relation to
/// the code being edited, so discovering from there could silently apply a
/// stranger's settings.
fn file_config(source: &ConfigSource, params: &serde_json::Value) -> Result<Config, String> {
    let root = match workspace_root(params) {
        Some(root) => root,
        None if *source == ConfigSource::Discover => return Ok(Config::default()),
        None => PathBuf::new(),
    };

    source.resolve(&root).map_err(|error| error.to_string())
}

/// The workspace root as a filesystem path. `workspaceFolders` is the current
/// spelling and `rootUri` the deprecated one; clients still send either.
fn workspace_root(params: &serde_json::Value) -> Option<PathBuf> {
    let folder = params
        .get("workspaceFolders")
        .and_then(|folders| folders.as_array())
        .and_then(|folders| folders.first())
        .and_then(|folder| folder.get("uri"))
        .and_then(|uri| uri.as_str());

    let root = params.get("rootUri").and_then(|uri| uri.as_str());

    folder.or(root).and_then(uri::to_path)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    use std::fs;

    /// A directory holding a `rafiki.toml` with `body`, plus that file's path.
    fn project(body: &str) -> (tempfile::TempDir, PathBuf) {
        let dir = tempfile::tempdir().expect("temp dir");
        let path = dir.path().join(monkey_c_config::FILE_NAME);
        fs::write(&path, body).expect("write config");

        (dir, path)
    }

    /// `initialize` params naming `root` as the workspace root.
    fn params_rooted_at(root: &std::path::Path) -> serde_json::Value {
        json!({ "rootUri": format!("file://{}", root.display()) })
    }

    #[test]
    fn defaults_apply_when_the_client_sends_nothing() {
        let (settings, warnings) = resolve(&ConfigSource::Defaults, &json!({ "capabilities": {} }));

        assert_eq!(settings.format, FormatSettings::default());
        assert!(warnings.is_empty());
    }

    #[test]
    fn initialization_options_override_only_named_keys() {
        let params = json!({
            "initializationOptions": { "lineWidth": 80, "wrapDeclarations": true }
        });
        let (settings, _) = resolve(&ConfigSource::Defaults, &params);

        assert_eq!(settings.format.line_width, 80);
        assert!(settings.format.wrap_declarations);
        // Unset key keeps its default.
        assert!(settings.format.alignment);
    }

    #[test]
    fn unknown_options_are_ignored() {
        let params = json!({ "initializationOptions": { "future": 1, "lineWidth": 90 } });
        let (settings, _) = resolve(&ConfigSource::Defaults, &params);

        assert_eq!(settings.format.line_width, 90);
    }

    #[test]
    fn a_pinned_file_supplies_the_file_layer() {
        let (_dir, path) = project("[format]\nline-width = 60\n");
        let (settings, warnings) = resolve(&ConfigSource::File(path), &json!({}));

        assert_eq!(settings.format.line_width, 60);
        assert!(warnings.is_empty());
    }

    #[test]
    fn a_pinned_file_is_read_even_without_a_workspace_root() {
        // The CLI's `--config` must win whether or not the editor reports a root.
        let (_dir, path) = project("[format]\nline-width = 24\n");
        let (settings, _) = resolve(&ConfigSource::File(path), &json!({ "rootUri": null }));

        assert_eq!(settings.format.line_width, 24);
    }

    #[test]
    fn initialization_options_beat_the_file() {
        let (_dir, path) = project("[format]\nline-width = 60\nalignment = false\n");
        let params = json!({ "initializationOptions": { "lineWidth": 120 } });
        let (settings, _) = resolve(&ConfigSource::File(path), &params);

        assert_eq!(settings.format.line_width, 120);
        // The file still supplies what the client did not set.
        assert!(!settings.format.alignment);
    }

    #[test]
    fn lint_selection_is_loaded_from_the_file() {
        let (_dir, path) = project("[lint]\ndisable = [\"naming-convention\"]\n");
        let (settings, warnings) = resolve(&ConfigSource::File(path), &json!({}));

        assert!(!settings.lint.selects("naming-convention"));
        assert!(settings.lint.selects("import-order"));
        assert!(warnings.is_empty());
    }

    #[test]
    fn unknown_lint_rules_warn_and_fall_back_to_defaults() {
        let (_dir, path) = project("[lint]\nenable = [\"not-a-rule\"]\n");
        let (settings, warnings) = resolve(&ConfigSource::File(path), &json!({}));

        assert!(settings.lint.selects("naming-convention"));
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("not-a-rule"));
    }

    #[test]
    fn the_workspace_root_is_searched_for_a_config() {
        let (dir, _path) = project("[format]\nline-width = 55\n");
        let (settings, warnings) = resolve(&ConfigSource::Discover, &params_rooted_at(dir.path()));

        assert_eq!(settings.format.line_width, 55);
        assert!(warnings.is_empty());
    }

    #[test]
    fn workspace_folders_take_precedence_over_the_deprecated_root_uri() {
        let dir = tempfile::tempdir().expect("temp dir");
        let folder = dir.path().join("folder");
        fs::create_dir(&folder).expect("create folder");
        fs::write(
            folder.join(monkey_c_config::FILE_NAME),
            "[format]\nline-width = 33\n",
        )
        .expect("write config");

        let params = json!({
            "workspaceFolders": [{ "uri": format!("file://{}", folder.display()) }],
            "rootUri": "file:///nowhere",
        });
        let (settings, _) = resolve(&ConfigSource::Discover, &params);

        assert_eq!(settings.format.line_width, 33);
    }

    #[test]
    fn a_broken_config_warns_and_falls_back_to_defaults() {
        let (dir, _path) = project("[format]\nline-width = true\n");
        let (settings, warnings) = resolve(&ConfigSource::Discover, &params_rooted_at(dir.path()));

        assert_eq!(settings.format, FormatSettings::default());
        assert_eq!(warnings.len(), 1, "the client is told why");
        assert!(
            warnings[0].contains(monkey_c_config::FILE_NAME),
            "{:?}",
            warnings[0]
        );
    }

    #[test]
    fn no_workspace_root_is_not_an_error() {
        let (settings, warnings) = resolve(&ConfigSource::Discover, &json!({ "rootUri": null }));

        assert_eq!(settings.format, FormatSettings::default());
        assert!(warnings.is_empty(), "nothing to search is not a problem");
    }
}
