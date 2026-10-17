//! Where the server's settings come from.
//!
//! Three layers, lowest priority first: the built-in defaults, the workspace's
//! `rafiki.toml`, and the `initializationOptions` the client sent. That is the
//! same precedence the CLI applies to its flags, so a project's `rafiki.toml`
//! means the same thing whether a file is formatted from an editor or from a
//! terminal, while a client that deliberately sets a key still wins.
//!
//! `initializationOptions` keys are camelCase (`lineWidth`, `alignment`)
//! to match LSP/JSON convention rather than the file's kebab-case. Unknown keys
//! are ignored and missing keys fall through, so old and new clients
//! interoperate.

use gen_lsp_types::Uri;
use monkey_c_config::{
    ConfigSource, FormatConfig, FormatSettings, LintSettings, PathRules, ResolvedConfig,
};
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
    short_array_element_width: Option<usize>,
}

/// Settings used throughout one language-server session.
#[derive(Debug, Default)]
pub struct Settings {
    pub format: FormatSettings,
    pub lint: LintSettings,
    path_rules: PathRules,
}

/// Documents without a path, like an untitled buffer, are never excluded and get no
/// `per-file-ignores`, the same as stdin on the command line.
impl Settings {
    pub fn formats(&self, uri: &Uri) -> bool {
        uri::to_path(&uri.0).is_none_or(|path| self.path_rules.formats(&path, false))
    }

    pub fn lints(&self, uri: &Uri) -> bool {
        uri::to_path(&uri.0).is_none_or(|path| self.path_rules.lints(&path, false))
    }

    /// Which rules run for the document at `uri`.
    pub fn rule_filter(&self, uri: &Uri) -> impl Fn(&str) -> bool + '_ {
        let ignored = uri::to_path(&uri.0)
            .map(|path| self.path_rules.ignored_rules(&path))
            .unwrap_or_default();

        move |rule| self.lint.selects(rule) && !ignored.contains(&rule)
    }
}

impl InitializationOptions {
    fn to_config(self) -> FormatConfig {
        FormatConfig {
            line_width: self.line_width,
            alignment: self.alignment,
            short_array_element_width: self.short_array_element_width,
            ..FormatConfig::default()
        }
    }
}

/// Resolve the formatter settings for a session from the raw `initialize`
/// params. Configuration problems are reported to the caller as a message to
/// surface rather than as a failure: a malformed `rafiki.toml` should leave a
/// usable editor session, not a dead language server.
pub fn resolve(source: &ConfigSource, params: &serde_json::Value) -> (Settings, Vec<String>) {
    let mut warnings = Vec::new();

    let mut resolved = match file_config(source, params) {
        Ok(resolved) => resolved,
        Err(message) => {
            warnings.push(message);

            ResolvedConfig::default()
        }
    };

    let options: InitializationOptions = params
        .get("initializationOptions")
        .and_then(|options| serde_json::from_value(options.clone()).ok())
        .unwrap_or_default();

    let format = FormatSettings::default()
        .with_overrides(&resolved.config.format)
        .with_overrides(&options.to_config());
    let mut lint = LintSettings::default().with_overrides(&resolved.config.lint);

    let unknown: Vec<_> = lint
        .named_rules()
        .chain(resolved.config.lint.per_file_rules())
        .filter(|name| !rules::exists(name))
        .map(str::to_string)
        .collect();
    // Only the unknown names are dropped, so one typo doesn't turn back on every rule the
    // project disabled.
    if !unknown.is_empty() {
        warnings.push(format!(
            "unknown lint rules in rafiki.toml: {}",
            unknown.join(", ")
        ));
        lint.enable.retain(|name| rules::exists(name));
        lint.disable.retain(|name| rules::exists(name));

        for listed in resolved
            .config
            .lint
            .per_file_ignores
            .iter_mut()
            .flat_map(|map| map.values_mut())
        {
            listed.retain(|name| rules::exists(name));
        }
    }

    let (path_rules, glob_errors) = PathRules::lenient(&resolved, &[]);
    warnings.extend(glob_errors.iter().map(ToString::to_string));

    let settings = Settings {
        format,
        lint,
        path_rules,
    };

    (settings, warnings)
}

/// The configuration `source` resolves to, searching from the workspace root
/// the client reported.
///
/// A session with no workspace root has nothing to search: the server's working
/// directory belongs to whoever spawned the editor and may have no relation to
/// the code being edited, so discovering from there could silently apply a
/// stranger's settings.
fn file_config(
    source: &ConfigSource,
    params: &serde_json::Value,
) -> Result<ResolvedConfig, String> {
    let root = match workspace_root(params) {
        Some(root) => root,
        None if *source == ConfigSource::Discover => return Ok(ResolvedConfig::default()),
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
            "initializationOptions": { "lineWidth": 80 }
        });
        let (settings, _) = resolve(&ConfigSource::Defaults, &params);

        assert_eq!(settings.format.line_width, 80);
        // Unset key keeps its default.
        assert!(settings.format.alignment);
    }

    #[test]
    fn short_array_element_width_is_read_as_camel_case() {
        let params = json!({ "initializationOptions": { "shortArrayElementWidth": 0 } });
        let (settings, _) = resolve(&ConfigSource::Defaults, &params);

        assert_eq!(settings.format.short_array_element_width, 0);
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
    fn unknown_lint_rules_warn_and_are_dropped() {
        let (_dir, path) = project("[lint]\nenable = [\"not-a-rule\"]\n");
        let (settings, warnings) = resolve(&ConfigSource::File(path), &json!({}));

        assert!(settings.lint.selects("naming-convention"));
        assert_eq!(warnings.len(), 1);
        assert!(warnings[0].contains("not-a-rule"));
    }

    #[test]
    fn an_unknown_rule_keeps_the_rest_of_the_selection() {
        let (dir, _path) = project(
            r#"
[lint]
disable = ["naming-convention"]

[lint.per-file-ignores]
"tests/**" = ["naming-conventon", "import-order"]
"#,
        );
        let (settings, warnings) = resolve(&ConfigSource::Discover, &params_rooted_at(dir.path()));
        let test_file = settings.rule_filter(&uri(&dir.path().join("tests/A.mc")));

        assert_eq!(warnings.len(), 1);
        assert!(!settings.lint.selects("naming-convention"));
        assert!(!test_file("import-order"));
    }

    fn uri(path: &std::path::Path) -> Uri {
        Uri(format!("file://{}", path.display()))
    }

    #[test]
    fn excluded_documents_are_neither_formatted_nor_linted() {
        let (dir, _path) = project(
            r#"
[files]
exclude = ["gen/**"]

[format]
exclude = ["tables/**"]

[lint]
exclude = ["vendor/**"]
"#,
        );
        let (settings, warnings) = resolve(&ConfigSource::Discover, &params_rooted_at(dir.path()));
        let root = dir.path();

        assert!(warnings.is_empty(), "{warnings:?}");
        assert!(!settings.formats(&uri(&root.join("gen/A.mc"))));
        assert!(!settings.lints(&uri(&root.join("gen/A.mc"))));
        assert!(!settings.formats(&uri(&root.join("tables/A.mc"))));
        assert!(settings.lints(&uri(&root.join("tables/A.mc"))));
        assert!(settings.formats(&uri(&root.join("vendor/A.mc"))));
        assert!(!settings.lints(&uri(&root.join("vendor/A.mc"))));
        assert!(settings.formats(&Uri("untitled:Untitled-1".to_string())));
    }

    #[test]
    fn an_invalid_glob_warns_and_keeps_the_other_excludes() {
        let (dir, _path) = project(
            r#"
[files]
exclude = ["gen/**"]

[lint]
exclude = ["a{b"]
"#,
        );
        let (settings, warnings) = resolve(&ConfigSource::Discover, &params_rooted_at(dir.path()));

        assert_eq!(warnings.len(), 1);
        assert!(
            warnings[0].contains("rafiki.toml: invalid glob `a{b` in `lint.exclude`"),
            "{warnings:?}"
        );
        assert!(!settings.formats(&uri(&dir.path().join("gen/A.mc"))));
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
