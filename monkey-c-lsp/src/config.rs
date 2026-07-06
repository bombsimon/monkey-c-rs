//! Server settings supplied by the client via `initializationOptions`.
//!
//! Keys are camelCase to match LSP/JSON convention (`lineWidth`, `alignment`,
//! `wrapDeclarations`). Unknown keys are ignored and missing keys fall back to
//! [`Settings::default`], so clients only set what they care about and old and
//! new clients interoperate.

use serde::Deserialize;

/// Formatter settings. Defaults match the `monkey-c-formatter` CLI, so
/// formatting through the LSP matches the standalone tool.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub struct Settings {
    pub line_width: usize,
    pub alignment: bool,
    pub wrap_declarations: bool,
}

impl Default for Settings {
    fn default() -> Self {
        Self {
            line_width: 111,
            alignment: true,
            wrap_declarations: false,
        }
    }
}

impl Settings {
    /// Read settings from the `initializationOptions` of the raw `initialize`
    /// params. Absent or malformed options yield defaults.
    pub fn from_initialize_params(params: &serde_json::Value) -> Self {
        params
            .get("initializationOptions")
            .and_then(|options| serde_json::from_value(options.clone()).ok())
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn defaults_match_the_formatter_cli() {
        let settings = Settings::default();
        assert_eq!(settings.line_width, 111);
        assert!(settings.alignment);
        assert!(!settings.wrap_declarations);
    }

    #[test]
    fn absent_initialization_options_yields_defaults() {
        assert_eq!(
            Settings::from_initialize_params(&json!({ "capabilities": {} })),
            Settings::default()
        );
    }

    #[test]
    fn partial_options_override_only_named_keys() {
        let params = json!({
            "initializationOptions": { "lineWidth": 80, "wrapDeclarations": true }
        });
        let settings = Settings::from_initialize_params(&params);
        assert_eq!(settings.line_width, 80);
        assert!(settings.wrap_declarations);
        // Unset key keeps its default.
        assert!(settings.alignment);
    }

    #[test]
    fn unknown_keys_are_ignored() {
        let params = json!({ "initializationOptions": { "future": 1, "lineWidth": 90 } });
        assert_eq!(Settings::from_initialize_params(&params).line_width, 90);
    }
}
