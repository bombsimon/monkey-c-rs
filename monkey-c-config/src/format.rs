use serde::Deserialize;

/// Formatter settings with every value resolved. This is the single definition
/// of the formatter's defaults: the CLI, the language server and `rafiki.toml`
/// all start from here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormatSettings {
    pub line_width: usize,
    pub alignment: bool,
    pub wrap_declarations: bool,
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self {
            line_width: 111,
            alignment: true,
            wrap_declarations: false,
        }
    }
}

impl FormatSettings {
    /// Overlay the keys `config` sets, leaving the rest untouched. Apply layers
    /// in increasing order of priority — file first, then the explicit request.
    pub fn with_overrides(mut self, config: &FormatConfig) -> Self {
        if let Some(line_width) = config.line_width {
            self.line_width = line_width;
        }

        if let Some(alignment) = config.alignment {
            self.alignment = alignment;
        }

        if let Some(wrap_declarations) = config.wrap_declarations {
            self.wrap_declarations = wrap_declarations;
        }

        self
    }
}

/// The `[format]` table. Also the shape of the CLI's formatting flags and of an
/// LSP client's `initializationOptions`, so all three overlay identically.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct FormatConfig {
    pub line_width: Option<usize>,
    pub alignment: Option<bool>,
    pub wrap_declarations: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_layer_overlays_only_the_keys_it_sets() {
        let settings = FormatSettings::default().with_overrides(&FormatConfig {
            line_width: Some(80),
            ..FormatConfig::default()
        });

        assert_eq!(settings.line_width, 80);
        assert_eq!(settings.alignment, FormatSettings::default().alignment);
    }

    #[test]
    fn the_last_layer_wins() {
        let file = FormatConfig {
            line_width: Some(80),
            alignment: Some(false),
            wrap_declarations: None,
        };
        let flags = FormatConfig {
            line_width: Some(100),
            alignment: None,
            wrap_declarations: Some(true),
        };

        let settings = FormatSettings::default()
            .with_overrides(&file)
            .with_overrides(&flags);

        assert_eq!(settings.line_width, 100);
        // Unset by the higher-priority layer, so the file's value survives.
        assert!(!settings.alignment);
        assert!(settings.wrap_declarations);
    }
}
