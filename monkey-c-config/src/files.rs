use serde::Deserialize;

/// Which files a directory walk yields, resolved. The excludes are resolved by
/// [`PathRules`](crate::PathRules) instead, since they depend on where they're
/// set.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FilesSettings {
    pub respect_gitignore: bool,
}

impl Default for FilesSettings {
    fn default() -> Self {
        Self {
            respect_gitignore: true,
        }
    }
}

impl FilesSettings {
    /// Overlay the keys `config` sets.
    pub fn with_overrides(mut self, config: &FilesConfig) -> Self {
        if let Some(respect_gitignore) = config.respect_gitignore {
            self.respect_gitignore = respect_gitignore;
        }

        self
    }
}

/// The `[files]` table. `exclude` holds gitignore-syntax globs relative to the
/// configuration file's directory.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct FilesConfig {
    pub exclude: Option<Vec<String>>,
    pub respect_gitignore: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_layer_overlays_only_the_keys_it_sets() {
        let settings = FilesSettings::default().with_overrides(&FilesConfig {
            respect_gitignore: Some(false),
            ..FilesConfig::default()
        });

        assert!(!settings.respect_gitignore);
    }
}
