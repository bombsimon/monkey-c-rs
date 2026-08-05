use serde::Deserialize;

/// Which lint rules run, resolved. `enable` empty means "every rule"; a
/// non-empty `enable` is an allowlist. `disable` always subtracts, so a rule
/// named in both is silenced.
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct LintSettings {
    pub enable: Vec<String>,
    pub disable: Vec<String>,
}

impl LintSettings {
    /// Overlay the keys `config` sets. A list given by a higher-priority layer
    /// replaces the lower one rather than extending it, so a CLI `--disable`
    /// gives the exact set the user asked for instead of the file's plus theirs.
    pub fn with_overrides(mut self, config: &LintConfig) -> Self {
        if let Some(enable) = &config.enable {
            self.enable = enable.clone();
        }

        if let Some(disable) = &config.disable {
            self.disable = disable.clone();
        }

        self
    }

    /// Whether `rule` runs under these settings.
    pub fn selects(&self, rule: &str) -> bool {
        let enabled = self.enable.is_empty() || self.enable.iter().any(|name| name == rule);
        let disabled = self.disable.iter().any(|name| name == rule);

        enabled && !disabled
    }

    /// Every rule name mentioned, for validation against the linter's real rule
    /// list — a misspelled name in `enable` would otherwise silently silence
    /// every rule.
    pub fn named_rules(&self) -> impl Iterator<Item = &str> {
        self.enable
            .iter()
            .chain(self.disable.iter())
            .map(String::as_str)
    }
}

/// The `[lint]` table, and the shape of the CLI's `--enable`/`--disable`.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct LintConfig {
    pub enable: Option<Vec<String>>,
    pub disable: Option<Vec<String>>,
}

impl LintConfig {
    /// A layer from CLI list flags, where "not given" is an empty list rather
    /// than `None`. Empty lists must not overlay, or passing neither flag would
    /// wipe the file's selection.
    pub fn from_flags(enable: &[String], disable: &[String]) -> Self {
        Self {
            enable: (!enable.is_empty()).then(|| enable.to_vec()),
            disable: (!disable.is_empty()).then(|| disable.to_vec()),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn settings(enable: &[&str], disable: &[&str]) -> LintSettings {
        LintSettings {
            enable: enable.iter().map(|s| s.to_string()).collect(),
            disable: disable.iter().map(|s| s.to_string()).collect(),
        }
    }

    #[test]
    fn no_selection_runs_every_rule() {
        assert!(settings(&[], &[]).selects("import-order"));
    }

    #[test]
    fn enable_is_an_allowlist() {
        let settings = settings(&["import-order"], &[]);
        assert!(settings.selects("import-order"));
        assert!(!settings.selects("naming-convention"));
    }

    #[test]
    fn disable_subtracts() {
        let settings = settings(&[], &["import-order"]);
        assert!(!settings.selects("import-order"));
        assert!(settings.selects("naming-convention"));
    }

    #[test]
    fn disable_wins_over_enable() {
        let settings = settings(&["import-order", "naming-convention"], &["import-order"]);
        assert!(!settings.selects("import-order"));
        assert!(settings.selects("naming-convention"));
    }

    #[test]
    fn a_flag_list_replaces_the_files_list() {
        let file = LintConfig {
            disable: Some(vec!["import-order".to_string()]),
            ..LintConfig::default()
        };
        let flags = LintConfig::from_flags(&[], &["naming-convention".to_string()]);

        let resolved = LintSettings::default()
            .with_overrides(&file)
            .with_overrides(&flags);

        assert!(resolved.selects("import-order"));
        assert!(!resolved.selects("naming-convention"));
    }

    #[test]
    fn absent_flags_keep_the_files_selection() {
        let file = LintConfig {
            disable: Some(vec!["import-order".to_string()]),
            ..LintConfig::default()
        };

        let resolved = LintSettings::default()
            .with_overrides(&file)
            .with_overrides(&LintConfig::from_flags(&[], &[]));

        assert!(!resolved.selects("import-order"));
    }

    #[test]
    fn named_rules_covers_both_lists() {
        let settings = settings(&["a"], &["b"]);
        assert_eq!(settings.named_rules().collect::<Vec<_>>(), vec!["a", "b"]);
    }
}
