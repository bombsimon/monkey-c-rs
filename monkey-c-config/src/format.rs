use monkey_c_formatter::Formatter;
use monkey_c_formatter::options::{
    self, AnnotationSeparator, ModifierOrder, Nullable, ResourceRef, UnionStyle,
};
use serde::Deserialize;

/// Formatter settings with every value resolved. This is the single definition
/// of the formatter's defaults: the CLI, the language server and `rafiki.toml`
/// all start from here.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct FormatSettings {
    pub line_width: usize,
    pub alignment: bool,
    pub short_array_element_width: usize,
    pub union_style: UnionStyle,
    pub nullable: Nullable,
    pub annotation_separator: AnnotationSeparator,
    pub resource_ref: ResourceRef,
    pub modifier_order: ModifierOrder,
    pub parens: options::Parens,
    pub sort_imports: bool,
}

impl Default for FormatSettings {
    fn default() -> Self {
        Self {
            line_width: 111,
            alignment: true,
            short_array_element_width: 8,
            union_style: UnionStyle::default(),
            nullable: Nullable::default(),
            annotation_separator: AnnotationSeparator::default(),
            resource_ref: ResourceRef::default(),
            modifier_order: ModifierOrder::default(),
            parens: options::Parens::default(),
            sort_imports: true,
        }
    }
}

impl FormatSettings {
    /// A formatter for `source` with these settings, so the CLI and the language server can't
    /// format the same file differently.
    pub fn formatter(&self, source: &str) -> Formatter {
        Formatter::new(source)
            .with_line_width(self.line_width)
            .with_alignment(self.alignment)
            .with_short_array_element_width(self.short_array_element_width)
            .with_union_style(self.union_style)
            .with_nullable(self.nullable)
            .with_annotation_separator(self.annotation_separator)
            .with_resource_ref(self.resource_ref)
            .with_modifier_order(self.modifier_order)
            .with_parens(self.parens)
            .with_sort_imports(self.sort_imports)
    }

    /// Overlay the keys `config` sets, leaving the rest untouched. Apply layers
    /// in increasing order of priority — file first, then the explicit request.
    pub fn with_overrides(mut self, config: &FormatConfig) -> Self {
        if let Some(line_width) = config.line_width {
            self.line_width = line_width;
        }

        if let Some(alignment) = config.alignment {
            self.alignment = alignment;
        }

        if let Some(width) = config.short_array_element_width {
            self.short_array_element_width = width;
        }

        if let Some(union_style) = config.union_style {
            self.union_style = union_style;
        }

        if let Some(nullable) = config.nullable {
            self.nullable = nullable;
        }

        if let Some(separator) = config.annotation_separator {
            self.annotation_separator = separator;
        }

        if let Some(resource_ref) = config.resource_ref {
            self.resource_ref = resource_ref;
        }

        if let Some(modifier_order) = config.modifier_order {
            self.modifier_order = modifier_order;
        }

        if let Some(parens) = config.parens {
            self.parens = parens;
        }

        if let Some(sort_imports) = config.sort_imports {
            self.sort_imports = sort_imports;
        }

        self
    }
}

/// The `[format]` table. Also the shape of the CLI's formatting flags and of an
/// LSP client's `initializationOptions`, so all three overlay identically.
///
/// `exclude` only comes from the file, and is read through
/// [`PathRules`](crate::PathRules) since it depends on where the file is.
///
/// The rewrites only come from the file too. How code is spelled is a choice for the project
/// rather than for one run or one editor.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct FormatConfig {
    pub line_width: Option<usize>,
    pub alignment: Option<bool>,
    pub short_array_element_width: Option<usize>,
    pub union_style: Option<UnionStyle>,
    pub nullable: Option<Nullable>,
    pub annotation_separator: Option<AnnotationSeparator>,
    pub resource_ref: Option<ResourceRef>,
    pub modifier_order: Option<ModifierOrder>,
    pub parens: Option<options::Parens>,
    pub sort_imports: Option<bool>,
    pub exclude: Option<Vec<String>>,
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
            short_array_element_width: Some(4),
            ..FormatConfig::default()
        };
        let flags = FormatConfig {
            line_width: Some(100),
            alignment: None,
            short_array_element_width: Some(0),
            ..FormatConfig::default()
        };

        let settings = FormatSettings::default()
            .with_overrides(&file)
            .with_overrides(&flags);

        assert_eq!(settings.line_width, 100);
        assert_eq!(settings.short_array_element_width, 0);
        // Unset by the higher-priority layer, so the file's value survives.
        assert!(!settings.alignment);
    }

    #[test]
    fn a_rewrite_overlays_only_its_own_setting() {
        let settings = FormatSettings::default().with_overrides(&FormatConfig {
            nullable: Some(Nullable::Preserve),
            sort_imports: Some(false),
            ..FormatConfig::default()
        });

        assert_eq!(
            settings,
            FormatSettings {
                nullable: Nullable::Preserve,
                sort_imports: false,
                ..FormatSettings::default()
            }
        );
    }
}
