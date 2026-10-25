use monkey_c_formatter::Formatter;
use monkey_c_formatter::options::{
    self, AnnotationSeparator, Case, ExponentPadding, LeadingZero, ModifierOrder, Nullable,
    Numbers, ResourceRef, TrailingZeros, UnionStyle,
};
use serde::Deserialize;

/// Formatter settings with every value resolved, and the one place the defaults the CLI and the
/// language server start from are defined.
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
    pub numbers: Numbers,
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
            numbers: Numbers::default(),
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
            .with_numbers(self.numbers)
    }

    /// Overlay the keys `config` sets, leaving the rest untouched. Apply layers
    /// in increasing order of priority — file first, then the explicit request.
    pub fn with_overrides(self, config: &FormatConfig) -> Self {
        Self {
            line_width: config.line_width.unwrap_or(self.line_width),
            alignment: config.alignment.unwrap_or(self.alignment),
            short_array_element_width: config
                .short_array_element_width
                .unwrap_or(self.short_array_element_width),
            union_style: config.union_style.unwrap_or(self.union_style),
            nullable: config.nullable.unwrap_or(self.nullable),
            annotation_separator: config
                .annotation_separator
                .unwrap_or(self.annotation_separator),
            resource_ref: config.resource_ref.unwrap_or(self.resource_ref),
            modifier_order: config.modifier_order.unwrap_or(self.modifier_order),
            parens: config.parens.unwrap_or(self.parens),
            sort_imports: config.sort_imports.unwrap_or(self.sort_imports),
            numbers: config.numbers.overlay(self.numbers),
        }
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

    #[serde(default)]
    pub numbers: NumbersConfig,
}

/// The `[format.numbers]` table. A table of its own since every part of a literal has a setting,
/// and the names read better without a `number-` prefix on each.
#[derive(Debug, Clone, Default, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields, rename_all = "kebab-case")]
pub struct NumbersConfig {
    pub hex_prefix_case: Option<Case>,
    pub hex_digit_case: Option<Case>,
    pub exponent_case: Option<Case>,
    pub exponent_padding: Option<ExponentPadding>,
    pub float_suffix_case: Option<Case>,
    pub double_suffix_case: Option<Case>,
    pub long_suffix_case: Option<Case>,
    pub leading_zero: Option<LeadingZero>,
    pub trailing_zeros: Option<TrailingZeros>,
}

impl NumbersConfig {
    fn overlay(&self, numbers: Numbers) -> Numbers {
        Numbers {
            hex_prefix_case: self.hex_prefix_case.unwrap_or(numbers.hex_prefix_case),
            hex_digit_case: self.hex_digit_case.unwrap_or(numbers.hex_digit_case),
            exponent_case: self.exponent_case.unwrap_or(numbers.exponent_case),
            exponent_padding: self.exponent_padding.unwrap_or(numbers.exponent_padding),
            float_suffix_case: self.float_suffix_case.unwrap_or(numbers.float_suffix_case),
            double_suffix_case: self
                .double_suffix_case
                .unwrap_or(numbers.double_suffix_case),
            long_suffix_case: self.long_suffix_case.unwrap_or(numbers.long_suffix_case),
            leading_zero: self.leading_zero.unwrap_or(numbers.leading_zero),
            trailing_zeros: self.trailing_zeros.unwrap_or(numbers.trailing_zeros),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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

    #[test]
    fn a_number_setting_overlays_only_itself() {
        let config: FormatConfig = toml::from_str(
            r#"
            [numbers]
            hex-digit-case = "lower"
            trailing-zeros = "trim"
            "#,
        )
        .expect("numbers table parses");

        let settings = FormatSettings::default().with_overrides(&config);

        assert_eq!(
            settings.numbers,
            Numbers {
                hex_digit_case: Case::Lower,
                trailing_zeros: TrailingZeros::Trim,
                ..Numbers::default()
            }
        );
    }
}
