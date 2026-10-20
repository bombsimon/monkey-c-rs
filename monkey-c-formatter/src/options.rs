//! Values for the settings that respell code without changing what it means, like `|` to `or` in
//! a union type.
//!
//! Each one has a `Preserve` value to keep the code as written, since these are taste rather than
//! layout. Nearly all default to a canonical form, except where the written form is often
//! deliberate. A single spot can still be kept as written with
//! `// rafiki: fmt off`.

use serde::Deserialize;

/// The separator between the members of a union type. `or` is what Garmin's documentation and
/// nearly all real code use, and it can't be mistaken for a bitwise or.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum UnionStyle {
    #[default]
    Or,
    Pipe,
    Preserve,
}

/// How a type that may be `null` is written: `String?` or `String or Null`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Nullable {
    #[default]
    Shorthand,
    Union,
    Preserve,
}

/// The separator between the entries of an annotation, `(:a :b)` or `(:a, :b)`. Spaces are the
/// only documented form.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AnnotationSeparator {
    #[default]
    Space,
    Comma,
    Preserve,
}

/// Whether to drop the legacy `@` in front of a resource reference like `@Rez.Strings.name`.
///
/// There's no `@`-adding counterpart: a bare `Rez.Strings.name` is an ordinary member access, and
/// telling which of those are resources would take more than the syntax.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ResourceRef {
    #[default]
    Bare,
    Preserve,
}

/// The order of the visibility and `static` keywords. The compiler takes either, and visibility
/// first matches Java and C#.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ModifierOrder {
    #[default]
    VisibilityFirst,
    Preserve,
}

/// Whether to drop parentheses that can't change how the code is read, like those around a
/// returned value.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Parens {
    #[default]
    Drop,
    Preserve,
}

/// How number literals are written. Each part is its own setting since projects that agree on
/// one often disagree on the next, like `0xff` against `0xFF`.
///
/// Nothing here changes whether a literal has a `.`, an exponent or a suffix, since those decide
/// its type.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Numbers {
    pub hex_prefix_case: Case,
    pub hex_digit_case: Case,
    pub exponent_case: Case,
    pub exponent_padding: ExponentPadding,
    pub float_suffix_case: Case,
    pub double_suffix_case: Case,
    pub long_suffix_case: Case,
    pub leading_zero: LeadingZero,
    pub trailing_zeros: TrailingZeros,
}

/// Everything lowercase except hex digits, which match Garmin's documentation, and the long
/// suffix, since `10l` is easy to misread as `101`. That's the convention Java and Black use too.
impl Default for Numbers {
    fn default() -> Self {
        Self {
            hex_prefix_case: Case::Lower,
            hex_digit_case: Case::Upper,
            exponent_case: Case::Lower,
            exponent_padding: ExponentPadding::default(),
            float_suffix_case: Case::Lower,
            double_suffix_case: Case::Lower,
            long_suffix_case: Case::Upper,
            leading_zero: LeadingZero::default(),
            trailing_zeros: TrailingZeros::default(),
        }
    }
}

impl Numbers {
    /// Every part of a literal kept as written.
    pub fn preserve() -> Self {
        Self {
            hex_prefix_case: Case::Preserve,
            hex_digit_case: Case::Preserve,
            exponent_case: Case::Preserve,
            exponent_padding: ExponentPadding::Preserve,
            float_suffix_case: Case::Preserve,
            double_suffix_case: Case::Preserve,
            long_suffix_case: Case::Preserve,
            leading_zero: LeadingZero::Preserve,
            trailing_zeros: TrailingZeros::Preserve,
        }
    }
}

/// The casing of one part of a number literal. Which one is canonical depends on the part, so
/// there's no default here but in [`Numbers`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Case {
    Lower,
    Upper,
    Preserve,
}

/// Whether to drop the `+` and leading zeros of an exponent, like `1e+05` to `1e5`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ExponentPadding {
    #[default]
    Trim,
    Preserve,
}

/// Whether to write the `0` in front of a fraction like `.5`, which is easy to miss.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum LeadingZero {
    #[default]
    Add,
    Preserve,
}

/// Whether to drop zeros at the end of a fraction, like `1.50` to `1.5`. Kept by default, since
/// they're often there on purpose, like lining up a table of values by precision.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum TrailingZeros {
    Trim,
    #[default]
    Preserve,
}
