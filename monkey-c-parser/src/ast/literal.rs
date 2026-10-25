/// A 32-bit floating point literal, kept in the shape it was written (`0f`, `0.5`, `.978`,
/// `6371e3`) rather than parsed, since parsing to `f32` would lose precision for no gain.
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Float.html>
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    /// The digits before any exponent or suffix.
    pub digits: String,
    pub has_dot: bool,
    /// Written without the zero before the `.`, as in `.5`.
    pub leading_dot: bool,
    pub suffix: Option<char>,
    /// The exponent as written, like `e3` or `E-2`.
    pub exponent: Option<String>,
}

/// A 64-bit floating point literal. Unlike [`FloatLit`] the `d` suffix is required.
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Double.html>
#[derive(Debug, Clone, PartialEq)]
pub struct DoubleLit {
    /// The digits before any exponent or suffix.
    pub digits: String,
    pub has_dot: bool,
    pub leading_dot: bool,
    pub exponent: Option<String>,
    pub suffix: char,
}

/// A compile-time constant value. Numbers, strings and chars keep their raw source text, escapes
/// and casing included, so the formatter controls exactly how they are written back.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#data-types>
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    /// A 32-bit integer, e.g. `42`.
    Number(String),
    /// A 64-bit integer, e.g. `42l`, suffix included.
    Long(String),
    /// A 32-bit hex integer, e.g. `0xFF`, prefix included.
    Hex(String),
    /// A 64-bit hex integer, e.g. `0xFFl`, prefix and suffix included.
    HexLong(String),
    /// A 32-bit float, e.g. `1.5`, `2f` or `1e3`.
    Float(FloatLit),
    /// A 64-bit float, e.g. `1.5d`.
    Double(DoubleLit),
    /// A string, e.g. `"text"`, without the quotes and with escapes as written.
    String(String),
    /// A character, e.g. `'c'`, without the quotes and with escapes as written.
    Char(String),
    Boolean(bool),
    /// A symbol, e.g. `:name`, without the `:`.
    Symbol(String),
    Null,
    NaN,
}
