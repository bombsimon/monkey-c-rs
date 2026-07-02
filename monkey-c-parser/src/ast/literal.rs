/// A 32-bit floating point literal together with the source-form flags
/// needed to round-trip it. The lexer captures `digits` verbatim from the
/// source (rather than parsing it to `f32`, which would lose precision and
/// is unnecessary for re-emitting it); flags capture which surface form the
/// user wrote so the formatter can re-emit the same shape (`0f`, `0.5`,
/// `0.5f`, `.978`, `.5f`, `6371e3`).
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Float.html>
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    /// The digits as written in source, before any exponent or `f`/`F`
    /// suffix (`0`, `0.5`, `.978`, `6371`).
    pub digits: String,
    /// Source contained a `.` (`0.5`, `.978`). False for integer-form
    /// literals that gain Float-ness only through the `f` suffix (`0f`) or
    /// an exponent (`6371e3`).
    pub has_dot: bool,
    /// Source omitted the leading zero (`.5`, `.5f`). Implies `has_dot`.
    pub leading_dot: bool,
    /// Source had an explicit `f` suffix.
    pub has_suffix: bool,
    /// Exponent part from scientific notation, e.g. `"e3"`, `"E-2"`, `"e+10"`.
    /// `None` for literals without an exponent.
    pub exponent: Option<String>,
}

/// A 64-bit floating point literal. Unlike [`FloatLit`], the `d` suffix is
/// always required in source, so there is no `has_suffix` flag.
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Double.html>
#[derive(Debug, Clone, PartialEq)]
pub struct DoubleLit {
    /// The digits as written in source, before any exponent or `d`/`D`
    /// suffix (`78`, `78.0`, `.5`, `6371`). See [`FloatLit::digits`].
    pub digits: String,
    /// Source contained a `.` (`78.0d`). False for integer-form Double
    /// literals (`78d`) or exponent-only form (`6371e3d`).
    pub has_dot: bool,
    /// Source omitted the leading zero (`.5d`). Implies `has_dot`.
    pub leading_dot: bool,
    /// Exponent part from scientific notation, e.g. `"e3"`, `"E-2"`.
    /// `None` for literals without an exponent.
    pub exponent: Option<String>,
}

impl std::fmt::Display for FloatLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.digits,
            self.exponent.as_deref().unwrap_or("")
        )
    }
}

impl std::fmt::Display for DoubleLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}",
            self.digits,
            self.exponent.as_deref().unwrap_or("")
        )
    }
}

/// A compile-time constant value.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#data-types>
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    /// 32-bit signed integer (no suffix in source). Stores the raw digits
    /// verbatim so the formatter preserves the exact written form and never
    /// loses information by round-tripping through a fixed-width integer.
    Number(String),
    /// 64-bit signed integer (`l` suffix in source). Stores the raw digits
    /// (without the suffix) for the same reason as [`LiteralValue::Number`].
    Long(String),
    /// A hex-formatted integer literal (`0x…`). Stores the raw digits so the
    /// formatter can preserve the original casing.
    Hex(String),
    /// A hex-formatted 64-bit integer literal (`0x…l`). Stores the raw digits
    /// so the formatter can preserve the original casing.
    HexLong(String),
    /// 32-bit floating point number. Source-form flags let the formatter
    /// round-trip the exact written form (`0f`, `0.5`, `0.5f`, `.978`, `.5f`).
    Float(FloatLit),
    /// 64-bit floating point number. Source-form flags let the formatter
    /// round-trip the exact written form (`78d`, `78.0d`, `.5d`).
    Double(DoubleLit),
    /// A string literal. Stores the raw source text between the double
    /// quotes, with escape sequences left intact so the formatter emits the
    /// literal exactly as written.
    String(String),
    /// A character literal: `'a'`. Stores the raw source text between the
    /// single quotes, with escape sequences left intact.
    Char(String),
    Boolean(bool),
    /// A symbol literal, e.g. `:mySymbol`.
    Symbol(String),
    Null,
    NaN,
}
