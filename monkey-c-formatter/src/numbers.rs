//! Respelling number literals by the [`Numbers`] settings. Literals keep their source text, so
//! each part is picked out of that text and rewritten on its own.

use monkey_c_parser::ast::{DoubleLit, FloatLit};

use crate::options::{Case, ExponentPadding, LeadingZero, Numbers, TrailingZeros};

impl Case {
    fn apply(self, text: &str) -> String {
        match self {
            Case::Lower => text.to_ascii_lowercase(),
            Case::Upper => text.to_ascii_uppercase(),
            Case::Preserve => text.to_string(),
        }
    }
}

impl Numbers {
    /// A `0x…` literal, with or without a long suffix.
    pub(crate) fn hex(&self, text: &str) -> String {
        let (prefix, rest) = text.split_at(2);
        let (digits, suffix) = split_long_suffix(rest);

        format!(
            "{}{}{}",
            self.hex_prefix_case.apply(prefix),
            self.hex_digit_case.apply(digits),
            self.long_suffix_case.apply(suffix),
        )
    }

    /// A decimal long literal. The lexer keeps a long's text whole even with a `.` or an
    /// exponent, like `1.5e+3l`, so the parts are split back out here.
    pub(crate) fn long(&self, text: &str) -> String {
        let (body, suffix) = split_long_suffix(text);
        let (digits, exponent) = match body.find(['e', 'E']) {
            Some(index) => (&body[..index], Some(&body[index..])),
            None => (body, None),
        };

        self.decimal(digits, exponent, self.long_suffix_case.apply(suffix))
    }

    pub(crate) fn float(&self, lit: &FloatLit) -> String {
        let suffix = lit
            .suffix
            .map(|suffix| self.float_suffix_case.apply(&suffix.to_string()))
            .unwrap_or_default();

        self.decimal(&lit.digits, lit.exponent.as_deref(), suffix)
    }

    pub(crate) fn double(&self, lit: &DoubleLit) -> String {
        let suffix = self.double_suffix_case.apply(&lit.suffix.to_string());

        self.decimal(&lit.digits, lit.exponent.as_deref(), suffix)
    }

    /// Every decimal literal goes through here, so a setting for the digits or the exponent
    /// applies to all of them alike.
    fn decimal(&self, digits: &str, exponent: Option<&str>, suffix: String) -> String {
        format!(
            "{}{}{suffix}",
            self.fraction(digits),
            self.exponent(exponent)
        )
    }

    /// The digits before any exponent. One digit always stays after the `.`, since `1.` doesn't
    /// lex as a float and `1` would be a `Number`.
    fn fraction(&self, digits: &str) -> String {
        let mut digits = digits.to_string();

        if self.leading_zero == LeadingZero::Add && digits.starts_with('.') {
            digits.insert(0, '0');
        }

        if self.trailing_zeros == TrailingZeros::Trim
            && let Some(dot) = digits.find('.')
        {
            let len = digits.trim_end_matches('0').len().max(dot + 2);
            digits.truncate(len);
        }

        digits
    }

    fn exponent(&self, exponent: Option<&str>) -> String {
        let Some(exponent) = exponent else {
            return String::new();
        };

        let (marker, rest) = exponent.split_at(1);
        let marker = self.exponent_case.apply(marker);
        if self.exponent_padding == ExponentPadding::Preserve {
            return format!("{marker}{rest}");
        }

        let (sign, digits) = match rest.strip_prefix('-') {
            Some(digits) => ("-", digits),
            None => ("", rest.strip_prefix('+').unwrap_or(rest)),
        };
        let digits = digits.trim_start_matches('0');
        if digits.is_empty() {
            return format!("{marker}0");
        }

        format!("{marker}{sign}{digits}")
    }
}

fn split_long_suffix(text: &str) -> (&str, &str) {
    if text.ends_with(['l', 'L']) {
        text.split_at(text.len() - 1)
    } else {
        (text, "")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn float(digits: &str, exponent: Option<&str>, suffix: Option<char>) -> FloatLit {
        FloatLit {
            digits: digits.to_string(),
            has_dot: digits.contains('.'),
            leading_dot: digits.starts_with('.'),
            suffix,
            exponent: exponent.map(str::to_string),
        }
    }

    #[test]
    fn hex_parts_are_cased_apart() {
        let numbers = Numbers::default();

        assert_eq!(numbers.hex("0XffAa"), "0xFFAA");
        assert_eq!(numbers.hex("0xffl"), "0xFFL");
        assert_eq!(Numbers::preserve().hex("0XffAal"), "0XffAal");
    }

    #[test]
    fn long_keeps_its_digits() {
        assert_eq!(Numbers::default().long("10l"), "10L");
        assert_eq!(Numbers::preserve().long("10l"), "10l");
    }

    #[test]
    fn long_with_a_fraction_or_exponent_is_respelled_like_a_float() {
        let numbers = Numbers {
            trailing_zeros: TrailingZeros::Trim,
            ..Numbers::default()
        };

        assert_eq!(numbers.long("1E+05l"), "1e5L");
        assert_eq!(numbers.long(".50e-03l"), "0.5e-3L");
        assert_eq!(Numbers::preserve().long(".50E+03l"), ".50E+03l");
    }

    #[test]
    fn a_fraction_keeps_one_digit_after_the_dot() {
        let numbers = Numbers {
            trailing_zeros: TrailingZeros::Trim,
            ..Numbers::default()
        };

        assert_eq!(numbers.float(&float("1.50", None, None)), "1.5");
        assert_eq!(numbers.float(&float("1.0", None, None)), "1.0");
        assert_eq!(numbers.float(&float("10.000", None, None)), "10.0");
        assert_eq!(numbers.float(&float("100", None, Some('f'))), "100f");
        assert_eq!(numbers.float(&float(".50", Some("e3"), None)), "0.5e3");
    }

    #[test]
    fn a_fraction_gets_its_leading_zero() {
        assert_eq!(Numbers::default().float(&float(".5", None, None)), "0.5");
        assert_eq!(Numbers::preserve().float(&float(".5", None, None)), ".5");
    }

    #[test]
    fn exponent_padding_is_trimmed() {
        let numbers = Numbers::default();

        assert_eq!(numbers.float(&float("2.5", Some("E+07"), None)), "2.5e7");
        assert_eq!(numbers.float(&float("2.5", Some("e-07"), None)), "2.5e-7");
        assert_eq!(numbers.float(&float("1", Some("e00"), Some('F'))), "1e0f");
        assert_eq!(numbers.float(&float("1", Some("e-00"), None)), "1e0");
        assert_eq!(
            Numbers::preserve().float(&float("1", Some("E+07"), None)),
            "1E+07"
        );
    }
}
