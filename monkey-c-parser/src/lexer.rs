use crate::ast::{DoubleLit, FloatLit};
use crate::token;

enum NumberLiteral {
    Number(String),
    Long(String),
    Float(FloatLit),
    Double(DoubleLit),
    Hex(String),
    HexLong(String),
}

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };

        lexer.read_char();
        lexer
    }

    fn read_char(&mut self) {
        if self.read_position >= self.input.len() {
            self.ch = 0;
        } else {
            self.ch = self.input.as_bytes()[self.read_position];
        }

        self.position = self.read_position;
        self.read_position += 1;
    }

    fn read_n_chars(&mut self, n: usize) {
        for _ in 0..n {
            self.read_char();
        }
    }

    fn peek_char(&self) -> u8 {
        self.peek_char_at(1)
    }

    fn peek_char_at(&self, offset: usize) -> u8 {
        let pos = self.read_position + offset - 1;
        if pos >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[pos]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            // `monkeyc` tolerates stray C0 control characters between tokens (e.g. a leftover `DC3`
            // byte before a `{`). `0x00` is excluded since it's this lexer's EOF sentinel. It also
            // ignores stray backticks, which some editors insert next to identifiers.
            if self.ch.is_ascii_whitespace() || (self.ch != 0 && self.ch < 0x20) || self.ch == b'`'
            {
                self.read_char();
            } else if self.ch == b'\\' && self.peek_char() == b'\n' {
                // Line continuation: `\` immediately followed by a newline lets an expression span
                // multiple lines.
                self.read_n_chars(2);
            } else {
                break;
            }
        }
    }

    fn is_newline_or_end(&self) -> bool {
        self.ch == b'\n' || self.ch == 0
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;
        let mut chars = self.input[position..].chars();

        let first = chars.next().unwrap_or('\0');
        if !first.is_alphabetic() && first != '_' {
            return String::new();
        }

        let mut byte_len = first.len_utf8();
        for ch in chars {
            if ch.is_alphanumeric() || ch == '_' {
                byte_len += ch.len_utf8();
            } else {
                break;
            }
        }

        self.read_n_chars(byte_len);
        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> NumberLiteral {
        let start_position = self.position;
        let leading_dot = self.ch == b'.';

        if self.ch == b'0' && matches!(self.peek_char(), b'x' | b'X') {
            self.read_n_chars(2); // consume `0x`
            let digits_start = self.position;
            while self.ch.is_ascii_hexdigit() {
                self.read_char();
            }

            let digits = self.input[digits_start..self.position].to_string();
            if matches!(self.ch, b'l' | b'L') {
                self.read_char();
                return NumberLiteral::HexLong(digits);
            }

            return NumberLiteral::Hex(digits);
        }

        let mut has_dot = false;
        loop {
            if self.ch.is_ascii_digit() {
                self.read_char();
            } else if self.ch == b'.' && !has_dot && self.peek_char().is_ascii_digit() {
                // A `.` only belongs to the number if followed by a digit; otherwise it's a member
                // access on an integer literal, e.g. `0.toFloat()`.
                has_dot = true;
                self.read_char();
            } else {
                break;
            }
        }

        let digits_end = self.position;

        // Scientific notation exponent: e.g. `e3`, `E-2`, `e+10`. Only consume it when at least one
        // digit follows the optional sign.
        let exponent = if matches!(self.ch, b'e' | b'E') {
            let next = self.peek_char();
            let has_exp_digits = next.is_ascii_digit()
                || (matches!(next, b'+' | b'-') && self.peek_char_at(2).is_ascii_digit());

            if has_exp_digits {
                let exp_start = self.position;
                self.read_char(); // consume 'e'/'E'
                if matches!(self.ch, b'+' | b'-') {
                    self.read_char(); // consume optional sign
                }

                while self.ch.is_ascii_digit() {
                    self.read_char();
                }

                Some(self.input[exp_start..self.position].to_string())
            } else {
                None
            }
        } else {
            None
        };

        let text = self.input[start_position..self.position].to_string();
        let digits = self.input[start_position..digits_end].to_string();

        match self.ch {
            b'l' | b'L' => {
                self.read_char();
                NumberLiteral::Long(text)
            }
            b'd' | b'D' => {
                self.read_char();
                NumberLiteral::Double(DoubleLit {
                    digits,
                    has_dot,
                    leading_dot,
                    exponent,
                })
            }
            b'f' | b'F' => {
                self.read_char();
                NumberLiteral::Float(FloatLit {
                    digits,
                    has_dot,
                    leading_dot,
                    has_suffix: true,
                    exponent,
                })
            }
            _ if has_dot || exponent.is_some() => NumberLiteral::Float(FloatLit {
                digits,
                has_dot,
                leading_dot,
                has_suffix: false,
                exponent,
            }),
            _ => NumberLiteral::Number(text),
        }
    }

    fn read_string(&mut self) -> String {
        self.read_quoted(b'"')
    }

    fn read_char_literal(&mut self) -> String {
        self.read_quoted(b'\'')
    }

    /// Read a quoted literal's content up to (but not consuming) the closing `quote`, then consume
    /// the quote. The returned slice is the raw source text with escape sequences left intact —
    /// decoding is intentionally not done here, so the formatter can re-emit the literal exactly as
    /// written (`\x41`, `A`, `\b` etc. survive verbatim instead of being lost).
    ///
    /// A backslash escapes the next byte so an escaped quote (`\"`) does not terminate the literal.
    /// Slicing is byte-based but always lands on a `char` boundary: the loop only stops on ASCII
    /// bytes, and UTF-8 continuation bytes (>= 0x80) never match them.
    fn read_quoted(&mut self, quote: u8) -> String {
        let start = self.position;

        loop {
            match self.ch {
                0 => break,
                c if c == quote => break,
                b'\\' => {
                    self.read_char();
                    if self.ch != 0 {
                        self.read_char();
                    }
                }
                _ => self.read_char(),
            }
        }

        let content = self.input[start..self.position].to_string();
        self.read_char(); // advance past closing quote

        content
    }

    fn read_comment(&mut self) -> String {
        let position = self.position;

        while !self.is_newline_or_end() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_block_comment(&mut self) -> String {
        let position = self.position;

        while self.ch != 0 {
            if self.ch == b'*' && self.peek_char() == b'/' {
                let content = self.input[position..self.position].to_string();
                self.read_n_chars(2); // consume `*/`

                return content;
            }
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    pub fn peek_token(&self) -> (usize, token::Type, usize) {
        Self {
            input: self.input,
            position: self.position,
            read_position: self.read_position,
            ch: self.ch,
        }
        .next_token()
    }

    /// Like [`peek_token`](Self::peek_token), but skips over any comments between the current token
    /// and the next significant one.
    pub fn peek_token_skip_comments(&self) -> (usize, token::Type, usize) {
        let mut lexer = Self {
            input: self.input,
            position: self.position,
            read_position: self.read_position,
            ch: self.ch,
        };

        loop {
            let token = lexer.next_token();
            if !matches!(
                token.1,
                token::Type::Comment(_) | token::Type::BlockComment(_)
            ) {
                return token;
            }
        }
    }

    pub fn next_token(&mut self) -> (usize, token::Type, usize) {
        self.skip_whitespace();

        let start = self.position;
        let (token_type, chars_to_read) = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    (token::Type::EqualEqual, 2)
                } else if self.peek_char() == b'>' {
                    (token::Type::FatArrow, 2)
                } else {
                    (token::Type::Assign, 1)
                }
            }
            b'+' => {
                if self.peek_char() == b'=' {
                    (token::Type::AddAssign, 2)
                } else if self.peek_char() == b'+' {
                    (token::Type::PlusPlus, 2)
                } else {
                    (token::Type::Plus, 1)
                }
            }
            b'-' => {
                if self.peek_char() == b'=' {
                    (token::Type::SubAssign, 2)
                } else if self.peek_char() == b'-' {
                    (token::Type::MinusMinus, 2)
                } else {
                    (token::Type::Minus, 1)
                }
            }
            b'*' => {
                if self.peek_char() == b'=' {
                    (token::Type::MulAssign, 2)
                } else {
                    (token::Type::Star, 1)
                }
            }
            b'/' => {
                if self.peek_char() == b'=' {
                    (token::Type::DivAssign, 2)
                } else if self.peek_char() == b'/' {
                    self.read_n_chars(2);
                    let content = self.read_comment();
                    return (start, token::Type::Comment(content), self.position);
                } else if self.peek_char() == b'*' {
                    self.read_n_chars(2); // consume `/*`
                    let content = self.read_block_comment();
                    return (start, token::Type::BlockComment(content), self.position);
                } else {
                    (token::Type::Slash, 1)
                }
            }
            b'%' => {
                if self.peek_char() == b'=' {
                    (token::Type::ModAssign, 2)
                } else {
                    (token::Type::Percent, 1)
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    (token::Type::BangEqual, 2)
                } else {
                    (token::Type::Bang, 1)
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    (token::Type::LessEqual, 2)
                } else if self.peek_char() == b'<' {
                    if self.peek_char_at(2) == b'=' {
                        (token::Type::LeftShiftAssign, 3)
                    } else {
                        (token::Type::LeftShift, 2)
                    }
                } else {
                    (token::Type::Less, 1)
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    (token::Type::GreaterEqual, 2)
                } else if self.peek_char() == b'>' {
                    if self.peek_char_at(2) == b'=' {
                        (token::Type::RightShiftAssign, 3)
                    } else {
                        (token::Type::RightShift, 2)
                    }
                } else {
                    (token::Type::Greater, 1)
                }
            }
            b'&' => {
                if self.peek_char() == b'=' {
                    (token::Type::BitAndAssign, 2)
                } else if self.peek_char() == b'&' {
                    (token::Type::And, 2)
                } else {
                    (token::Type::BitAnd, 1)
                }
            }
            b'|' => {
                if self.peek_char() == b'=' {
                    (token::Type::BitOrAssign, 2)
                } else if self.peek_char() == b'|' {
                    (token::Type::Or, 2)
                } else {
                    (token::Type::BitOr, 1)
                }
            }
            b'^' => {
                if self.peek_char() == b'=' {
                    (token::Type::BitXorAssign, 2)
                } else {
                    (token::Type::BitXor, 1)
                }
            }
            b'~' => (token::Type::Tilde, 1),
            b'?' => (token::Type::Question, 1),
            b':' => (token::Type::Colon, 1),
            b';' => (token::Type::Semicolon, 1),
            b',' => (token::Type::Comma, 1),
            b'.' => {
                // `.978` — leading-zero-omitted float literal. `read_number`
                // sets the `leading_dot` flag from `self.ch == b'.'` so we
                // re-use the same path here.
                if self.peek_char().is_ascii_digit() {
                    let token_type = match self.read_number() {
                        NumberLiteral::Float(lit) => token::Type::Float(lit),
                        NumberLiteral::Double(lit) => token::Type::Double(lit),
                        _ => unreachable!("leading `.` always produces a float-class literal"),
                    };
                    return (start, token_type, self.position);
                }
                (token::Type::Dot, 1)
            }
            b'(' => (token::Type::LParen, 1),
            b')' => (token::Type::RParen, 1),
            b'{' => (token::Type::LBrace, 1),
            b'}' => (token::Type::RBrace, 1),
            b'[' => (token::Type::LBracket, 1),
            b']' => (token::Type::RBracket, 1),
            b'"' => {
                self.read_char(); // Move past opening quote
                let content = self.read_string();
                return (start, token::Type::String(content), self.position);
            }
            b'\'' => {
                self.read_char(); // Move past opening quote
                let content = self.read_char_literal();
                return (start, token::Type::Char(content), self.position);
            }
            b'$' => (token::Type::Bling, 1),
            b'@' => (token::Type::At, 1),
            0 => (token::Type::Eof, 0),
            _ => {
                let first = self.input[self.position..].chars().next().unwrap_or('\0');
                if first.is_alphabetic() || first == '_' {
                    let ident = self.read_identifier();
                    if let Ok(token_type) = ident.parse() {
                        (token_type, 0)
                    } else {
                        (token::Type::Identifier(ident), 0)
                    }
                } else if self.ch.is_ascii_digit() {
                    let token_type = match self.read_number() {
                        NumberLiteral::Number(s) => token::Type::Number(s),
                        NumberLiteral::Long(s) => token::Type::Long(s),
                        NumberLiteral::Float(lit) => token::Type::Float(lit),
                        NumberLiteral::Double(lit) => token::Type::Double(lit),
                        NumberLiteral::Hex(s) => token::Type::Hex(s),
                        NumberLiteral::HexLong(s) => token::Type::HexLong(s),
                    };
                    (token_type, 0)
                } else {
                    (token::Type::Illegal, 0)
                }
            }
        };

        self.read_n_chars(chars_to_read);

        (start, token_type, self.position)
    }
}
