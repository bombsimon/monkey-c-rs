use crate::token;

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
        if self.read_position >= self.input.len() {
            0
        } else {
            self.input.as_bytes()[self.read_position]
        }
    }

    fn skip_whitespace(&mut self) {
        while self.ch.is_ascii_whitespace() {
            self.read_char();
        }
    }

    fn is_newline_or_end(&self) -> bool {
        self.ch == b'\n' || self.ch == 0
    }

    fn read_identifier(&mut self) -> String {
        let position = self.position;

        if self.ch.is_ascii_alphabetic() || self.ch == b'_' {
            self.read_char();

            while self.ch.is_ascii_alphanumeric() || self.ch == b'_' {
                self.read_char();
            }
        }

        self.input[position..self.position].to_string()
    }

    fn read_number(&mut self) -> (String, bool) {
        let start_position = self.position;
        let mut is_float = false;

        while self.ch.is_ascii_digit() || self.ch == b'.' {
            if self.ch == b'.' {
                is_float = true;
            }

            self.read_char();
        }

        (
            self.input[start_position..self.position].to_string(),
            is_float,
        )
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();

        loop {
            match self.ch {
                b'"' => break, // End of string, non escaped quote
                0 => break,    // End of input
                b'\\' => {
                    self.read_char();

                    match self.ch {
                        b'n' => result.push('\n'),
                        b'r' => result.push('\r'),
                        b't' => result.push('\t'),
                        b'\\' => result.push('\\'),
                        b'"' => result.push('"'),
                        _ => result.push(self.ch as char),
                    }
                }
                _ => result.push(self.ch as char),
            }

            self.read_char();
        }

        self.read_char(); // Now advance past the closing quote

        result
    }

    fn read_comment(&mut self) -> String {
        let position = self.position;

        while !self.is_newline_or_end() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_annotation(&mut self) -> token::Type {
        self.read_n_chars(2); // consume '(:'
        let annotation = self.read_identifier();
        self.read_char(); // consume ')'

        token::Type::Annotation(annotation)
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
                } else {
                    (token::Type::Less, 1)
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    (token::Type::GreaterEqual, 2)
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
            b':' => {
                let next = self.peek_char();
                if next.is_ascii_alphabetic() || next == b'_' {
                    self.read_char(); // advance past ':'
                    let name = self.read_identifier();
                    return (start, token::Type::Symbol(name), self.position);
                } else {
                    (token::Type::Colon, 1)
                }
            }
            b';' => (token::Type::Semicolon, 1),
            b',' => (token::Type::Comma, 1),
            b'.' => (token::Type::Dot, 1),
            b'(' => {
                if self.peek_char() == b':' {
                    (self.read_annotation(), 0)
                } else {
                    (token::Type::LParen, 1)
                }
            }
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
            b'$' => (token::Type::Bling, 1),
            b'\n' => (token::Type::Newline, 1),
            0 => (token::Type::Eof, 0),
            _ => {
                if self.ch.is_ascii_alphabetic() || self.ch == b'_' {
                    let ident = self.read_identifier();
                    if let Ok(token_type) = ident.parse() {
                        (token_type, 0)
                    } else {
                        (token::Type::Identifier(ident), 0)
                    }
                } else if self.ch.is_ascii_digit() {
                    let (num, is_float) = self.read_number();
                    if is_float {
                        (token::Type::Double(num.parse().unwrap_or(0.0)), 0)
                    } else {
                        (token::Type::Long(num.parse().unwrap_or(0)), 0)
                    }
                } else {
                    (token::Type::Illegal, 0)
                }
            }
        };

        self.read_n_chars(chars_to_read);

        (start, token_type, self.position)
    }
}
