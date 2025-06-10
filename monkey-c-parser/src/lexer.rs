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
                b'"' => break, // Only consume the closing quote, do not advance further
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
        let position = self.position + 2; // Skip `//`

        while !self.is_newline_or_end() {
            self.read_char();
        }

        self.input[position..self.position].to_string()
    }

    fn read_annotation(&mut self) -> token::Type {
        self.read_char(); // consume '('
        self.read_char(); // consume ':'
                          //
        let annotation = self.read_identifier();

        self.read_char(); // consume ')'

        token::Type::Annotation(annotation)
    }

    pub fn next_token(&mut self) -> (usize, token::Type, usize) {
        self.skip_whitespace();

        let start = self.position;
        let token_type = match self.ch {
            b'=' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::EqualEqual
                } else {
                    self.read_char();
                    token::Type::Assign
                }
            }
            b'+' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::AddAssign
                } else if self.peek_char() == b'+' {
                    self.read_char();
                    self.read_char();
                    token::Type::PlusPlus
                } else {
                    self.read_char();
                    token::Type::Plus
                }
            }
            b'-' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::SubAssign
                } else if self.peek_char() == b'-' {
                    self.read_char();
                    self.read_char();
                    token::Type::MinusMinus
                } else {
                    self.read_char();
                    token::Type::Minus
                }
            }
            b'*' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::MulAssign
                } else {
                    self.read_char();
                    token::Type::Star
                }
            }
            b'/' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::DivAssign
                } else if self.peek_char() == b'/' {
                    self.read_char();
                    self.read_char();
                    let content = self.read_comment();
                    return (start, token::Type::Comment(content), self.position);
                } else {
                    self.read_char();
                    token::Type::Slash
                }
            }
            b'%' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::ModAssign
                } else {
                    self.read_char();
                    token::Type::Percent
                }
            }
            b'!' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::BangEqual
                } else {
                    self.read_char();
                    token::Type::Bang
                }
            }
            b'<' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::LessEqual
                } else {
                    self.read_char();
                    token::Type::Less
                }
            }
            b'>' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::GreaterEqual
                } else {
                    self.read_char();
                    token::Type::Greater
                }
            }
            b'&' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::BitAndAssign
                } else if self.peek_char() == b'&' {
                    self.read_char();
                    self.read_char();
                    token::Type::And
                } else {
                    self.read_char();
                    token::Type::BitAnd
                }
            }
            b'|' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::BitOrAssign
                } else if self.peek_char() == b'|' {
                    self.read_char();
                    self.read_char();
                    token::Type::Or
                } else {
                    self.read_char();
                    token::Type::BitOr
                }
            }
            b'^' => {
                if self.peek_char() == b'=' {
                    self.read_char();
                    self.read_char();
                    token::Type::BitXorAssign
                } else {
                    self.read_char();
                    token::Type::BitXor
                }
            }
            b'~' => {
                self.read_char();
                token::Type::Tilde
            }
            b'?' => {
                self.read_char();
                token::Type::Question
            }
            b':' => {
                self.read_char();
                token::Type::Colon
            }
            b';' => {
                self.read_char();
                token::Type::Semicolon
            }
            b',' => {
                self.read_char();
                token::Type::Comma
            }
            b'.' => {
                self.read_char();
                token::Type::Dot
            }
            b'(' => {
                if self.peek_char() == b':' {
                    self.read_annotation()
                } else {
                    self.read_char();
                    token::Type::LParen
                }
            }
            b')' => {
                self.read_char();
                token::Type::RParen
            }
            b'{' => {
                self.read_char();
                token::Type::LBrace
            }
            b'}' => {
                self.read_char();
                token::Type::RBrace
            }
            b'[' => {
                self.read_char();
                token::Type::LBracket
            }
            b']' => {
                self.read_char();
                token::Type::RBracket
            }
            b'"' => {
                self.read_char(); // Move past opening quote
                let content = self.read_string();
                return (start, token::Type::String(content), self.position);
            }
            b'\n' => {
                self.read_char();
                token::Type::Newline
            }
            0 => token::Type::Eof,
            _ => {
                if self.ch.is_ascii_alphabetic() || self.ch == b'_' {
                    let ident = self.read_identifier();
                    if let Ok(token_type) = ident.parse() {
                        token_type
                    } else {
                        token::Type::Identifier(ident)
                    }
                } else if self.ch.is_ascii_digit() {
                    let (num, is_float) = self.read_number();
                    if is_float {
                        token::Type::Double(num.parse().unwrap_or(0.0))
                    } else {
                        token::Type::Long(num.parse().unwrap_or(0))
                    }
                } else {
                    token::Type::Illegal
                }
            }
        };

        (start, token_type, self.position)
    }

    pub fn peek_token(&self) -> (usize, token::Type, usize) {
        let mut lexer = Self {
            input: self.input,
            position: self.position,
            read_position: self.read_position,
            ch: self.ch,
        };

        lexer.next_token()
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Type;

    fn consume_all_tokens(input: &str) -> Vec<Type> {
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();

        loop {
            let (_, token_type, _) = lexer.next_token();
            if token_type == Type::Eof {
                break;
            }

            tokens.push(token_type);
        }

        tokens
    }

    #[test]
    fn test_lexer() {
        let input = "var x = 5.0;";
        let tokens = consume_all_tokens(input);

        let expected = vec![
            Type::Var,
            Type::Identifier("x".to_string()),
            Type::Assign,
            Type::Double(5.0),
            Type::Semicolon,
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_lexer_class_declaration() {
        let input = "class MyClass { var x as Float; }";
        let tokens = consume_all_tokens(input);
        let expected = vec![
            Type::Class,
            Type::Identifier("MyClass".to_string()),
            Type::LBrace,
            Type::Var,
            Type::Identifier("x".to_string()),
            Type::As,
            Type::Identifier("Float".to_string()),
            Type::Semicolon,
            Type::RBrace,
        ];

        assert_eq!(expected, tokens);
    }

    #[test]
    fn test_lexer_string_literal() {
        let input = "x = \"hello world\"; y = 42;";
        let tokens = consume_all_tokens(input);
        let expected = vec![
            Type::Identifier("x".to_string()),
            Type::Assign,
            Type::String("hello world".to_string()),
            Type::Semicolon,
            Type::Identifier("y".to_string()),
            Type::Assign,
            Type::Long(42),
            Type::Semicolon,
        ];

        assert_eq!(expected, tokens,);
    }

    #[test]
    fn test_lexer_string_literal_with_escape() {
        let input = "msg = \"line1\\nline2\";";
        let tokens = consume_all_tokens(input);
        let expected = vec![
            Type::Identifier("msg".to_string()),
            Type::Assign,
            Type::String("line1\nline2".to_string()),
            Type::Semicolon,
        ];

        assert_eq!(expected, tokens,);
    }
}
