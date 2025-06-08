#![allow(dead_code)]
use crate::token;

pub struct Lexer<'a> {
    input: &'a str,
    position: usize,
    read_position: usize,
    ch: u8,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut l = Self {
            input,
            position: 0,
            read_position: 0,
            ch: 0,
        };
        l.read_char();
        l
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
        let position = self.position;
        let mut is_float = false;
        while self.ch.is_ascii_digit() || self.ch == b'.' {
            if self.ch == b'.' {
                is_float = true;
            }
            self.read_char();
        }
        (self.input[position..self.position].to_string(), is_float)
    }

    fn read_string(&mut self) -> String {
        let mut result = String::new();
        loop {
            if self.ch == b'"' {
                break; // Only consume the closing quote, do not advance further
            }
            if self.ch == 0 {
                break; // End of input
            }
            if self.ch == b'\\' {
                self.read_char();
                match self.ch {
                    b'n' => result.push('\n'),
                    b'r' => result.push('\r'),
                    b't' => result.push('\t'),
                    b'\\' => result.push('\\'),
                    b'"' => result.push('"'),
                    _ => result.push(self.ch as char),
                }
            } else {
                result.push(self.ch as char);
            }
            self.read_char();
        }
        self.read_char(); // Now advance past the closing quote
        result
    }

    fn read_comment(&mut self) -> String {
        let position = self.position + 2; // Skip //
        loop {
            self.read_char();
            if self.ch == b'\n' || self.ch == 0 {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    fn read_annotation(&mut self) -> String {
        let position = self.position + 1; // Skip @
        loop {
            self.read_char();
            if self.ch == b'\n' || self.ch == 0 {
                break;
            }
        }
        self.input[position..self.position].to_string()
    }

    pub fn next_token(&mut self) -> (usize, token::Type, usize) {
        self.skip_whitespace();

        let start = self.position;
        // Special handling for (:annotation) syntax
        if self.ch == b'(' && self.peek_char() == b':' {
            self.read_char(); // consume '('
            self.read_char(); // consume ':'
            let annotation_start = self.position;
            // Read until ')'
            while self.ch != b')' && self.ch != 0 && self.ch != b'\n' {
                self.read_char();
            }
            let annotation_content = self.input[annotation_start..self.position].trim().to_string();
            if self.ch == b')' {
                self.read_char(); // consume ')'
            }
            return (start, token::Type::Annotation(annotation_content), self.position);
        }

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
                self.read_char();
                token::Type::LParen
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
            b'@' => {
                self.read_char();
                let content = self.read_annotation();
                return (start, token::Type::Annotation(content), self.position);
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
                    match ident.as_str() {
                        "class" => token::Type::Class,
                        "function" => token::Type::Function,
                        "var" => token::Type::Var,
                        "hidden" => token::Type::Hidden,
                        "static" => token::Type::Static,
                        "const" => token::Type::Const,
                        "using" => token::Type::Using,
                        "module" => token::Type::Module,
                        "enum" => token::Type::Enum,
                        "if" => token::Type::If,
                        "else" => token::Type::Else,
                        "while" => token::Type::While,
                        "for" => token::Type::For,
                        "return" => token::Type::Return,
                        "break" => token::Type::Break,
                        "continue" => token::Type::Continue,
                        "switch" => token::Type::Switch,
                        "case" => token::Type::Case,
                        "default" => token::Type::Default,
                        "try" => token::Type::Try,
                        "catch" => token::Type::Catch,
                        "finally" => token::Type::Finally,
                        "throw" => token::Type::Throw,
                        "has" => token::Type::Has,
                        "is" => token::Type::Is,
                        "where" => token::Type::Where,
                        "do" => token::Type::Do,
                        "until" => token::Type::Until,
                        "as" => token::Type::As,
                        "extends" => token::Type::Extends,
                        "import" => token::Type::Import,
                        "new" => token::Type::New,
                        "instanceof" => token::Type::InstanceOf,
                        "private" => token::Type::Private,
                        "protected" => token::Type::Protected,
                        "public" => token::Type::Public,
                        "true" => token::Type::Boolean(true),
                        "false" => token::Type::Boolean(false),
                        "null" => token::Type::Null,
                        "NaN" => token::Type::NaN,
                        "me" => token::Type::Me,
                        "self" => token::Type::Self_,
                        _ => token::Type::Identifier(ident),
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

        let end = self.position;
        (start, token_type, end)
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

    pub fn get_state(&self) -> (usize, usize, u8) {
        (self.position, self.read_position, self.ch)
    }

    pub fn set_state(&mut self, position: usize, read_position: usize, ch: u8) {
        self.position = position;
        self.read_position = read_position;
        self.ch = ch;
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Lexer;
    use crate::token::Type;

    #[test]
    fn test_lexer() {
        let input = "var x = 5.0;";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let (_, token_type, _) = lexer.next_token();
            if token_type == Type::Eof {
                break;
            }
            tokens.push(token_type);
        }
        assert_eq!(tokens.len(), 5);
        assert_eq!(tokens[0], Type::Var);
        assert_eq!(tokens[1], Type::Identifier("x".to_string()));
        assert_eq!(tokens[2], Type::Assign);
        assert_eq!(tokens[3], Type::Double(5.0));
        assert_eq!(tokens[4], Type::Semicolon);
    }

    #[test]
    fn test_lexer_class_declaration() {
        let input = "class MyClass { var x as Float; }";
        let mut lexer = Lexer::new(input);
        let mut tokens = Vec::new();
        loop {
            let (_, token_type, _) = lexer.next_token();
            if token_type == Type::Eof {
                break;
            }
            tokens.push(token_type);
        }
        assert_eq!(tokens.len(), 9);
        assert_eq!(tokens[0], Type::Class);
        assert_eq!(tokens[1], Type::Identifier("MyClass".to_string()));
        assert_eq!(tokens[2], Type::LBrace);
        assert_eq!(tokens[3], Type::Var);
        assert_eq!(tokens[4], Type::Identifier("x".to_string()));
        assert_eq!(tokens[5], Type::As);
        assert_eq!(tokens[6], Type::Identifier("Float".to_string()));
        assert_eq!(tokens[7], Type::Semicolon);
        assert_eq!(tokens[8], Type::RBrace);
    }

    #[test]
    fn test_lexer_string_literal() {
        let mut lexer = Lexer::new("x = \"hello world\"; y = 42;");
        let mut tokens = Vec::new();
        loop {
            let (_, t, _) = lexer.next_token();
            if let token::Type::Eof = t {
                break;
            }
            tokens.push(t);
        }
        assert_eq!(tokens,
            vec![
                token::Type::Identifier("x".to_string()),
                token::Type::Assign,
                token::Type::String("hello world".to_string()),
                token::Type::Semicolon,
                token::Type::Identifier("y".to_string()),
                token::Type::Assign,
                token::Type::Long(42),
                token::Type::Semicolon,
            ]
        );
    }

    #[test]
    fn test_lexer_string_literal_with_escape() {
        let mut lexer = Lexer::new("msg = \"line1\\nline2\";");
        let mut tokens = Vec::new();
        loop {
            let (_, t, _) = lexer.next_token();
            if let token::Type::Eof = t {
                break;
            }
            tokens.push(t);
        }
        assert_eq!(tokens,
            vec![
                token::Type::Identifier("msg".to_string()),
                token::Type::Assign,
                token::Type::String("line1\nline2".to_string()),
                token::Type::Semicolon,
            ]
        );
    }
}

