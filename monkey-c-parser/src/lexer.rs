#![allow(dead_code)]
use crate::token::{self, Token};
use std::str::FromStr;

pub(crate) struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { input, position: 0 }
    }

    fn next_token(&mut self) -> Token {
        let start_position = self.position;

        self.skip_whitespace();

        if self.position >= self.input.len() {
            return Token::new(token::Type::EOF, start_position, self.position);
        }

        let current_char = self.input.chars().nth(self.position).unwrap();

        match current_char {
            ';' => {
                self.position += 1;
                self.next_token()
            }
            '\n' => {
                self.position += 1;
                Token::new(token::Type::Newline, start_position, self.position)
            }
            '0'..='9' => match self.parse_number() {
                (n, true) => Token::new(token::Type::Double(n), start_position, self.position),
                (n, false) => {
                    Token::new(token::Type::Long(n as i64), start_position, self.position)
                }
            },
            ',' => {
                self.position += 1;
                Token::new(token::Type::Comma, start_position, self.position)
            }
            _ if current_char.is_ascii_alphabetic() => {
                let token_type = self.parse_identifier();
                Token::new(token_type, start_position, self.position)
            }
            // TODO: Assignment operator +=, -= etc
            // TODO: Relational operator >=, <= etc
            // TODO: Assignment operator <<=, >>= etc
            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' => {
                self.position += 1;
                Token::new(
                    token::Type::from_str(&current_char.to_string()).unwrap(),
                    start_position,
                    self.position,
                )
            }
            '(' | ')' | '[' | ']' | '{' | '}' => {
                self.position += 1;
                Token::new(
                    token::Type::from_str(&current_char.to_string()).unwrap(),
                    start_position,
                    self.position,
                )
            }
            _ => panic!("Unexpected character: {}", current_char),
        }
    }

    fn peek_token(&mut self) -> Token {
        let saved_position = self.position;
        let token = self.next_token();
        self.position = saved_position; // Restore the position

        token
    }

    fn parse_identifier(&mut self) -> token::Type {
        let start = self.position;

        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_ascii_alphabetic() || c.is_ascii_digit() {
                self.position += 1;
            } else {
                break;
            }
        }

        let ident = self.input[start..self.position].to_string();
        if let Ok(tt) = token::Type::from_str(&ident) {
            tt
        } else {
            token::Type::Identifier(ident)
        }
    }

    fn parse_number(&mut self) -> (f64, bool) {
        let start = self.position;
        let mut has_fraction = false;

        while let Some(c) = self.input.chars().nth(self.position) {
            if c == '.' {
                has_fraction = true;
            }

            if c.is_ascii_digit() || c == '.' {
                self.position += 1;
            } else {
                break;
            }
        }

        let num_str = &self.input[start..self.position];
        (num_str.parse().unwrap(), has_fraction)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_whitespace() && c != '\n' {
                self.position += 1;
            } else {
                break;
            }
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_tokenizer() {
        let input = r#"
        module Foo {
            class Bar {
                function fooBar(arg1 as Number, arg2 as Float) {
                    var a = arg1 + arg2 * 3.2 / 4;
                }
            }
        }
        "#;
        let mut tokenizer = super::Lexer::new(input);

        loop {
            let tkn = tokenizer.next_token();
            println!("token: {tkn:?}");

            if tkn.token_type == crate::token::Type::EOF {
                break;
            }
        }
    }
}
