#![allow(dead_code)]
use crate::token::{self, Span};
use std::str::FromStr;

pub(crate) struct Lexer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Lexer<'a> {
        Lexer { input, position: 0 }
    }

    fn next_token(&mut self) -> Span {
        let start_position = self.position;

        self.skip_whitespace();

        if self.position >= self.input.len() {
            return (start_position, token::Type::EOF, self.position);
        }

        let current_char = self.input.chars().nth(self.position).unwrap();

        match current_char {
            '\n' => {
                self.position += 1;
                (start_position, token::Type::Newline, self.position)
            }
            '/' => {
                // TODO: Can't just parse comments like this, need context.
                let comment = self.parse_comment();
                (start_position, token::Type::Comment(comment), self.position)
            }
            '0'..='9' => match self.parse_number() {
                (n, true) => (start_position, token::Type::Double(n), self.position),
                (n, false) => (start_position, token::Type::Long(n as i64), self.position),
            },
            '"' => {
                let string = self.parse_string();
                (start_position, token::Type::String(string), self.position)
            }
            _ if current_char.is_ascii_alphabetic() || current_char == '_' => {
                let token_type = self.parse_identifier();
                (start_position, token_type, self.position)
            }
            ',' | ':' | ';' | '.' | '?' => {
                self.position += 1;
                (
                    start_position,
                    token::Type::from_str(&current_char.to_string()).unwrap(),
                    self.position,
                )
            }
            '(' | ')' | '[' | ']' | '{' | '}' => {
                self.position += 1;
                (
                    start_position,
                    token::Type::from_str(&current_char.to_string()).unwrap(),
                    self.position,
                )
            }
            // TODO: Assignment operator +=, -= etc
            // TODO: Relational operator >=, <= etc
            // TODO: Assignment operator <<=, >>= etc
            '+' | '-' | '*' | '/' | '%' | '<' | '>' | '=' => {
                self.position += 1;
                (
                    start_position,
                    token::Type::from_str(&current_char.to_string()).unwrap(),
                    self.position,
                )
            }
            _ => panic!("Unexpected character: {}", current_char),
        }
    }

    fn peek_token(&mut self) -> Span {
        let saved_position = self.position;
        let token = self.next_token();
        self.position = saved_position; // Restore the position

        token
    }

    fn parse_identifier(&mut self) -> token::Type {
        let start = self.position;

        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_ascii_alphabetic() || c.is_ascii_digit() || c == '_' {
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

    fn parse_string(&mut self) -> String {
        let start = self.position;

        // Skip first quote
        self.position += 1;

        let mut is_escape = false;
        while let Some(c) = self.input.chars().nth(self.position) {
            self.position += 1;

            if c == '"' && !is_escape {
                break;
            }

            if c == '\\' {
                is_escape = !is_escape;
            } else {
                is_escape = false;
            }
        }

        self.input[start + 1..self.position - 1].to_string()
    }
    fn parse_comment(&mut self) -> String {
        let start = self.position;

        // Skip first two //
        self.position += 2;

        while let Some(c) = self.input.chars().nth(self.position) {
            if c == '\n' {
                break;
            }

            self.position += 1;
        }

        self.input[start + 2..self.position].to_string()
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
import Toybox.Application;
import Toybox.Lang;
import Toybox.WatchUi;

//! The PaceCalculator app is an app that can convert between pace (min/km) and
//! speed (km/h).
class PaceCalculatorApp extends Application.AppBase {
    private var _speedConverter as SpeedConverter;

    function initialize() {
        _speedConverter = new SpeedConverter();

        AppBase.initialize();
    }

    //! onStart() is called on application start up
    function onStart(state as Dictionary?) as Void {}

    //! onStop() is called when your application is exiting
    function onStop(state as Dictionary?) as Void {}

    //! Returns initial view of application.
    function getInitialView() as Array<Views or InputDelegates>? {
        return [
            new PaceCalculatorView(_speedConverter),
            new PaceCalculatorDelegate(_speedConverter),
        ] as Array<Views or InputDelegates>;
    }
}

function getApp() as PaceCalculatorApp {
    return Application.getApp() as PaceCalculatorApp;
}
        "#;
        let mut tokenizer = super::Lexer::new(input);

        loop {
            let (start, token_type, end) = tokenizer.next_token();
            println!("token: {start:<4} -> {end:<4}: {token_type:?}");

            if token_type == crate::token::Type::EOF {
                break;
            }
        }
    }
}
