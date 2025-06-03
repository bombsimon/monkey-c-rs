#![allow(dead_code)]
use crate::token::{self, Span};
use std::str::FromStr;

#[derive(Debug)]
pub(crate) struct Lexer<'a> {
    input: std::str::Chars<'a>,
    position: usize,
    peek: Option<token::Span>,
    ch0: Option<char>,
    ch1: Option<char>,
    ch2: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Lexer<'a> {
        let mut lexer = Lexer {
            input: input.chars(),
            position: 0,
            peek: None,
            ch0: None,
            ch1: None,
            ch2: None,
        };

        lexer.next_char();
        lexer.next_char();
        lexer.next_char();
        lexer.position = 0;

        lexer
    }

    fn next_char(&mut self) {
        self.ch0 = self.ch1;
        self.ch1 = self.ch2;
        self.ch2 = self.input.next();

        self.position += 1;
    }

    pub(crate) fn next_token(&mut self) -> Span {
        // If we peeked a token it's already processed so we return that one and reset spen sapn.
        if let Some(peeked_span) = &self.peek {
            let span = peeked_span.clone();
            self.peek = None;

            return span;
        }

        self.skip_whitespace();

        let start_position = self.position;

        if self.ch0.is_none() {
            return (start_position, token::Type::Eof, self.position);
        }

        let current_ch = self.ch0.unwrap();

        match current_ch {
            '\n' => {
                self.next_char();
                (start_position, token::Type::Newline, self.position)
            }
            '(' if self.ch1 == Some(':') => {
                // Parse annotation (:annotation)
                let annotation = self.parse_annotation();
                (
                    start_position,
                    token::Type::Annotation(annotation),
                    self.position,
                )
            }
            '/' if self.ch1.unwrap() == '/' => {
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
            c if c.is_ascii_alphabetic() || c == '_' => {
                let token_type = self.parse_identifier();
                (start_position, token_type, self.position)
            }
            ',' | ':' | ';' | '.' | '?' => {
                self.next_char();
                (
                    start_position,
                    token::Type::from_str(&current_ch.to_string()).unwrap(),
                    self.position,
                )
            }
            '(' => {
                // Check for annotation pattern (:
                if let Some(':') = self.ch1 {
                    let annotation = self.parse_annotation();
                    (
                        start_position,
                        token::Type::Annotation(annotation),
                        self.position,
                    )
                } else {
                    self.next_char();
                    (
                        start_position,
                        token::Type::from_str(&current_ch.to_string()).unwrap(),
                        self.position,
                    )
                }
            }
            ')' | '[' | ']' | '{' | '}' => {
                self.next_char();
                (
                    start_position,
                    token::Type::from_str(&current_ch.to_string()).unwrap(),
                    self.position,
                )
            }
            '+' | '-' | '*' | '/' | '%' => {
                self.next_char();

                match (current_ch, self.ch0) {
                    (_, Some(next)) if next == '=' => {
                        self.next_char();

                        let mut c = String::new();
                        c.push(current_ch);
                        c.push(next);

                        (
                            start_position,
                            token::Type::from_str(&c).unwrap(),
                            self.position,
                        )
                    }
                    ('+', Some('+')) => {
                        self.next_char();
                        (start_position, token::Type::Increment, self.position)
                    }
                    ('-', Some('-')) => {
                        self.next_char();
                        (start_position, token::Type::Decrement, self.position)
                    }
                    _ => (
                        start_position,
                        token::Type::from_str(&current_ch.to_string()).unwrap(),
                        self.position,
                    ),
                }
            }
            '<' | '>' => {
                self.next_char();

                let mut s = String::new();

                match (current_ch, self.ch0, self.ch1) {
                    (_, Some('='), _) => {
                        self.next_char();

                        s.push(current_ch);
                        s.push('=');
                    }
                    // <<= and >>=
                    (a, Some(b), Some(c)) if a == b && a != c && c == '=' => {
                        self.next_char();
                        self.next_char();

                        s.push(a);
                        s.push(b);
                        s.push(c);
                    }
                    ('<', _, _) => return (start_position, token::Type::Less, self.position),
                    ('>', _, _) => return (start_position, token::Type::Greater, self.position),
                    (_, _, _) => s = current_ch.to_string(),
                };

                (
                    start_position,
                    token::Type::from_str(&s).unwrap(),
                    self.position,
                )
            }
            '=' => {
                self.next_char();

                let s = match self.ch0 {
                    Some('=') => {
                        self.next_char();
                        "=="
                    }
                    _ => "=",
                };

                (
                    start_position,
                    token::Type::from_str(s).unwrap(),
                    self.position,
                )
            }
            _ => panic!("Unexpected character: {}", self.ch2.unwrap()),
        }
    }

    pub(crate) fn peek_token(&mut self) -> Span {
        if let Some(peeked_span) = &self.peek {
            return peeked_span.clone();
        }

        let next = self.next_token();
        self.peek = Some(next.clone());

        next
    }

    fn parse_identifier(&mut self) -> token::Type {
        let mut ident = String::new();

        while let Some(c) = self.ch0 {
            if !c.is_ascii_alphabetic() && !c.is_ascii_digit() && c != '_' {
                break;
            }

            ident.push(c);
            self.next_char();
        }

        if let Ok(tt) = token::Type::from_str(&ident) {
            tt
        } else {
            token::Type::Identifier(ident)
        }
    }

    fn parse_string(&mut self) -> String {
        let mut string = String::new();
        let mut is_escape = false;

        // Advance to skip the `"`
        self.next_char();
        while let Some(c) = self.ch0 {
            if c == '"' && !is_escape {
                // Consume the closing \"
                self.next_char();
                break;
            }

            if c == '\\' {
                is_escape = !is_escape;
            } else {
                is_escape = false;
            }

            string.push(c);
            self.next_char();
        }

        string
    }

    fn parse_comment(&mut self) -> String {
        let mut comment = String::new();

        while let Some(c) = self.ch0 {
            if c == '\n' {
                break;
            }

            comment.push(c);
            self.next_char();
        }

        comment
    }

    fn parse_annotation(&mut self) -> String {
        let mut annotation = String::new();

        // Skip the opening '(:'
        self.next_char(); // skip '('
        self.next_char(); // skip ':'

        while let Some(c) = self.ch0 {
            if c == ')' {
                self.next_char(); // consume the closing ')'
                break;
            }

            annotation.push(c);
            self.next_char();
        }

        annotation
    }

    fn parse_number(&mut self) -> (f64, bool) {
        let mut has_fraction = false;
        let mut number = String::new();

        while let Some(c) = self.ch0 {
            if c == '.' {
                has_fraction = true;
            }

            if !c.is_ascii_digit() && c != '.' {
                break;
            }

            number.push(c);
            self.next_char();
        }

        (number.parse().unwrap(), has_fraction)
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.ch0 {
            if c.is_whitespace() && c != '\n' {
            } else {
                break;
            }

            self.next_char();
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

            if token_type == crate::token::Type::Eof {
                break;
            }
        }
    }
}
