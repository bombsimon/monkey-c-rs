use monkey_c_parser::ast::Span;
use monkey_c_parser::line_index::LineIndex;

use crate::ast::{
    Assignment, Comment, Entry, JungleFile, LiteralPart, QualifiedName, Text, Value, ValueKind,
};
use crate::lexer::Lexer;
use crate::token;

/// Reusing the Monkey C error type lets jungle errors render through the same
/// `monkey-c-diagnostics` reporting.
pub use monkey_c_parser::parser::ParserError;

/// Recursive-descent parser for jungle files. Usually reached through
/// [`JungleFile::parse`](crate::ast::JungleFile::parse), which wraps it.
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    line_index: LineIndex,
    current_token: token::Type,
    current_token_start: usize,
    current_token_end: usize,
    /// End of the previous token, so a node's span can stop there rather than running on to the
    /// newline that terminated it.
    prev_token_end: usize,
    /// Comments drained by `next_token` and not yet placed. The grammar never sees one.
    pending_comments: Vec<Comment>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut parser = Self {
            lexer: Lexer::new(source),
            line_index: LineIndex::new(source),
            current_token: token::Type::Eof,
            current_token_start: 0,
            current_token_end: 0,
            prev_token_end: 0,
            pending_comments: Vec::new(),
        };

        parser.next_token();

        parser
    }

    /// Parse a complete jungle file.
    pub fn parse(mut self) -> Result<JungleFile, ParserError> {
        let mut entries = Vec::new();

        loop {
            // Comments left over from the previous instruction had no value to attach to, so they
            // land here, between entries.
            entries.extend(self.take_comments().into_iter().map(Entry::Comment));

            match &self.current_token {
                token::Type::Eof => break,
                token::Type::Newline => {
                    self.next_token();

                    if !matches!(entries.last(), Some(Entry::BlankLine)) {
                        entries.push(Entry::BlankLine);
                    }
                }
                _ => entries.push(Entry::Assignment(self.parse_assignment()?)),
            }
        }

        Ok(JungleFile { entries })
    }

    fn parse_assignment(&mut self) -> Result<Assignment, ParserError> {
        let start = self.current_token_start;
        let target = self.parse_target()?;

        if self.current_token != token::Type::Assign {
            return Err(self.parse_error(format!(
                "expected `=` after the qualifier name, got `{}`",
                self.current_token
            )));
        }

        self.next_token();

        // The value list may start on a line below the `=`.
        self.skip_newlines();

        let values = self.parse_value_list()?;
        let end = self.prev_token_end;

        self.end_of_line()?;

        Ok(Assignment {
            target,
            values,
            span: Span { start, end },
        })
    }

    fn parse_target(&mut self) -> Result<QualifiedName, ParserError> {
        let token::Type::Word(word) = &self.current_token else {
            return Err(self.parse_error(format!(
                "expected a qualifier name, got `{}`",
                self.current_token
            )));
        };

        let word = word.clone();
        let span = self.current_span();
        let name = self.qualified_name(&word, span)?;

        self.next_token();

        Ok(name)
    }

    /// Parse the `;`-separated values right of the `=`, or the contents of a `[…]` group.
    ///
    /// Every position must hold a value: `monkeyc` rejects a bare `qualifier =`, a trailing `;`, a
    /// gap between two `;` and an empty `[]` alike.
    fn parse_value_list(&mut self) -> Result<Vec<Value>, ParserError> {
        let mut values = Vec::new();

        loop {
            let mut value = self.parse_value()?;

            // Comments read while finishing the value describe it, whether they came before or
            // after the `;`.
            value.comments = self.take_comments();

            let separated = self.current_token == token::Type::Semicolon;
            if separated {
                self.next_token();
                value.comments.extend(self.take_comments());
            }

            values.push(value);

            if !separated {
                return Ok(values);
            }
        }
    }

    fn skip_newlines(&mut self) {
        while self.current_token == token::Type::Newline {
            self.next_token();
        }
    }

    fn parse_value(&mut self) -> Result<Value, ParserError> {
        let start = self.current_token_start;

        let kind = if self.current_token == token::Type::LBracket {
            ValueKind::Group(self.parse_group()?)
        } else {
            ValueKind::Literal(self.parse_literal_parts()?)
        };

        Ok(Value {
            kind,
            comments: Vec::new(),
            span: Span {
                start,
                end: self.prev_token_end,
            },
        })
    }

    fn parse_group(&mut self) -> Result<Vec<Value>, ParserError> {
        self.next_token(); // consume `[`

        let values = self.parse_value_list()?;

        if self.current_token != token::Type::RBracket {
            return Err(self.parse_error(format!(
                "expected `]` to close the value group, got `{}`",
                self.current_token
            )));
        }

        self.next_token(); // consume `]`

        Ok(values)
    }

    /// Parse the parts of one value. Only a `;` starts a new value, so adjacent text and
    /// dereferences accumulate: `$(base.resourcePath)/shared` is one value of two parts.
    fn parse_literal_parts(&mut self) -> Result<Vec<LiteralPart>, ParserError> {
        let mut parts = Vec::new();

        loop {
            let span = self.current_span();
            let part = match &self.current_token {
                token::Type::Word(text) => LiteralPart::Text(Text {
                    text: text.clone(),
                    quoted: false,
                    span,
                }),
                token::Type::QuotedWord(text) => LiteralPart::Text(Text {
                    text: text.clone(),
                    quoted: true,
                    span,
                }),
                token::Type::Deref(name) => {
                    let name = name.clone();

                    LiteralPart::Reference(self.qualified_name(&name, span)?)
                }
                token::Type::UnclosedString(text) => {
                    let text = text.clone();

                    return Err(
                        self.parse_error(format!("expected a closing `\"` for the value `{text}`"))
                    );
                }
                token::Type::UnclosedDeref(text) => {
                    let text = text.clone();

                    return Err(self.parse_error(format!("expected a closing `)` for `{text}`")));
                }
                _ => break,
            };

            parts.push(part);
            self.next_token();
        }

        if parts.is_empty() {
            return Err(self.parse_error(format!("expected a value, got `{}`", self.current_token)));
        }

        Ok(parts)
    }

    /// Split a dotted name, rejecting the empty segment a typo like `fenix5..sourcePath` leaves.
    fn qualified_name(&self, text: &str, span: Span) -> Result<QualifiedName, ParserError> {
        let segments: Vec<String> = text.split('.').map(str::to_string).collect();

        if segments.iter().any(String::is_empty) {
            return Err(self.error_at(
                span,
                format!("`{text}` is not a valid name — it has an empty segment"),
            ));
        }

        Ok(QualifiedName { segments, span })
    }

    fn take_comments(&mut self) -> Vec<Comment> {
        std::mem::take(&mut self.pending_comments)
    }

    /// Consume the newline that terminates a build instruction.
    fn end_of_line(&mut self) -> Result<(), ParserError> {
        match self.current_token {
            token::Type::Eof => Ok(()),
            token::Type::Newline => {
                self.next_token();

                Ok(())
            }
            _ => Err(self.parse_error(format!(
                "expected end of line, got `{}`",
                self.current_token
            ))),
        }
    }

    /// Advance, collecting comments on the way. Draining them here is what lets an instruction
    /// carry on past the line a comment ended.
    fn next_token(&mut self) {
        loop {
            let (start, token, end) = self.lexer.next_token();

            if let token::Type::Comment(text) = token {
                self.pending_comments.push(Comment {
                    text,
                    span: Span { start, end },
                });

                continue;
            }

            self.prev_token_end = self.current_token_end;
            self.current_token = token;
            self.current_token_start = start;
            self.current_token_end = end;

            return;
        }
    }

    fn current_span(&self) -> Span {
        Span {
            start: self.current_token_start,
            end: self.current_token_end,
        }
    }

    fn parse_error(&self, message: impl Into<String>) -> ParserError {
        self.error_at(self.current_span(), message)
    }

    fn error_at(&self, span: Span, message: impl Into<String>) -> ParserError {
        let position = self.line_index.line_col(span.start as u32);

        ParserError {
            message: message.into(),
            line: position.line + 1,
            col: position.col + 1,
            span: span.start..span.end,
        }
    }
}
