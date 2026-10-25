mod decl;
mod expr;
mod stmt;
mod types;

use crate::ast::{Ast, CommentStmt, CommentTable, Ident, Position, Span, Spanned, Type};
use crate::line_index::LineIndex;
use crate::token;

/// A parse error with the source location where it occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    pub message: String,
    /// The line of the offending token, counted from 1.
    pub line: u32,
    /// The column of the offending token in bytes, counted from 1.
    pub col: u32,
    pub span: std::ops::Range<usize>,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for ParserError {}

/// What [`Parser::parse`] returns: the [`Ast::Document`] and every comment in source order.
#[derive(Debug)]
pub struct ParseOutput {
    pub ast: Ast,
    pub comments: CommentTable,
}

/// Recursive-descent parser for Monkey C. Parsing stops at the first error.
pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
    pub(crate) line_index: LineIndex,
    /// The token being looked at, never a comment.
    pub current_token: token::Type,
    pub(crate) current_token_start: usize,
    pub(crate) current_token_end: usize,
    pub(crate) prev_token_end: usize,
    /// Comments are collected here at the lexer boundary so the grammar never sees them.
    pub(crate) comment_table: CommentTable,
    pub(crate) source_len: usize,
    pub(crate) expression_depth: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let line_index = LineIndex::new(source);
        let lexer = crate::lexer::Lexer::new(source);
        let mut parser = Self {
            lexer,
            line_index,
            current_token: token::Type::Eof,
            current_token_start: 0,
            current_token_end: 0,
            prev_token_end: 0,
            comment_table: CommentTable::default(),
            source_len: source.len(),
            expression_depth: 0,
        };

        parser.advance();
        parser
    }

    /// Parse a complete source file into an [`Ast::Document`] and the [`CommentTable`] of source
    /// comments.
    pub fn parse(mut self) -> Result<ParseOutput, ParserError> {
        let mut nodes = Vec::new();

        loop {
            self.skip_semicolons();

            let decl = self.parse_declaration()?;
            if decl == Ast::Eof {
                break;
            }

            nodes.push(decl);
        }

        let span = Span {
            start: 0,
            end: self.source_len,
        };

        Ok(ParseOutput {
            ast: Ast::Document(nodes, span),
            comments: self.comment_table,
        })
    }

    /// Consume `expected` and return its span, or fail if the current token is something else.
    pub(crate) fn expect(&mut self, expected: token::Type) -> Result<Span, ParserError> {
        if self.current_token != expected {
            return Err(self.parse_error(format!(
                "Expected '{expected}', got {:?}",
                self.current_token
            )));
        }

        Ok(self.advance())
    }

    /// Monkey C compiler treats consecutive `;` as empty statement making them no-op and thus
    /// supporting syntax like `var x = 1;;;`.
    fn skip_semicolons(&mut self) {
        while self.current_token == token::Type::Semicolon {
            self.advance();
        }
    }

    /// Consume the current token and return its span, collecting any comments before the next one.
    pub(crate) fn advance(&mut self) -> Span {
        let consumed = Span {
            start: self.current_token_start,
            end: self.current_token_end,
        };

        loop {
            let (start, token_type, end) = self.lexer.next_token();
            let (text, is_block) = match token_type {
                token::Type::Comment(text) => (text, false),
                token::Type::BlockComment(text) => (text, true),
                token_type => {
                    self.prev_token_end = self.current_token_end;
                    self.current_token = token_type;
                    self.current_token_start = start;
                    self.current_token_end = end;

                    return consumed;
                }
            };

            self.comment_table.comments.push(CommentStmt {
                text,
                is_block,
                span: Span { start, end },
            });
        }
    }

    /// Consume the `>` that closes a generic argument list, splitting a greedy `>>` (`RightShift`)
    /// token in half so nested generics like `Array<List<T>>` close correctly. The remaining `>`
    /// becomes the new `current_token` for the outer level to consume.
    pub(crate) fn consume_generic_close(&mut self) -> Result<(), ParserError> {
        match self.current_token {
            token::Type::Greater => {
                self.advance();
                Ok(())
            }
            token::Type::RightShift => {
                let mid = self.current_token_start + 1;
                self.prev_token_end = mid;
                self.current_token = token::Type::Greater;
                self.current_token_start = mid;
                Ok(())
            }
            _ => Err(self.parse_error(format!("Expected '>', got {:?}", self.current_token))),
        }
    }

    /// Build a [`ParserError`] pointing at the start of `current_token`.
    pub(crate) fn parse_error(&self, message: impl Into<String>) -> ParserError {
        let lc = self.line_index.line_col(self.current_token_start as u32);

        ParserError {
            message: message.into(),
            line: lc.line + 1,
            col: lc.col + 1,
            span: self.current_token_start..self.current_token_end,
        }
    }

    pub(crate) fn parse_dotted_identifier(&mut self) -> Result<String, ParserError> {
        let name = if self.current_token == token::Type::Bling {
            self.advance();
            "$".to_string()
        } else {
            self.parse_identifier()?
        };

        self.parse_dotted_identifier_continuation(name)
    }

    /// Continue a dotted identifier after its first segment has already been consumed, e.g. `self`
    /// in `self.classDef_`.
    pub(crate) fn parse_dotted_identifier_continuation(
        &mut self,
        mut name: String,
    ) -> Result<String, ParserError> {
        while self.current_token == token::Type::Dot {
            self.advance();
            name.push('.');
            name.push_str(&self.parse_identifier()?);
        }

        Ok(name)
    }

    /// Wrap `node` in a span from `start` to the end of the last consumed token.
    pub(crate) fn spanned<T>(&self, start: Position, node: T) -> Spanned<T> {
        Spanned {
            span: Span {
                start,
                end: self.prev_token_end,
            },
            node,
        }
    }

    pub(crate) fn parse_spanned_identifier(&mut self) -> Result<Spanned<Ident>, ParserError> {
        let start = self.current_token_start;
        let name = self.parse_identifier()?;

        Ok(self.spanned(start, name))
    }

    pub(crate) fn parse_spanned_dotted_identifier(
        &mut self,
    ) -> Result<Spanned<Ident>, ParserError> {
        let start = self.current_token_start;
        let name = self.parse_dotted_identifier()?;

        Ok(self.spanned(start, name))
    }

    /// Parse an optional `as Type`, returning where `as` was written along with the type.
    pub(crate) fn parse_type_annotation(
        &mut self,
    ) -> Result<(Option<Position>, Option<Type>), ParserError> {
        if self.current_token != token::Type::As {
            return Ok((None, None));
        }

        let as_kw_start = self.advance().start;

        Ok((Some(as_kw_start), Some(self.parse_type()?)))
    }

    /// Consume an identifier and return its name.
    pub(crate) fn parse_identifier(&mut self) -> Result<String, ParserError> {
        let token::Type::Identifier(name) = &self.current_token else {
            return Err(
                self.parse_error(format!("Expected identifier, got {:?}", self.current_token))
            );
        };

        let name = name.clone();
        self.advance();

        Ok(name)
    }

    /// Read the current token as a symbol name, accepting both `Identifier` and keyword tokens.
    /// Keywords are valid symbol names (e.g. `:hidden`, `:private`) — the Display text is their
    /// canonical string.
    pub(crate) fn parse_symbol_name(&mut self) -> Result<String, ParserError> {
        let name = match &self.current_token {
            token::Type::Identifier(name) => name.clone(),
            // Keywords can be valid symbol names, e.g (:private)
            tok => {
                let s = tok.to_string();
                if s.starts_with(|c: char| c.is_ascii_alphabetic() || c == '_') {
                    s
                } else {
                    return Err(self.parse_error(format!(
                        "Expected name after `:` for symbol, got {:?}",
                        self.current_token
                    )));
                }
            }
        };

        self.advance();

        Ok(name)
    }
}
