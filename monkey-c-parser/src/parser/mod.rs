mod decl;
mod expr;
mod stmt;
mod types;

use crate::ast::{Ast, CommentStmt, CommentTable, Span};
use crate::line_index::LineIndex;
use crate::token;

/// A parse error with the source location where it occurred.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParserError {
    pub message: String,
    /// 1-indexed line number of the offending token.
    pub line: u32,
    /// 1-indexed column number of the offending token.
    pub col: u32,
    /// Byte span of the offending token, for rendering a source snippet.
    pub span: std::ops::Range<usize>,
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for ParserError {}

/// Output of [`Parser::parse`]. Carries both the AST and the source-order comment table.
#[derive(Debug)]
pub struct ParseOutput {
    pub ast: Ast,
    pub comments: CommentTable,
}

/// Recursive-descent parser for Monkey C source text.
///
/// Construct with [`Parser::new`], then call [`Parser::parse`] to produce an [`Ast::Document`].
pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
    pub(crate) line_index: LineIndex,
    /// The token currently being examined.
    pub current_token: token::Type,
    /// Byte offset of the start of `current_token`.
    pub(crate) current_token_start: usize,
    /// Byte offset of the end (exclusive) of `current_token`.
    pub(crate) current_token_end: usize,
    /// Byte offset of the end (exclusive) of the token consumed *before* `current_token`. Useful
    /// for callers that want "position right after the last consumed real token", e.g. the end of a
    /// parsed type's last token, used to bound the `BeforeBracket` comment slot on a function with
    /// a return type.
    pub(crate) prev_token_end: usize,
    /// Every comment encountered during parsing, in source order. Populated at the lexer-parser
    /// boundary by `next_token_span`, comments never reach `current_token`.
    pub(crate) comment_table: CommentTable,
    /// Byte length of the source — used to build the synthetic `Ast::Document` span so top-level
    /// standalone comments can attach as dangling-inside.
    pub(crate) source_len: usize,
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
            comment_table: CommentTable::new(),
            source_len: source.len(),
        };

        // Prime the first token; comment tokens are auto-drained into the table by
        // `next_token_span`.
        parser.next_token_span();
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

    pub(crate) fn is_expr_start(tok: &token::Type) -> bool {
        matches!(
            tok,
            token::Type::Bang
                | token::Type::Bling
                | token::Type::Boolean(_)
                | token::Type::Char(_)
                | token::Type::Colon
                | token::Type::Double(_)
                | token::Type::Float(_)
                | token::Type::Hex(_)
                | token::Type::HexLong(_)
                | token::Type::Identifier(_)
                | token::Type::LBrace
                | token::Type::LBracket
                | token::Type::LParen
                | token::Type::Long(_)
                | token::Type::Me
                | token::Type::Minus
                | token::Type::MinusMinus
                | token::Type::NaN
                | token::Type::New
                | token::Type::Null
                | token::Type::Number(_)
                | token::Type::Plus
                | token::Type::PlusPlus
                | token::Type::Self_
                | token::Type::String(_)
                | token::Type::Tilde
        )
    }

    pub(crate) fn assert_next_token(
        &mut self,
        expect: &[token::Type],
    ) -> Result<token::Type, ParserError> {
        for expected in expect {
            if self.current_token == *expected {
                let tkn = self.current_token.clone();
                self.next_token_span();

                return Ok(tkn);
            }
        }

        Err(self.parse_error(format!(
            "got unexpected token from `assert_next_token`: '{:?}', expected one of: {:?}",
            self.current_token, expect
        )))
    }

    pub(crate) fn current_token_is(&self, expect: &[token::Type]) -> bool {
        expect.contains(&self.current_token)
    }

    /// Monkey C compiler treats consecutive `;` as empty statement making them no-op and thus
    /// supporting syntax like `var x = 1;;;`.
    fn skip_semicolons(&mut self) {
        while self.current_token == token::Type::Semicolon {
            self.next_token_span();
        }
    }

    /// Advance past `current_token`, drain any intervening comments into the side-table, and update
    /// `current_token` to the next non-comment token. The grammar code therefore never observes
    /// comments — they're invisible at the parser/lexer boundary.
    pub(crate) fn next_token_span(&mut self) -> (usize, token::Type, usize) {
        loop {
            let (start, token_type, end) = self.lexer.next_token();
            match &token_type {
                token::Type::Comment(text) => {
                    self.comment_table.push(CommentStmt {
                        text: text.clone(),
                        is_block: false,
                        span: Span { start, end },
                    });
                }
                token::Type::BlockComment(text) => {
                    self.comment_table.push(CommentStmt {
                        text: text.clone(),
                        is_block: true,
                        span: Span { start, end },
                    });
                }
                _ => {
                    self.prev_token_end = self.current_token_end;
                    self.current_token = token_type.clone();
                    self.current_token_start = start;
                    self.current_token_end = end;
                    return (start, token_type, end);
                }
            }
        }
    }

    /// Consume the `>` that closes a generic argument list, splitting a greedy `>>` (`RightShift`)
    /// token in half so nested generics like `Array<List<T>>` close correctly. The remaining `>`
    /// becomes the new `current_token` for the outer level to consume.
    pub(crate) fn consume_generic_close(&mut self) -> Result<(), ParserError> {
        match self.current_token {
            token::Type::Greater => {
                self.next_token_span();
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
            self.next_token_span();
            "$".to_string()
        } else {
            let n = self.parse_identifier()?;
            self.next_token_span();
            n
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
            self.next_token_span(); // consume dot
            let next = self.parse_identifier()?;
            name.push('.');
            name.push_str(&next);
            self.next_token_span();
        }

        Ok(name)
    }

    pub(crate) fn parse_identifier(&mut self) -> Result<String, ParserError> {
        match &self.current_token {
            token::Type::Identifier(name) => Ok(name.clone()),
            _ => {
                Err(self.parse_error(format!("Expected identifier, got {:?}", self.current_token)))
            }
        }
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

        self.next_token_span();

        Ok(name)
    }
}
