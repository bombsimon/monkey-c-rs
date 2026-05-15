use crate::ast::{
    Ast, BlockStmt, ClassDecl, ConstDecl, ElseBranch, ForInit, ForStmt, FunctionDecl, IfStmt,
    ImportDecl, ModuleDecl, ReturnStmt, Span, Stmt, Type, VarDecl, Variable, Visibility, WhileStmt,
};
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
}

impl std::fmt::Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.line, self.col, self.message)
    }
}

impl std::error::Error for ParserError {}

/// Recursive-descent parser for Monkey C source text.
///
/// Construct with [`Parser::new`], then call [`Parser::parse`] to produce an
/// [`Ast::Document`].
pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
    pub(crate) line_index: LineIndex,
    /// The token currently being examined.
    pub current_token: token::Type,
    /// Byte offset of the start of `current_token`.
    pub(crate) current_token_start: usize,
    /// Byte offset of the end (exclusive) of `current_token`.
    pub(crate) current_token_end: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let line_index = LineIndex::new(source);
        let mut lexer = crate::lexer::Lexer::new(source);
        let (start, token_type, end) = lexer.next_token();

        Self {
            lexer,
            line_index,
            current_token: token_type,
            current_token_start: start,
            current_token_end: end,
        }
    }

    /// Parse a complete source file into an [`Ast::Document`].
    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut nodes = Vec::new();

        loop {
            let decl = self.parse_declaration()?;
            if decl == Ast::Eof {
                break;
            }

            nodes.push(decl);
        }

        Ok(Ast::Document(nodes))
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

    /// Consume any line/block comment tokens at the current position and
    /// return them as `Stmt::Comment` / `Stmt::BlockComment`. Stops at the
    /// first non-comment token. Used to absorb trailing comments inside
    /// delimited lists (dicts, arrays, args).
    pub(crate) fn consume_trailing_comments(&mut self) -> Vec<Stmt> {
        let mut comments = Vec::new();
        loop {
            let start = self.current_token_start;
            let end = self.current_token_end;
            let stmt = match self.current_token.clone() {
                token::Type::Comment(content) => {
                    self.next_token_span();
                    Stmt::Comment(content, Span { start, end })
                }
                token::Type::BlockComment(content) => {
                    self.next_token_span();
                    Stmt::BlockComment(content, Span { start, end })
                }
                _ => return comments,
            };
            comments.push(stmt);
        }
    }

    pub(crate) fn next_token_span(&mut self) -> (usize, token::Type, usize) {
        let (start, token_type, end) = self.lexer.next_token();
        self.current_token = token_type.clone();
        self.current_token_start = start;
        self.current_token_end = end;

        (start, token_type, end)
    }

    /// Build a [`ParserError`] pointing at the start of `current_token`.
    pub(crate) fn parse_error(&self, message: impl Into<String>) -> ParserError {
        let lc = self.line_index.line_col(self.current_token_start as u32);

        ParserError {
            message: message.into(),
            line: lc.line + 1,
            col: lc.col + 1,
        }
    }

    pub(crate) fn parse_dotted_identifier(&mut self) -> Result<String, ParserError> {
        let mut name = self.parse_identifier()?;
        self.next_token_span();

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

    /// Parse a full type, including `or`-joined union alternatives at the top level.
    ///
    /// Use this for variable/parameter/return type annotations.
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut ty = self.parse_simple_type()?;
        while self.current_token == token::Type::Or {
            self.next_token_span(); // consume `or`
            ty.alternatives.push(self.parse_simple_type()?);
        }

        Ok(ty)
    }

    /// Parse a single type name with optional generic params and `?`, but
    /// without consuming `or` alternatives.
    ///
    /// Used for generic parameter lists where `or` is a param separator, not a
    /// union operator: `Array<Number or Float>` → two params, not one union.
    fn parse_simple_type(&mut self) -> Result<Type, ParserError> {
        let ident = self.parse_identifier()?;
        if self.current_token != token::Type::Less {
            self.next_token_span();
        }

        let generic_params = if self.current_token == token::Type::Less {
            self.next_token_span(); // consume <
            let mut params = Vec::new();

            if self.current_token != token::Type::Greater {
                params.push(self.parse_simple_type()?);
                while self.is_type_separator() {
                    self.next_token_span(); // consume , or `or`
                    params.push(self.parse_simple_type()?);
                }
            }

            self.assert_next_token(&[token::Type::Greater])?;

            params
        } else {
            Vec::new()
        };

        let optional = self.current_token == token::Type::Question;
        if optional {
            self.next_token_span(); // consume ?
        }

        Ok(Type {
            ident,
            generic_params,
            alternatives: Vec::new(),
            optional,
        })
    }

    /// Returns `true` if the current token is a type-separator inside a generic
    /// parameter list: either `,` or the contextual keyword `or`.
    fn is_type_separator(&self) -> bool {
        matches!(self.current_token, token::Type::Comma | token::Type::Or)
    }

    fn parse_declaration(&mut self) -> Result<Ast, ParserError> {
        let start = self.current_token_start;
        let mut visibility = None;
        let mut is_static = false;

        loop {
            match self.current_token {
                token::Type::Private => {
                    visibility = Some(Visibility::Private);
                    self.next_token_span();
                }
                token::Type::Protected => {
                    visibility = Some(Visibility::Protected);
                    self.next_token_span();
                }
                token::Type::Public => {
                    visibility = Some(Visibility::Public);
                    self.next_token_span();
                }
                token::Type::Static => {
                    is_static = true;
                    self.next_token_span();
                }
                token::Type::Hidden => {
                    visibility = Some(Visibility::Hidden);
                    self.next_token_span();
                }
                _ => break,
            }
        }

        match self.current_token.clone() {
            token::Type::Annotation(content) => self.parse_annotation_decl(start, content),
            token::Type::Class => self.parse_class_decl(start),
            token::Type::Comment(content) => self.parse_comment_decl(start, content),
            token::Type::BlockComment(content) => self.parse_block_comment_decl(start, content),
            token::Type::Const => self.parse_const_decl(start, visibility, is_static),
            token::Type::Function => self.parse_function_decl(start, visibility, is_static),
            token::Type::Import => self.parse_import_decl(start),
            token::Type::Module => self.parse_module_decl(start),
            token::Type::Var => self.parse_var_decl(start, visibility, is_static),
            token::Type::Eof => Ok(Ast::Eof),
            _ => Err(self.parse_error(format!(
                "Unexpected token at top level: {:?}",
                self.current_token
            ))),
        }
    }

    fn parse_annotation_decl(&mut self, start: usize, content: String) -> Result<Ast, ParserError> {
        let end = self.current_token_end;
        self.next_token_span();

        Ok(Ast::Annotation(content, Span { start, end }))
    }

    fn parse_class_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span();
        let name = self.parse_identifier()?;
        self.next_token_span();

        let extends = if self.current_token == token::Type::Extends {
            self.next_token_span();
            let n = self.parse_dotted_identifier()?;

            Some(n)
        } else {
            None
        };

        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Class(ClassDecl {
            name,
            extends,
            body,
            span: Span {
                start,
                end: rbrace_end,
            },
        }))
    }

    fn parse_class_body(&mut self) -> Result<Vec<Ast>, ParserError> {
        let mut body = Vec::new();
        while self.current_token != token::Type::RBrace {
            let member = self.parse_declaration()?;
            body.push(member);
        }

        Ok(body)
    }

    fn parse_comment_decl(&mut self, start: usize, content: String) -> Result<Ast, ParserError> {
        let end = self.current_token_end;
        self.next_token_span();

        Ok(Ast::Comment(content, Span { start, end }))
    }

    fn parse_block_comment_decl(
        &mut self,
        start: usize,
        content: String,
    ) -> Result<Ast, ParserError> {
        let end = self.current_token_end;
        self.next_token_span();

        Ok(Ast::BlockComment(content, Span { start, end }))
    }

    fn parse_const_decl(
        &mut self,
        start: usize,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<Ast, ParserError> {
        self.next_token_span();
        let name = self.parse_identifier()?;
        self.next_token_span();

        let type_ = if self.current_token == token::Type::As {
            self.next_token_span();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.assert_next_token(&[token::Type::Assign])?;
        let initializer = self.parse_expression()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Const(ConstDecl {
            name,
            type_,
            visibility,
            initializer,
            is_static,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_function_decl(
        &mut self,
        start: usize,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<Ast, ParserError> {
        self.next_token_span();
        let name = self.parse_identifier()?;
        self.next_token_span();
        let (args, args_tail_comments) = self.parse_function_args()?;

        let returns = if self.current_token == token::Type::As {
            self.next_token_span();
            Some(self.parse_type()?)
        } else {
            None
        };

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(Ast::Function(FunctionDecl {
            name,
            args,
            args_tail_comments,
            returns,
            body,
            visibility,
            is_static,
            span: Span { start, end },
        }))
    }

    fn parse_import_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span();
        let name = self.parse_dotted_identifier()?;
        let alias = if self.current_token == token::Type::As {
            self.next_token_span(); // consume `as`
            let alias_name = self.parse_identifier()?;
            self.next_token_span(); // advance past alias name
            Some(alias_name)
        } else {
            None
        };
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Import(ImportDecl {
            name,
            alias,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_module_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `module`
        let name = self.parse_identifier()?;
        self.next_token_span(); // advance past name

        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Module(ModuleDecl {
            name,
            body,
            span: Span {
                start,
                end: rbrace_end,
            },
        }))
    }

    fn parse_var_decl(
        &mut self,
        start: usize,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<Ast, ParserError> {
        self.next_token_span();
        let mut var_decl = self.parse_var_contents(visibility, is_static)?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;
        var_decl.span = Span {
            start,
            end: semi_end,
        };

        Ok(Ast::Variable(var_decl))
    }

    fn parse_function_args(&mut self) -> Result<(Vec<Variable>, Vec<Stmt>), ParserError> {
        self.assert_next_token(&[token::Type::LParen])?;
        let mut args: Vec<Variable> = Vec::new();
        let mut tail_comments: Vec<Stmt> = Vec::new();

        loop {
            let pending = self.consume_trailing_comments();

            if self.current_token == token::Type::RParen {
                if let Some(last) = args.last_mut() {
                    last.trailing_comments.extend(pending);
                } else {
                    tail_comments = pending;
                }
                break;
            }

            if let Some(last) = args.last_mut() {
                last.trailing_comments.extend(pending);
            } else if !pending.is_empty() {
                tail_comments.extend(pending);
            }

            let name = self.parse_identifier()?;
            self.next_token_span();

            let type_ = if self.current_token == token::Type::As {
                self.next_token_span();
                Some(self.parse_type()?)
            } else {
                None
            };

            let mut arg = Variable {
                name,
                type_,
                visibility: None,
                initializer: None,
                is_static: false,
                trailing_comments: Vec::new(),
            };
            arg.trailing_comments
                .extend(self.consume_trailing_comments());

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                arg.trailing_comments
                    .extend(self.consume_trailing_comments());
                args.push(arg);
                if self.current_token == token::Type::RParen {
                    break;
                }
            } else if self.current_token == token::Type::RParen {
                args.push(arg);
                break;
            } else {
                return Err(self.parse_error(format!(
                    "Expected ',' or ')' in function arguments, got {:?}",
                    self.current_token
                )));
            }
        }

        self.next_token_span(); // consume RParen

        Ok((args, tail_comments))
    }

    /// Parse the contents of a `var` declaration after the `var` keyword has been consumed.
    /// Does NOT consume a trailing semicolon — callers set the final span after consuming it.
    fn parse_var_contents(
        &mut self,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<VarDecl, ParserError> {
        let name = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier

        let type_ = if self.current_token == token::Type::As {
            self.next_token_span();
            Some(self.parse_type()?)
        } else {
            None
        };

        let initializer = if self.current_token == token::Type::Assign {
            self.next_token_span(); // consume =
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(VarDecl {
            name,
            type_,
            visibility,
            initializer,
            is_static,
            // Callers fill in the real span after consuming the trailing semicolon.
            span: Span { start: 0, end: 0 },
        })
    }

    /// Parse a block body. `brace_start` is the byte offset of the already-consumed `{`.
    /// Consumes the closing `}`.
    pub(crate) fn parse_block(&mut self, brace_start: usize) -> Result<BlockStmt, ParserError> {
        let mut stmts = Vec::new();
        while self.current_token != token::Type::RBrace {
            stmts.push(self.parse_statement()?);
        }
        let end = self.current_token_end;
        self.next_token_span(); // consume }

        Ok(BlockStmt {
            stmts,
            span: Span {
                start: brace_start,
                end,
            },
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.current_token.clone() {
            token::Type::Comment(content) => self.parse_comment_stmt(content),
            token::Type::BlockComment(content) => self.parse_block_comment_stmt(content),
            token::Type::Break => self.parse_break_stmt(),
            token::Type::Continue => self.parse_continue_stmt(),
            token::Type::Var => self.parse_var_stmt(),
            token::Type::Return => self.parse_return_stmt(),
            token::Type::If => self.parse_if_stmt(),
            token::Type::While => self.parse_while_stmt(),
            token::Type::For => self.parse_for_stmt(),
            token::Type::LBrace => self.parse_block_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParserError> {
        let brace_start = self.current_token_start;
        self.next_token_span(); // consume `{`
        let block = self.parse_block(brace_start)?;

        Ok(Stmt::Block(block))
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Break(Span {
            start,
            end: semi_end,
        }))
    }

    fn parse_comment_stmt(&mut self, content: String) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let end = self.current_token_end;
        self.next_token_span();

        Ok(Stmt::Comment(content, Span { start, end }))
    }

    fn parse_block_comment_stmt(&mut self, content: String) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let end = self.current_token_end;
        self.next_token_span();

        Ok(Stmt::BlockComment(content, Span { start, end }))
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Continue(Span {
            start,
            end: semi_end,
        }))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expression()?;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `for`
        self.assert_next_token(&[token::Type::LParen])?;

        let init = match self.current_token {
            token::Type::Semicolon => {
                self.next_token_span();
                None
            }
            token::Type::Var => {
                let var_start = self.current_token_start;
                self.next_token_span(); // consume `var`
                let mut var_decl = self.parse_var_contents(None, false)?;
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                var_decl.span = Span {
                    start: var_start,
                    end: semi_end,
                };
                Some(ForInit::Var(var_decl))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Some(ForInit::Expr(expr))
            }
        };

        let condition = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.assert_next_token(&[token::Type::Semicolon])?;

        let update = if self.current_token == token::Type::RParen {
            None
        } else {
            Some(self.parse_expression()?)
        };
        self.assert_next_token(&[token::Type::RParen])?;

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(Stmt::For(ForStmt {
            init,
            condition,
            update,
            body,
            span: Span { start, end },
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParserError> {
        let inner = self.parse_if_inner()?;

        Ok(Stmt::If(inner))
    }

    fn parse_if_inner(&mut self) -> Result<IfStmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;

        let then_brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let then_branch = self.parse_block(then_brace_start)?;

        let mut trailing_comments = Vec::new();
        while matches!(
            self.current_token,
            token::Type::Comment(_) | token::Type::BlockComment(_)
        ) {
            trailing_comments.push(self.parse_statement()?);
        }

        let else_branch = if self.current_token == token::Type::Else {
            self.next_token_span(); // consume `else`
            if self.current_token == token::Type::If {
                Some(ElseBranch::If(Box::new(self.parse_if_inner()?)))
            } else {
                let else_brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                Some(ElseBranch::Block(self.parse_block(else_brace_start)?))
            }
        } else {
            None
        };

        let end = else_branch
            .as_ref()
            .map(|b| b.span().end)
            .or_else(|| trailing_comments.last().map(|s| s.span().end))
            .unwrap_or(then_branch.span.end);

        Ok(IfStmt {
            condition,
            then_branch,
            trailing_comments,
            else_branch,
            span: Span { start, end },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let value = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Return(ReturnStmt {
            value,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_var_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let mut var_decl = self.parse_var_contents(None, false)?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;
        var_decl.span = Span {
            start,
            end: semi_end,
        };

        Ok(Stmt::Var(var_decl))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `while`
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(Stmt::While(WhileStmt {
            condition,
            body,
            span: Span { start, end },
        }))
    }
}
