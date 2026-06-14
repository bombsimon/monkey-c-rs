use crate::ast::{
    AnnotationEntry, Ast, Binding, BlockStmt, CaseLabel, CatchClause, ClassDecl, CommentStmt,
    CommentTable, ConstDecl, DictTypeEntry, DictTypeKey, DoWhileStmt, ElseBranch, EnumDecl,
    EnumVariant, ForHeader, ForInit, ForStmt, FunctionDecl, IfStmt, ImportDecl, InterfaceMember,
    InterfaceMethod, InterfaceVar, ModuleDecl, Parens, ParseOutput, ReturnStmt, Span, Stmt,
    SwitchCase, SwitchStmt, ThrowStmt, TryStmt, Type, TypeKind, TypedefDecl, UsingDecl, VarDecl,
    Variable, Visibility, WhileStmt,
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
    /// Byte offset of the end (exclusive) of the token consumed *before*
    /// `current_token`. Useful for callers that want "position right after the
    /// last consumed real token" — e.g. the end of a parsed type's last
    /// token, used to bound the `BeforeBracket` comment slot on a function
    /// with a return type.
    pub(crate) prev_token_end: usize,
    /// Every comment encountered during parsing, in source order. Populated
    /// at the lexer-parser boundary by `next_token_span` — comments never
    /// reach `current_token`.
    pub(crate) comment_table: CommentTable,
    /// Byte length of the source — used to build the synthetic `Ast::Document`
    /// span so top-level standalone comments can attach as dangling-inside.
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

        // Prime the first token; comment tokens are auto-drained into the
        // table by `next_token_span`.
        parser.next_token_span();
        parser
    }

    /// Parse a complete source file into an [`Ast::Document`] and the
    /// [`CommentTable`] of source comments.
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

    /// Advance past `current_token`, drain any intervening comments into the
    /// side-table, and update `current_token` to the next non-comment token.
    /// The grammar code therefore never observes comments — they're invisible
    /// at the parser/lexer boundary.
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

    /// Consume the `>` that closes a generic argument list, splitting a
    /// greedy `>>` (`RightShift`) token in half so nested generics like
    /// `Array<List<T>>` close correctly. The remaining `>` becomes the new
    /// `current_token` for the outer level to consume.
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

    /// Continue a dotted identifier after its first segment has already been
    /// consumed, e.g. `self` in `self.classDef_`.
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

    /// Read the current token as a symbol name, accepting both `Identifier`
    /// and keyword tokens. Keywords are valid symbol names (e.g. `:hidden`,
    /// `:private`) — the Display text is their canonical string.
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

    /// Parse a full type including union alternatives (`or` / `|`) and
    /// nullable `?`. Use this for variable/parameter/return type annotations.
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        let mut ty = self.parse_simple_type(true)?;
        while matches!(
            self.current_token,
            token::Type::OrKeyword | token::Type::BitOr
        ) {
            self.next_token_span(); // consume `or` or `|`
            ty.alternatives.push(self.parse_simple_type(true)?);
        }

        Ok(ty)
    }

    /// Like [`parse_type`] but for the target type of an `as` cast expression.
    ///
    /// Two differences from [`parse_type`]:
    /// - `?` uses lookahead to distinguish a nullable marker (`as T?;`) from a
    ///   ternary operator (`as T ? a : b`).
    /// - `|` uses lookahead: consumed as a union separator only when followed
    ///   by a token that can start a type name, so `as T | Null` is a union
    ///   cast while `0x00 as U32 | 0xFF` and `0x00 as U32 | (a | b)` leave `|`
    ///   as a bitwise-OR operator. `(` is deliberately excluded from
    ///   "type-start" tokens here: `(expr)` after a cast type is virtually
    ///   always a parenthesized bitwise-OR operand, not a parenthesized type.
    pub(crate) fn parse_cast_type(&mut self) -> Result<Type, ParserError> {
        let mut ty = self.parse_simple_type(false)?;
        while self.current_token == token::Type::OrKeyword
            || (self.current_token == token::Type::BitOr
                && Self::is_type_start(&self.lexer.peek_token().1))
        {
            self.next_token_span(); // consume `or` or `|`
            ty.alternatives.push(self.parse_simple_type(false)?);
        }

        Ok(ty)
    }

    fn is_type_start(tok: &token::Type) -> bool {
        matches!(
            tok,
            token::Type::Identifier(_) | token::Type::LBrace | token::Type::LBracket
        )
    }

    /// Parse a single type with optional generic params, optional `?`, and
    /// no `or` alternatives (which `parse_type` handles).
    ///
    /// Generic params are separated by `,` only. `or` inside `<…>` belongs
    /// to a single param's union (`Array<Number or Null>` → one param whose
    /// `alternatives` is `[Null]`), not to the param list itself.
    pub(crate) fn parse_simple_type(&mut self, allow_optional: bool) -> Result<Type, ParserError> {
        let kind = if self.current_token == token::Type::LBrace {
            let (entries, trailing_comma) = self.parse_inline_dict_type()?;
            TypeKind::Dict {
                entries,
                trailing_comma,
            }
        } else if self.current_token == token::Type::Interface {
            let (members, body_span) = self.parse_interface_members()?;
            TypeKind::Interface { members, body_span }
        } else if self.current_token == token::Type::LBracket {
            TypeKind::Tuple {
                elements: self.parse_tuple_type_elements()?,
            }
        } else if self.current_token == token::Type::LParen {
            // `(T)` — parenthesised type. Needed so a trailing `?` or `or`
            // binds to the whole inner type, e.g. `(Method(x) as Void)?`.
            let open = self.current_token_start;
            self.next_token_span(); // consume (
            let inner = self.parse_type()?;
            let close = self.current_token_end;
            self.assert_next_token(&[token::Type::RParen])?;

            TypeKind::Group(Parens {
                open,
                inner: Box::new(inner),
                close,
            })
        } else {
            let ident = self.parse_dotted_identifier()?;

            if self.current_token == token::Type::LParen {
                // `Method(arg as T) as Return` — a callable / method-reference
                // type. The `as Return` part binds to the method type, not to
                // any outer position (the outer `parse_type` handles unions
                // afterwards).
                let (args, _trailing) = self.parse_function_args()?;
                let returns = if self.current_token == token::Type::As {
                    self.next_token_span();
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };

                TypeKind::Method {
                    name: ident,
                    args: args.inner,
                    returns,
                }
            } else {
                let generic_params = if self.current_token == token::Type::Less {
                    self.next_token_span(); // consume <
                    let mut params = Vec::new();

                    if self.current_token != token::Type::Greater {
                        params.push(self.parse_type()?);
                        while self.current_token == token::Type::Comma {
                            self.next_token_span(); // consume ,
                            params.push(self.parse_type()?);
                        }
                    }

                    self.consume_generic_close()?;

                    params
                } else {
                    Vec::new()
                };

                TypeKind::Named {
                    ident,
                    generic_params,
                }
            }
        };

        // In cast context (`allow_optional = false`) a trailing `?` is ambiguous:
        // `as T ? a : b` uses `?` as a ternary operator while `as T?;` makes the
        // type nullable. Peek one token ahead: a ternary `?` must be followed by an
        // expression, so if the next token is not an expression start the `?` is a
        // nullable marker. A following `:` is treated as a nullable marker too —
        // `cond ? expr as T? : fallback` is far more common than a cast directly
        // followed by a ternary whose branches are symbol literals (`as T ? :a : :b`).
        let peek = self.lexer.peek_token().1;
        let optional = self.current_token == token::Type::Question
            && (allow_optional || peek == token::Type::Colon || !Self::is_expr_start(&peek));

        if optional {
            self.next_token_span(); // consume ?
        }

        Ok(Type {
            kind,
            alternatives: Vec::new(),
            optional,
        })
    }

    /// Parse the body of an `interface { … }` type — a `{`-delimited list of
    /// function signatures and/or `var name as Type;` declarations, each
    /// terminated by `;`. Consumes the surrounding braces and returns the
    /// members together with the byte span of the body (`{` … `}`).
    fn parse_interface_members(&mut self) -> Result<(Vec<InterfaceMember>, Span), ParserError> {
        self.assert_next_token(&[token::Type::Interface])?;
        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;

        let mut members = Vec::new();
        while self.current_token != token::Type::RBrace {
            let member = match self.current_token {
                token::Type::Function => InterfaceMember::Function(self.parse_interface_method()?),
                token::Type::Var => InterfaceMember::Variable(self.parse_interface_var()?),
                _ => {
                    return Err(self.parse_error(format!(
                        "Expected `function` or `var` in interface body, got {:?}",
                        self.current_token
                    )));
                }
            };
            members.push(member);
        }

        let brace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok((
            members,
            Span {
                start: brace_start,
                end: brace_end,
            },
        ))
    }

    fn parse_interface_method(&mut self) -> Result<InterfaceMethod, ParserError> {
        let start = self.current_token_start;
        self.assert_next_token(&[token::Type::Function])?;
        let name = self.parse_identifier()?;
        self.next_token_span();
        let (args, _trailing) = self.parse_function_args()?;
        let returns = if self.current_token == token::Type::As {
            self.next_token_span();
            Some(self.parse_type()?)
        } else {
            None
        };
        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(InterfaceMethod {
            name,
            args: args.inner,
            returns,
            span: Span { start, end },
        })
    }

    fn parse_interface_var(&mut self) -> Result<InterfaceVar, ParserError> {
        let start = self.current_token_start;
        self.assert_next_token(&[token::Type::Var])?;
        let name = self.parse_identifier()?;
        self.next_token_span();
        self.assert_next_token(&[token::Type::As])?;
        let type_ = self.parse_type()?;
        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(InterfaceVar {
            name,
            type_,
            span: Span { start, end },
        })
    }

    /// Parse the body of a tuple type `[T1, T2, …]`. Consumes the surrounding
    /// brackets. Each element is a full type, so `or`-unions inside are
    /// allowed (`[Number, Number or Null]`).
    fn parse_tuple_type_elements(&mut self) -> Result<Vec<Type>, ParserError> {
        self.assert_next_token(&[token::Type::LBracket])?;
        let mut elements = Vec::new();
        if self.current_token != token::Type::RBracket {
            elements.push(self.parse_type()?);

            while self.current_token == token::Type::Comma {
                self.next_token_span();
                if self.current_token == token::Type::RBracket {
                    break; // trailing comma allowed
                }

                elements.push(self.parse_type()?);
            }
        }

        self.assert_next_token(&[token::Type::RBracket])?;

        Ok(elements)
    }

    /// Parse the entries of an inline dictionary type `{ :k as T, "k2" as U }`.
    /// Consumes the surrounding braces. Returns the entries and whether the
    /// source ended with a trailing comma.
    fn parse_inline_dict_type(&mut self) -> Result<(Vec<DictTypeEntry>, bool), ParserError> {
        self.assert_next_token(&[token::Type::LBrace])?;
        let mut entries = Vec::new();
        let mut trailing_comma = false;

        while self.current_token != token::Type::RBrace {
            let key = match self.current_token.clone() {
                token::Type::Colon => {
                    self.next_token_span(); // consume `:`
                    DictTypeKey::Symbol(self.parse_symbol_name()?)
                }
                token::Type::String(name) => {
                    self.next_token_span();
                    DictTypeKey::String(name)
                }
                _ => {
                    return Err(self.parse_error(format!(
                        "Expected `:symbol` or string key in inline dict type, got {:?}",
                        self.current_token
                    )));
                }
            };
            self.assert_next_token(&[token::Type::As])?;
            let value_type = self.parse_type()?;
            entries.push(DictTypeEntry { key, value_type });

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                if self.current_token == token::Type::RBrace {
                    trailing_comma = true;
                    break;
                }
            } else {
                break;
            }
        }

        self.assert_next_token(&[token::Type::RBrace])?;

        Ok((entries, trailing_comma))
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
            token::Type::LParen
                if matches!(
                    self.lexer.peek_token_skip_comments().1,
                    token::Type::Colon | token::Type::RParen
                ) =>
            {
                self.parse_annotation_decl(start)
            }
            token::Type::Class => self.parse_class_decl(start),
            token::Type::Const => self.parse_const_decl(start, visibility, is_static),
            token::Type::Function => self.parse_function_decl(start, visibility, is_static),
            token::Type::Enum => self.parse_enum_decl(start),
            token::Type::Import => self.parse_import_decl(start),
            token::Type::Using => self.parse_using_decl(start),
            token::Type::Typedef => self.parse_typedef_decl(start),
            token::Type::Module => self.parse_module_decl(start),
            token::Type::Var => self.parse_var_decl(start, visibility, is_static),
            token::Type::Eof => Ok(Ast::Eof),
            _ => Err(self.parse_error(format!(
                "Unexpected token at top level: {:?}",
                self.current_token
            ))),
        }
    }

    /// Parse a `(:Name)`, `(:Name1, :Name2, …)`, or empty `()`/`( /* … */ )`
    /// annotation at a declaration position. Each entry is a `:Name` pair —
    /// the parser consumes the colon and identifier separately. In
    /// expression scope the same bytes parse as a parenthesised symbol
    /// expression.
    fn parse_annotation_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.assert_next_token(&[token::Type::LParen])?;
        let mut entries = Vec::new();

        if self.current_token != token::Type::RParen {
            loop {
                let entry_start = self.current_token_start;
                self.assert_next_token(&[token::Type::Colon])?; // consume `:`
                let name = self.parse_symbol_name()?;

                let args = if self.current_token == token::Type::LParen {
                    self.next_token_span(); // consume `(`
                    let (args, _trailing) = self.parse_call_args(token::Type::RParen)?;
                    self.assert_next_token(&[token::Type::RParen])?;
                    args.into_iter().map(|a| a.value).collect()
                } else {
                    Vec::new()
                };

                entries.push(AnnotationEntry {
                    name,
                    args,
                    span: Span {
                        start: entry_start,
                        end: self.prev_token_end,
                    },
                });

                match self.current_token {
                    token::Type::Comma => {
                        self.next_token_span();
                    } // comma-separated: `(:a, :b)`
                    token::Type::Colon => {} // space-separated: `(:a :b)`
                    _ => break,
                }
            }
        }

        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;

        Ok(Ast::Annotation(entries, Span { start, end }))
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

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Class(ClassDecl {
            name,
            extends,
            body,
            brace_start,
            span: Span {
                start,
                end: rbrace_end,
            },
        }))
    }

    fn parse_class_body(&mut self) -> Result<Vec<Ast>, ParserError> {
        let mut body = Vec::new();
        while self.current_token != token::Type::RBrace {
            self.skip_semicolons();
            if self.current_token == token::Type::RBrace {
                break;
            }

            body.push(self.parse_declaration()?);
        }

        Ok(body)
    }

    fn parse_const_decl(
        &mut self,
        start: usize,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `const`
        let bindings = self.parse_bindings()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Const(ConstDecl {
            bindings,
            visibility,
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
        let (args, args_trailing_comma) = self.parse_function_args()?;

        let mut header_end = args.close;
        let returns = if self.current_token == token::Type::As {
            self.next_token_span();
            let ty = self.parse_type()?;
            header_end = self.prev_token_end;
            Some(ty)
        } else {
            None
        };

        let (body, end) = if self.current_token == token::Type::Semicolon {
            let end = self.current_token_end;
            self.next_token_span(); // consume `;`
            (None, end)
        } else {
            let brace_start = self.current_token_start;
            self.assert_next_token(&[token::Type::LBrace])?;
            let block = self.parse_block(brace_start)?;
            let end = block.span.end;
            (Some(block), end)
        };

        Ok(Ast::Function(FunctionDecl {
            name,
            args,
            args_trailing_comma,
            returns,
            body,
            visibility,
            is_static,
            header_end,
            span: Span { start, end },
        }))
    }

    fn parse_enum_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `enum`

        let name = if matches!(self.current_token, token::Type::Identifier(_)) {
            let n = self.parse_identifier()?;
            self.next_token_span();
            Some(n)
        } else {
            None
        };

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;

        let mut variants: Vec<EnumVariant> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == token::Type::RBrace {
                break;
            }

            let name_start = self.current_token_start;
            let name = self.parse_identifier()?;
            let mut variant_end = self.current_token_end;
            self.next_token_span(); // advance past identifier

            let value = if self.current_token == token::Type::Assign {
                self.next_token_span(); // consume `=`
                let v = self.parse_expression()?;
                variant_end = v.span().end;
                Some(v)
            } else {
                None
            };

            let variant = EnumVariant {
                name,
                value,
                span: Span {
                    start: name_start,
                    end: variant_end,
                },
            };

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                variants.push(variant);
                if self.current_token == token::Type::RBrace {
                    trailing_comma = true;
                    break;
                }
            } else if self.current_token == token::Type::RBrace {
                variants.push(variant);
                break;
            } else {
                return Err(self.parse_error(format!(
                    "Expected ',' or '}}' in enum body, got {:?}",
                    self.current_token
                )));
            }
        }

        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Enum(EnumDecl {
            name,
            variants,
            trailing_comma,
            brace_start,
            span: Span { start, end },
        }))
    }

    fn parse_import_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `import`
        let name = self.parse_dotted_identifier()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Import(ImportDecl {
            name,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_using_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `using`
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

        Ok(Ast::Using(UsingDecl {
            name,
            alias,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_typedef_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `typedef`
        let name = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier
        self.assert_next_token(&[token::Type::As])?;
        let type_ = self.parse_type()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Typedef(TypedefDecl {
            name,
            type_,
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

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Module(ModuleDecl {
            name,
            body,
            brace_start,
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

    /// Parse a `(arg, arg, ...)` parameter list, returning the args wrapped
    /// with their parens' source positions.
    fn parse_function_args(&mut self) -> Result<(Parens<Vec<Variable>>, bool), ParserError> {
        let open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let mut args: Vec<Variable> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == token::Type::RParen {
                break;
            }

            let arg_start = self.current_token_start;
            let name = self.parse_identifier()?;
            let mut arg_end = self.current_token_end;
            self.next_token_span();

            let type_ = if self.current_token == token::Type::As {
                self.next_token_span();
                let t = self.parse_type()?;
                // `parse_type` advances to the token after the type, so the
                // type ends at the previous token boundary. Track via
                // `current_token_start` saturating back.
                arg_end = self.current_token_start;
                Some(t)
            } else {
                None
            };

            let arg = Variable {
                name,
                type_,
                visibility: None,
                initializer: None,
                is_static: false,
                span: Span {
                    start: arg_start,
                    end: arg_end,
                },
            };

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                args.push(arg);

                if self.current_token == token::Type::RParen {
                    trailing_comma = true;
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

        let close = self.current_token_end;
        self.next_token_span(); // consume RParen

        Ok((
            Parens {
                open,
                inner: args,
                close,
            },
            trailing_comma,
        ))
    }

    /// Parse the contents of a `var` declaration after the `var` keyword has been consumed.
    /// Does NOT consume a trailing semicolon — callers set the final span after consuming it.
    fn parse_var_contents(
        &mut self,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<VarDecl, ParserError> {
        let bindings = self.parse_bindings()?;

        Ok(VarDecl {
            bindings,
            visibility,
            is_static,
            // Callers fill in the real span after consuming the trailing semicolon.
            span: Span { start: 0, end: 0 },
        })
    }

    /// Parse a comma-separated list of `name [as Type] [= init]` bindings —
    /// shared between `var` and `const` declarations.
    fn parse_bindings(&mut self) -> Result<Vec<Binding>, ParserError> {
        let mut bindings = Vec::new();
        loop {
            bindings.push(self.parse_one_binding()?);
            if self.current_token == token::Type::Comma {
                self.next_token_span(); // consume `,`
            } else {
                break;
            }
        }

        Ok(bindings)
    }

    fn parse_one_binding(&mut self) -> Result<Binding, ParserError> {
        let start = self.current_token_start;
        let name = self.parse_identifier()?;
        let mut end = self.current_token_end;
        self.next_token_span();

        let type_ = if self.current_token == token::Type::As {
            self.next_token_span();
            let ty = self.parse_type()?;
            end = self.prev_token_end;
            Some(ty)
        } else {
            None
        };

        let initializer = if self.current_token == token::Type::Assign {
            self.next_token_span(); // consume `=`
            let expr = self.parse_expression()?;
            end = expr.span().end;
            Some(Box::new(expr))
        } else {
            None
        };

        Ok(Binding {
            name,
            type_,
            initializer,
            span: Span { start, end },
        })
    }

    /// Parse a block body. `brace_start` is the byte offset of the already-consumed `{`.
    /// Consumes the closing `}`.
    pub(crate) fn parse_block(&mut self, brace_start: usize) -> Result<BlockStmt, ParserError> {
        let mut stmts = Vec::new();
        loop {
            self.skip_semicolons();

            if self.current_token == token::Type::RBrace {
                break;
            }

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
            token::Type::Break => self.parse_break_stmt(),
            token::Type::Continue => self.parse_continue_stmt(),
            token::Type::Var => self.parse_var_stmt(),
            token::Type::Return => self.parse_return_stmt(),
            token::Type::If => self.parse_if_stmt(),
            token::Type::While => self.parse_while_stmt(),
            token::Type::Do => self.parse_do_while_stmt(),
            token::Type::For => self.parse_for_stmt(),
            token::Type::Switch => self.parse_switch_stmt(),
            token::Type::Try => self.parse_try_stmt(),
            token::Type::Throw => self.parse_throw_stmt(),
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
        let header_open = self.current_token_start;
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
                let mut exprs = vec![self.parse_expression()?];
                while self.current_token == token::Type::Comma {
                    self.next_token_span(); // consume `,`
                    exprs.push(self.parse_expression()?);
                }
                self.assert_next_token(&[token::Type::Semicolon])?;
                Some(ForInit::Expr(exprs))
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
            let mut exprs = vec![self.parse_expression()?];
            while self.current_token == token::Type::Comma {
                self.next_token_span(); // consume `,`
                exprs.push(self.parse_expression()?);
            }

            Some(exprs)
        };
        let header_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let header = Parens {
            open: header_open,
            inner: ForHeader {
                init,
                condition,
                update,
            },
            close: header_close,
        };

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(Stmt::For(ForStmt {
            header,
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
        let cond_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let cond_inner = self.parse_expression()?;
        let cond_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let condition = Parens {
            open: cond_open,
            inner: cond_inner,
            close: cond_close,
        };

        let then_brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let then_branch = self.parse_block(then_brace_start)?;

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
            .unwrap_or(then_branch.span.end);

        Ok(IfStmt {
            condition,
            then_branch,
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

    fn parse_throw_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `throw`
        let value = self.parse_expression()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Throw(ThrowStmt {
            value,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_switch_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `switch`
        let disc_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let disc_inner = self.parse_expression()?;
        let disc_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let discriminant = Parens {
            open: disc_open,
            inner: disc_inner,
            close: disc_close,
        };
        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;

        let mut cases = Vec::new();
        while self.current_token != token::Type::RBrace {
            cases.push(self.parse_switch_case()?);
        }

        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Stmt::Switch(SwitchStmt {
            discriminant,
            cases,
            brace_start,
            span: Span { start, end },
        }))
    }

    /// Parse one `case <…>:` or `default:` arm and the statements that
    /// follow, up to the next case/default or the closing `}`. Leading
    /// comments are filled in by [`Parser::parse_switch_stmt`].
    fn parse_switch_case(&mut self) -> Result<SwitchCase, ParserError> {
        let start = self.current_token_start;
        let label = match self.current_token.clone() {
            token::Type::Case => {
                self.next_token_span(); // consume `case`
                if self.current_token == token::Type::InstanceOf {
                    self.next_token_span(); // consume `instanceof`
                    CaseLabel::InstanceOf(self.parse_type()?)
                } else {
                    CaseLabel::Value(self.parse_expression()?)
                }
            }
            token::Type::Default => {
                self.next_token_span(); // consume `default`
                CaseLabel::Default
            }
            _ => {
                return Err(self.parse_error(format!(
                    "Expected `case` or `default` in switch body, got {:?}",
                    self.current_token
                )));
            }
        };

        let colon_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Colon])?;

        let mut stmts = Vec::new();
        while !matches!(
            self.current_token,
            token::Type::Case | token::Type::Default | token::Type::RBrace
        ) {
            stmts.push(self.parse_statement()?);
        }

        let end = stmts
            .last()
            .map(|s| s.span().end)
            .unwrap_or(self.current_token_end);

        Ok(SwitchCase {
            label,
            stmts,
            label_span: Span {
                start,
                end: colon_end,
            },
            span: Span { start, end },
        })
    }

    fn parse_try_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let header_end = self.current_token_end;
        self.next_token_span(); // consume `try`

        let body_brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(body_brace_start)?;

        let mut catches = Vec::new();
        while self.current_token == token::Type::Catch {
            catches.push(self.parse_catch_clause()?);
        }

        let finally = if self.current_token == token::Type::Finally {
            self.next_token_span(); // consume `finally`
            let brace_start = self.current_token_start;
            self.assert_next_token(&[token::Type::LBrace])?;
            Some(self.parse_block(brace_start)?)
        } else {
            None
        };

        if catches.is_empty() && finally.is_none() {
            return Err(self.parse_error(
                "`try` block must be followed by at least one `catch` or `finally`".to_string(),
            ));
        }

        let end = finally
            .as_ref()
            .map(|b| b.span.end)
            .or_else(|| catches.last().map(|c| c.span.end))
            .unwrap_or(body.span.end);

        Ok(Stmt::Try(TryStmt {
            body,
            catches,
            finally,
            header_end,
            span: Span { start, end },
        }))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `catch`
        self.assert_next_token(&[token::Type::LParen])?;
        let binding = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier
        let type_filter = if self.current_token == token::Type::InstanceOf {
            self.next_token_span(); // consume `instanceof`
            Some(self.parse_type()?)
        } else {
            None
        };
        self.assert_next_token(&[token::Type::RParen])?;
        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(CatchClause {
            binding,
            type_filter,
            body,
            span: Span { start, end },
        })
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
        let cond_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let cond_inner = self.parse_expression()?;
        let cond_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let condition = Parens {
            open: cond_open,
            inner: cond_inner,
            close: cond_close,
        };

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

    fn parse_do_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let header_end = self.current_token_end;
        self.next_token_span(); // consume `do`

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;

        self.assert_next_token(&[token::Type::While])?;
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::DoWhile(DoWhileStmt {
            body,
            condition,
            header_end,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }
}
