use crate::ast::{
    AnnotationEntry, Ast, Binding, ClassDecl, ConstDecl, EnumDecl, EnumVariant, FunctionDecl,
    ImportDecl, Modifiers, ModuleDecl, Parameter, Parens, Separated, Span, Spanned, TypedefDecl,
    UsingDecl, VarDecl, Visibility,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    pub(crate) fn parse_declaration(&mut self) -> Result<Ast, ParserError> {
        let start = self.current_token_start;
        let modifiers = self.parse_modifiers()?;

        match self.current_token.clone() {
            token::Type::Class => self.parse_class_decl(start, modifiers),
            token::Type::Const => self.parse_const_decl(start, modifiers),
            token::Type::Enum => self.parse_enum_decl(start, modifiers),
            token::Type::Function => self.parse_function_decl(start, modifiers),
            token::Type::Module => self.parse_module_decl(start, modifiers),
            token::Type::Var => Ok(Ast::Variable(self.parse_var_decl(start, modifiers)?)),
            _ if !modifiers.is_empty() => Err(self.parse_error(format!(
                "Unexpected token after modifiers: {:?}",
                self.current_token
            ))),
            token::Type::LParen
                if matches!(
                    self.lexer.peek_token_skip_comments().1,
                    token::Type::Colon | token::Type::RParen
                ) =>
            {
                self.parse_annotation_decl(start)
            }
            token::Type::Import => self.parse_import_decl(start),
            token::Type::Using => self.parse_using_decl(start),
            token::Type::Typedef => self.parse_typedef_decl(start),
            token::Type::Eof => Ok(Ast::Eof),
            _ => Err(self.parse_error(format!(
                "Unexpected token at top level: {:?}",
                self.current_token
            ))),
        }
    }

    /// Collect the modifiers in front of a declaration. Like the compiler, each kind may only be
    /// given once, so no modifier is ever silently overwritten.
    fn parse_modifiers(&mut self) -> Result<Modifiers, ParserError> {
        let mut modifiers = Modifiers::default();

        loop {
            match self.current_token {
                token::Type::Static if modifiers.is_static() => {
                    return Err(self.parse_error("Duplicate `static` modifier"));
                }
                token::Type::Static => modifiers.static_kw_start = Some(self.current_token_start),
                _ => {
                    let Some(visibility) = visibility_keyword(&self.current_token) else {
                        break;
                    };

                    if modifiers.visibility.is_some() {
                        return Err(self.parse_error("Multiple visibility modifiers specified"));
                    }

                    modifiers.visibility = Some(Spanned {
                        span: Span {
                            start: self.current_token_start,
                            end: self.current_token_end,
                        },
                        node: visibility,
                    });
                }
            }

            self.advance();
        }

        Ok(modifiers)
    }

    /// Parse a `(:Name)`, `(:Name1, :Name2, …)`, or empty `()`/`( /* … */ )` annotation at a
    /// declaration position. Each entry is a `:Name` pair — the parser consumes the colon and
    /// identifier separately. In expression scope the same bytes parse as a parenthesised symbol
    /// expression.
    fn parse_annotation_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.expect(token::Type::LParen)?;
        let mut entries = Vec::new();
        let mut preceding_comma_pos = None;

        if self.current_token != token::Type::RParen {
            loop {
                let entry_start = self.current_token_start;
                self.expect(token::Type::Colon)?;
                let name = self.parse_symbol_name()?;

                let args = if self.current_token == token::Type::LParen {
                    self.advance();
                    let args = self.parse_call_args(token::Type::RParen)?;
                    self.expect(token::Type::RParen)?;
                    args.items.into_iter().map(|a| a.value).collect()
                } else {
                    Vec::new()
                };

                entries.push(AnnotationEntry {
                    name,
                    args,
                    preceding_comma_pos,
                    span: Span {
                        start: entry_start,
                        end: self.prev_token_end,
                    },
                });

                match self.current_token {
                    token::Type::Comma => {
                        preceding_comma_pos = Some(self.current_token_start);
                        self.advance();
                    }
                    token::Type::Colon => preceding_comma_pos = None, // space-separated: `(:a :b)`
                    _ => break,
                }
            }
        }

        let end = self.current_token_end;
        self.expect(token::Type::RParen)?;

        Ok(Ast::Annotation(entries, Span { start, end }))
    }

    fn parse_class_decl(&mut self, start: usize, modifiers: Modifiers) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_identifier()?;

        let (extends_kw_start, extends) = if self.current_token == token::Type::Extends {
            let extends_kw_start = self.advance().start;

            (
                Some(extends_kw_start),
                Some(self.parse_spanned_dotted_identifier()?),
            )
        } else {
            (None, None)
        };

        let brace_start = self.expect(token::Type::LBrace)?.start;
        let body = self.parse_class_body()?;
        let end = self.expect(token::Type::RBrace)?.end;

        Ok(Ast::Class(ClassDecl {
            name,
            extends,
            extends_kw_start,
            body,
            brace_start,
            modifiers,
            span: Span { start, end },
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

    fn parse_const_decl(&mut self, start: usize, modifiers: Modifiers) -> Result<Ast, ParserError> {
        self.advance();
        let bindings = self.parse_bindings()?;
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(Ast::Const(ConstDecl {
            bindings,
            modifiers,
            semi_pos: semi.start,
            span: Span {
                start,
                end: semi.end,
            },
        }))
    }

    fn parse_function_decl(
        &mut self,
        start: usize,
        modifiers: Modifiers,
    ) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_identifier()?;
        let parameters = self.parse_function_args()?;
        let (as_kw_start, returns) = self.parse_type_annotation()?;

        let (body, end) = if self.current_token == token::Type::Semicolon {
            (None, self.advance().end)
        } else {
            let block = self.parse_block()?;
            let end = block.span.end;

            (Some(block), end)
        };

        Ok(Ast::Function(FunctionDecl {
            name,
            parameters,
            returns,
            as_kw_start,
            body,
            modifiers,
            span: Span { start, end },
        }))
    }

    fn parse_enum_decl(&mut self, start: usize, modifiers: Modifiers) -> Result<Ast, ParserError> {
        self.advance();

        let name = if matches!(self.current_token, token::Type::Identifier(_)) {
            Some(self.parse_spanned_identifier()?)
        } else {
            None
        };

        let brace_start = self.expect(token::Type::LBrace)?.start;

        let variants = self.parse_separated(&token::Type::RBrace, |parser| {
            let name_start = parser.current_token_start;
            let name = parser.parse_identifier()?;

            let (assign_kw_start, value) = if parser.current_token == token::Type::Assign {
                let assign_kw_start = parser.advance().start;

                (Some(assign_kw_start), Some(parser.parse_expression()?))
            } else {
                (None, None)
            };

            Ok(EnumVariant {
                name,
                value,
                assign_kw_start,
                span: Span {
                    start: name_start,
                    end: parser.prev_token_end,
                },
            })
        })?;

        let end = self.expect(token::Type::RBrace)?.end;

        Ok(Ast::Enum(EnumDecl {
            name,
            variants,
            brace_start,
            modifiers,
            span: Span { start, end },
        }))
    }

    fn parse_import_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_dotted_identifier()?;
        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(Ast::Import(ImportDecl {
            name,
            span: Span { start, end },
        }))
    }

    fn parse_using_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_dotted_identifier()?;

        let (as_kw_start, alias) = if self.current_token == token::Type::As {
            let as_kw_start = self.advance().start;

            (Some(as_kw_start), Some(self.parse_spanned_identifier()?))
        } else {
            (None, None)
        };

        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(Ast::Using(UsingDecl {
            name,
            alias,
            as_kw_start,
            span: Span { start, end },
        }))
    }

    fn parse_typedef_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_identifier()?;
        let as_kw_start = self.expect(token::Type::As)?.start;
        let type_ = self.parse_type()?;
        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(Ast::Typedef(TypedefDecl {
            name,
            as_kw_start,
            type_,
            span: Span { start, end },
        }))
    }

    fn parse_module_decl(
        &mut self,
        start: usize,
        modifiers: Modifiers,
    ) -> Result<Ast, ParserError> {
        self.advance();
        let name = self.parse_spanned_identifier()?;
        let brace_start = self.expect(token::Type::LBrace)?.start;
        let body = self.parse_class_body()?;
        let end = self.expect(token::Type::RBrace)?.end;

        Ok(Ast::Module(ModuleDecl {
            name,
            body,
            brace_start,
            modifiers,
            span: Span { start, end },
        }))
    }

    /// Parse a `var` declaration from the `var` keyword through its terminating `;`, which in a
    /// `for` header is the loop's first `;`.
    pub(crate) fn parse_var_decl(
        &mut self,
        start: usize,
        modifiers: Modifiers,
    ) -> Result<VarDecl, ParserError> {
        self.advance();
        let bindings = self.parse_bindings()?;
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(VarDecl {
            bindings,
            modifiers,
            semi_pos: semi.start,
            span: Span {
                start,
                end: semi.end,
            },
        })
    }

    /// Parse a `(arg, arg, ...)` parameter list, returning the args wrapped
    /// with their parens' source positions.
    pub(crate) fn parse_function_args(
        &mut self,
    ) -> Result<Parens<Separated<Parameter>>, ParserError> {
        let open = self.expect(token::Type::LParen)?.start;
        let args = self.parse_separated(&token::Type::RParen, |parser| {
            let name = parser.parse_spanned_identifier()?;
            let (as_kw_start, type_) = parser.parse_type_annotation()?;

            Ok(Parameter {
                span: Span {
                    start: name.start(),
                    end: parser.prev_token_end,
                },
                name,
                type_,
                as_kw_start,
                initializer: None,
            })
        })?;

        let close = self.advance().end;

        Ok(Parens {
            open,
            inner: args,
            close,
        })
    }

    /// Parse a comma-separated list of `name [as Type] [= init]` bindings —
    /// shared between `var` and `const` declarations.
    fn parse_bindings(&mut self) -> Result<Separated<Binding>, ParserError> {
        let mut bindings = Separated::default();
        loop {
            bindings.items.push(self.parse_one_binding()?);
            if self.current_token != token::Type::Comma {
                break;
            }

            bindings.commas.push(self.advance().start);
        }

        Ok(bindings)
    }

    fn parse_one_binding(&mut self) -> Result<Binding, ParserError> {
        let name = self.parse_spanned_identifier()?;
        let (as_kw_start, type_) = self.parse_type_annotation()?;

        let (assign_kw_start, initializer) = if self.current_token == token::Type::Assign {
            let assign_kw_start = self.advance().start;

            (
                Some(assign_kw_start),
                Some(Box::new(self.parse_expression()?)),
            )
        } else {
            (None, None)
        };

        Ok(Binding {
            span: Span {
                start: name.start(),
                end: self.prev_token_end,
            },
            name,
            type_,
            as_kw_start,
            initializer,
            assign_kw_start,
        })
    }
}

fn visibility_keyword(token: &token::Type) -> Option<Visibility> {
    match token {
        token::Type::Hidden => Some(Visibility::Hidden),
        token::Type::Private => Some(Visibility::Private),
        token::Type::Protected => Some(Visibility::Protected),
        token::Type::Public => Some(Visibility::Public),
        _ => None,
    }
}
