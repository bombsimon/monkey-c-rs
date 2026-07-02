use crate::ast::{
    AnnotationEntry, Ast, Binding, ClassDecl, ConstDecl, EnumDecl, EnumVariant, FunctionDecl,
    ImportDecl, ModuleDecl, Parens, Span, Spanned, TypedefDecl, UsingDecl, VarDecl, Variable,
    Visibility,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    pub(crate) fn parse_declaration(&mut self) -> Result<Ast, ParserError> {
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
        let name_start = self.current_token_start;
        let name_node = self.parse_identifier()?;
        self.next_token_span();
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };

        let (extends_kw_start, extends) = if self.current_token == token::Type::Extends {
            let ek = self.current_token_start;
            self.next_token_span();
            let es = self.current_token_start;
            let n = self.parse_dotted_identifier()?;
            let extends_end = self.prev_token_end;
            (
                Some(ek),
                Some(Spanned {
                    span: Span {
                        start: es,
                        end: extends_end,
                    },
                    node: n,
                }),
            )
        } else {
            (None, None)
        };

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Ast::Class(ClassDecl {
            name,
            extends,
            extends_kw_start,
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
        let semi_pos = self.current_token_start;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Const(ConstDecl {
            bindings,
            visibility,
            is_static,
            semi_pos,
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
        let name_start = self.current_token_start;
        let name_node = self.parse_identifier()?;
        self.next_token_span();
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };
        let (args, args_trailing_comma) = self.parse_function_args()?;

        let mut header_end = args.close;
        let (as_kw_start, returns) = if self.current_token == token::Type::As {
            let ak = self.current_token_start;
            self.next_token_span();
            let ty = self.parse_type()?;
            header_end = self.prev_token_end;
            (Some(ak), Some(ty))
        } else {
            (None, None)
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
            as_kw_start,
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
            let ns = self.current_token_start;
            let n = self.parse_identifier()?;
            self.next_token_span();
            let ne = self.prev_token_end;
            Some(Spanned {
                span: Span { start: ns, end: ne },
                node: n,
            })
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

            let (assign_kw_start, value) = if self.current_token == token::Type::Assign {
                let ak = self.current_token_start;
                self.next_token_span(); // consume `=`
                let v = self.parse_expression()?;
                variant_end = v.span().end;
                (Some(ak), Some(v))
            } else {
                (None, None)
            };

            let variant = EnumVariant {
                name,
                value,
                assign_kw_start,
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
        let name_start = self.current_token_start;
        let name_node = self.parse_dotted_identifier()?;
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };
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
        let name_start = self.current_token_start;
        let name_node = self.parse_dotted_identifier()?;
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };
        let (as_kw_start, alias) = if self.current_token == token::Type::As {
            let ak = self.current_token_start;
            self.next_token_span(); // consume `as`
            let al = self.current_token_start;
            let alias_node = self.parse_identifier()?;
            self.next_token_span(); // advance past alias name
            let alias_end = self.prev_token_end;
            (
                Some(ak),
                Some(Spanned {
                    span: Span {
                        start: al,
                        end: alias_end,
                    },
                    node: alias_node,
                }),
            )
        } else {
            (None, None)
        };
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Using(UsingDecl {
            name,
            alias,
            as_kw_start,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_typedef_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `typedef`
        let name_start = self.current_token_start;
        let name_node = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };
        let as_kw_start = self.current_token_start;
        self.assert_next_token(&[token::Type::As])?;
        let type_ = self.parse_type()?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Ast::Typedef(TypedefDecl {
            name,
            as_kw_start,
            type_,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_module_decl(&mut self, start: usize) -> Result<Ast, ParserError> {
        self.next_token_span(); // consume `module`
        let name_start = self.current_token_start;
        let name_node = self.parse_identifier()?;
        self.next_token_span(); // advance past name
        let name = Spanned {
            span: Span {
                start: name_start,
                end: self.prev_token_end,
            },
            node: name_node,
        };

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
        let semi_pos = self.current_token_start;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;
        var_decl.semi_pos = semi_pos;
        var_decl.span = Span {
            start,
            end: semi_end,
        };

        Ok(Ast::Variable(var_decl))
    }

    /// Parse a `(arg, arg, ...)` parameter list, returning the args wrapped
    /// with their parens' source positions.
    pub(crate) fn parse_function_args(
        &mut self,
    ) -> Result<(Parens<Vec<Variable>>, bool), ParserError> {
        let open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let mut args: Vec<Variable> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == token::Type::RParen {
                break;
            }

            let arg_start = self.current_token_start;
            let name_node = self.parse_identifier()?;
            let mut arg_end = self.current_token_end;
            self.next_token_span();
            let name = Spanned {
                span: Span {
                    start: arg_start,
                    end: self.prev_token_end,
                },
                node: name_node,
            };

            let (as_kw_start, type_) = if self.current_token == token::Type::As {
                let ak = self.current_token_start;
                self.next_token_span();
                let t = self.parse_type()?;
                // `parse_type` advances to the token after the type, so the
                // type ends at the previous token boundary. Track via
                // `current_token_start` saturating back.
                arg_end = self.current_token_start;
                (Some(ak), Some(t))
            } else {
                (None, None)
            };

            let arg = Variable {
                name,
                type_,
                as_kw_start,
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
    pub(crate) fn parse_var_contents(
        &mut self,
        visibility: Option<Visibility>,
        is_static: bool,
    ) -> Result<VarDecl, ParserError> {
        let bindings = self.parse_bindings()?;

        Ok(VarDecl {
            bindings,
            visibility,
            is_static,
            // Callers fill in the real span and semi_pos after consuming the trailing semicolon.
            semi_pos: 0,
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
        let name_node = self.parse_identifier()?;
        let mut end = self.current_token_end;
        self.next_token_span();
        let name = Spanned {
            span: Span {
                start,
                end: self.prev_token_end,
            },
            node: name_node,
        };

        let (as_kw_start, type_) = if self.current_token == token::Type::As {
            let ak = self.current_token_start;
            self.next_token_span();
            let ty = self.parse_type()?;
            end = self.prev_token_end;
            (Some(ak), Some(ty))
        } else {
            (None, None)
        };

        let (assign_kw_start, initializer) = if self.current_token == token::Type::Assign {
            let ak = self.current_token_start;
            self.next_token_span(); // consume `=`
            let expr = self.parse_expression()?;
            end = expr.span().end;
            (Some(ak), Some(Box::new(expr)))
        } else {
            (None, None)
        };

        Ok(Binding {
            name,
            type_,
            as_kw_start,
            initializer,
            assign_kw_start,
            span: Span { start, end },
        })
    }
}
