use crate::ast::{
    DictTypeEntry, DictTypeKey, InterfaceMember, InterfaceMethod, InterfaceVar, Parens, Span,
    Spanned, Type, TypeKind,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
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

        if !ty.alternatives.is_empty() {
            ty.span.end = self.prev_token_end;
        }

        Ok(ty)
    }

    /// Like [`parse_type`](Self::parse_type) but for the target type of an
    /// `as` cast expression.
    ///
    /// Two differences from [`parse_type`](Self::parse_type):
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

        if !ty.alternatives.is_empty() {
            ty.span.end = self.prev_token_end;
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
        let start = self.current_token_start;
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
            span: Span {
                start,
                end: self.prev_token_end,
            },
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
        let (args, _trailing) = self.parse_function_args()?;
        let (as_kw_start, returns) = if self.current_token == token::Type::As {
            let ak = self.current_token_start;
            self.next_token_span();
            (Some(ak), Some(self.parse_type()?))
        } else {
            (None, None)
        };
        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(InterfaceMethod {
            name,
            args: args.inner,
            returns,
            as_kw_start,
            span: Span { start, end },
        })
    }

    fn parse_interface_var(&mut self) -> Result<InterfaceVar, ParserError> {
        let start = self.current_token_start;
        self.assert_next_token(&[token::Type::Var])?;
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
        let as_kw_start = self.current_token_start;
        self.assert_next_token(&[token::Type::As])?;
        let type_ = self.parse_type()?;
        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(InterfaceVar {
            name,
            as_kw_start,
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
            let entry_start = self.current_token_start;
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
            let entry_span = Span {
                start: entry_start,
                end: self.prev_token_end,
            };
            entries.push(DictTypeEntry {
                key,
                value_type,
                span: entry_span,
            });

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
}
