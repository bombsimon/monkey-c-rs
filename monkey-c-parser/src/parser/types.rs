use crate::ast::{
    DictTypeEntry, DictTypeKey, InterfaceMember, InterfaceMethod, InterfaceVar, Parens, Separated,
    Span, Spanned, Type, TypeKind, UnionAlternative, UnionSeparator,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    /// Parse a full type including union alternatives (`or` / `|`) and nullable `?`. Use this for
    /// variable/parameter/return type annotations.
    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        self.parse_union_type(false)
    }

    /// Like [`parse_type`](Self::parse_type) but for the target of an `as` cast, where `?` might
    /// start a ternary and `|` might be a bitwise or, as in `0x00 as U32 | 0xFF`. `|` is only read
    /// as a union when a type name follows, and `(` is not counted as one since `(expr)` after a
    /// cast is virtually always a bitwise operand.
    pub(crate) fn parse_cast_type(&mut self) -> Result<Type, ParserError> {
        self.parse_union_type(true)
    }

    fn parse_union_type(&mut self, is_cast: bool) -> Result<Type, ParserError> {
        let mut ty = self.parse_simple_type(is_cast)?;
        while let Some(separator) = self.union_separator() {
            if is_cast
                && separator.node == UnionSeparator::Pipe
                && !Self::is_type_start(&self.lexer.peek_token().1)
            {
                break;
            }

            self.advance();
            ty.alternatives.push(UnionAlternative {
                separator,
                type_: self.parse_simple_type(is_cast)?,
            });
        }

        if !ty.alternatives.is_empty() {
            ty.span.end = self.prev_token_end;
        }

        Ok(ty)
    }

    fn union_separator(&self) -> Option<Spanned<UnionSeparator>> {
        let separator = match self.current_token {
            token::Type::OrKeyword => UnionSeparator::Or,
            token::Type::BitOr => UnionSeparator::Pipe,
            _ => return None,
        };

        Some(Spanned {
            span: Span {
                start: self.current_token_start,
                end: self.current_token_end,
            },
            node: separator,
        })
    }

    fn is_type_start(tok: &token::Type) -> bool {
        matches!(
            tok,
            token::Type::Identifier(_) | token::Type::LBrace | token::Type::LBracket
        )
    }

    /// Parse a single type with optional generic params and `?`, but no union alternatives.
    fn parse_simple_type(&mut self, is_cast: bool) -> Result<Type, ParserError> {
        let start = self.current_token_start;
        let kind = if self.current_token == token::Type::LBrace {
            let (entries, body_span) = self.parse_inline_dict_type()?;
            TypeKind::Dict { entries, body_span }
        } else if self.current_token == token::Type::Interface {
            let (members, body_span) = self.parse_interface_members()?;
            TypeKind::Interface { members, body_span }
        } else if self.current_token == token::Type::LBracket {
            TypeKind::Tuple {
                elements: self.parse_tuple_type_elements()?,
            }
        } else if self.current_token == token::Type::LParen {
            let open = self.advance().start;
            let inner = self.parse_type()?;
            let close = self.expect(token::Type::RParen)?.end;

            TypeKind::Group(Parens {
                open,
                inner: Box::new(inner),
                close,
            })
        } else {
            let ident = self.parse_dotted_identifier()?;

            if self.current_token == token::Type::LParen {
                // `Method(arg as T) as Return` — a callable / method-reference type. The `as
                // Return` part binds to the method type, not to any outer position (the outer
                // `parse_type` handles unions afterwards).
                let args = self.parse_function_args()?;
                let returns = if self.current_token == token::Type::As {
                    self.advance();
                    Some(Box::new(self.parse_type()?))
                } else {
                    None
                };

                TypeKind::Method {
                    name: ident,
                    args,
                    returns,
                }
            } else {
                let generic_params = if self.current_token == token::Type::Less {
                    self.parse_generic_params()?
                } else {
                    Vec::new()
                };

                TypeKind::Named {
                    ident,
                    generic_params,
                }
            }
        };

        let optional = self.current_token == token::Type::Question
            && (!is_cast || self.lexer.peek_token().1.follows_nullable_cast());

        if optional {
            self.advance();
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

    /// Parse the body of an `interface { … }` type — a `{`-delimited list of function signatures
    /// and/or `var name as Type;` declarations, each terminated by `;`. Consumes the surrounding
    /// braces and returns the members together with the byte span of the body (`{` … `}`).
    fn parse_interface_members(&mut self) -> Result<(Vec<InterfaceMember>, Span), ParserError> {
        self.expect(token::Type::Interface)?;
        let brace_start = self.expect(token::Type::LBrace)?.start;

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

        let brace_end = self.expect(token::Type::RBrace)?.end;

        Ok((
            members,
            Span {
                start: brace_start,
                end: brace_end,
            },
        ))
    }

    fn parse_interface_method(&mut self) -> Result<InterfaceMethod, ParserError> {
        let start = self.expect(token::Type::Function)?.start;
        let name = self.parse_spanned_identifier()?;
        let args = self.parse_function_args()?;
        let (as_kw_start, returns) = self.parse_type_annotation()?;
        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(InterfaceMethod {
            name,
            args,
            returns,
            as_kw_start,
            span: Span { start, end },
        })
    }

    fn parse_interface_var(&mut self) -> Result<InterfaceVar, ParserError> {
        let start = self.expect(token::Type::Var)?.start;
        let name = self.parse_spanned_identifier()?;
        let as_kw_start = self.expect(token::Type::As)?.start;
        let type_ = self.parse_type()?;
        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(InterfaceVar {
            name,
            as_kw_start,
            type_,
            span: Span { start, end },
        })
    }

    /// Parse a generic argument list `<T, U, ...>`. A union inside belongs to one argument, so
    /// `or` is never a separator here.
    pub(crate) fn parse_generic_params(&mut self) -> Result<Vec<Type>, ParserError> {
        self.expect(token::Type::Less)?;
        let mut params = Vec::new();
        if self.current_token != token::Type::Greater {
            params.push(self.parse_type()?);
            while self.current_token == token::Type::Comma {
                self.advance();
                params.push(self.parse_type()?);
            }
        }

        self.consume_generic_close()?;

        Ok(params)
    }

    /// Parse the body of a tuple type `[T1, T2, …]`. Consumes the surrounding brackets. Each
    /// element is a full type, so `or`-unions inside are allowed (`[Number, Number or Null]`).
    fn parse_tuple_type_elements(&mut self) -> Result<Vec<Type>, ParserError> {
        self.expect(token::Type::LBracket)?;
        let mut elements = Vec::new();
        if self.current_token != token::Type::RBracket {
            elements.push(self.parse_type()?);

            while self.current_token == token::Type::Comma {
                self.advance();
                if self.current_token == token::Type::RBracket {
                    break; // trailing comma allowed
                }

                elements.push(self.parse_type()?);
            }
        }

        self.expect(token::Type::RBracket)?;

        Ok(elements)
    }

    /// Parse the entries of an inline dictionary type `{ :k as T, "k2" as U }`. Consumes the
    /// surrounding braces. Returns the entries and the span from `{` through `}`.
    fn parse_inline_dict_type(&mut self) -> Result<(Separated<DictTypeEntry>, Span), ParserError> {
        let start = self.expect(token::Type::LBrace)?.start;
        let entries = self.parse_separated(&token::Type::RBrace, |parser| {
            let entry_start = parser.current_token_start;
            let key = match parser.current_token.clone() {
                token::Type::Colon => {
                    parser.advance();
                    DictTypeKey::Symbol(parser.parse_symbol_name()?)
                }
                token::Type::String(name) => {
                    parser.advance();
                    DictTypeKey::String(name)
                }
                _ => {
                    return Err(parser.parse_error(format!(
                        "Expected `:symbol` or string key in inline dict type, got {:?}",
                        parser.current_token
                    )));
                }
            };
            parser.expect(token::Type::As)?;
            let value_type = parser.parse_type()?;

            Ok(DictTypeEntry {
                key,
                value_type,
                span: Span {
                    start: entry_start,
                    end: parser.prev_token_end,
                },
            })
        })?;
        let end = self.expect(token::Type::RBrace)?.end;

        Ok((entries, Span { start, end }))
    }
}
