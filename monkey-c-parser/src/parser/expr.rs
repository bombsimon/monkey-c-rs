use crate::ast::{
    ArrayEntry, ArrayExpr, AssignExpr, AssignOperator, BinaryExpr, BinaryOperator, CallArg,
    CallExpr, DictEntry, DictExpr, Expr, IdentExpr, IndexExpr, LitExpr, LiteralValue, MemberExpr,
    NewArrayExpr, NewExpr, ParenExpr, Separated, Span, TernaryExpr, Type, TypeCastExpr, TypeKind,
    UnaryExpr, UnaryOperator,
};
use crate::parser::{Parser, ParserError};
use crate::token;

const MAX_EXPRESSION_DEPTH: usize = 1000;

impl Parser<'_> {
    pub(crate) fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_ternary()?;
        let Some(operator) = assign_operator(&self.current_token) else {
            return Ok(expr);
        };

        let op_pos = self.advance().start;
        let value = self.parse_expression()?;

        Ok(Expr::Assign(AssignExpr {
            span: Span {
                start: expr.span().start,
                end: value.span().end,
            },
            target: Box::new(expr),
            operator,
            op_pos,
            value: Box::new(value),
        }))
    }

    /// Parse a ternary `cond ? then : else`. Right-associative: the `else` branch is itself a
    /// ternary so `a ? b : c ? d : e` becomes `a ? b : (c ? d : e)`.
    fn parse_ternary(&mut self) -> Result<Expr, ParserError> {
        let condition = self.parse_logical_or()?;
        if self.current_token != token::Type::Question {
            return Ok(condition);
        }

        let question_pos = self.advance().start;
        let then_expr = self.parse_expression()?;
        let colon_pos = self.expect(token::Type::Colon)?.start;
        let else_expr = self.parse_ternary()?;

        Ok(Expr::Ternary(TernaryExpr {
            span: Span {
                start: condition.span().start,
                end: else_expr.span().end,
            },
            condition: Box::new(condition),
            question_pos,
            then_expr: Box::new(then_expr),
            colon_pos,
            else_expr: Box::new(else_expr),
        }))
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParserError> {
        self.parse_binary_level(1, Self::parse_logical_and)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParserError> {
        self.parse_binary_level(2, Self::parse_comparison)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        self.parse_binary_level(3, Self::parse_term)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        self.parse_binary_level(4, Self::parse_factor)
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        self.parse_binary_level(5, Self::parse_unary)
    }

    /// Fold a left-associative chain of the operators at `precedence`, with operands parsed by the
    /// next tighter level.
    fn parse_binary_level(
        &mut self,
        precedence: u8,
        parse_operand: fn(&mut Self) -> Result<Expr, ParserError>,
    ) -> Result<Expr, ParserError> {
        let mut expr = parse_operand(self)?;

        while let Some(operator) = binary_operator(&self.current_token)
            .filter(|operator| operator.precedence() == precedence)
        {
            let op_pos = self.advance().start;
            let right = parse_operand(self)?;

            expr = Expr::Binary(BinaryExpr {
                span: Span {
                    start: expr.span().start,
                    end: right.span().end,
                },
                left: Box::new(expr),
                operator,
                op_pos,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        if self.expression_depth > MAX_EXPRESSION_DEPTH {
            return Err(self.parse_error("Expression too complex"));
        }

        self.expression_depth += 1;

        let result = if let Some(operator) = prefix_operator(&self.current_token) {
            let start = self.advance().start;
            self.parse_unary().map(|operand| {
                Expr::Unary(UnaryExpr {
                    operator,
                    span: Span {
                        start,
                        end: operand.span().end,
                    },
                    operand: Box::new(operand),
                })
            })
        } else {
            self.parse_postfix()
        };

        self.expression_depth -= 1;

        result
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParserError> {
        let start = self.current_token_start;
        let mut expr = self.parse_primary()?;

        loop {
            expr = match self.current_token {
                token::Type::Dot => {
                    self.advance();
                    let property = self.parse_identifier()?;

                    Expr::Member(MemberExpr {
                        object: Box::new(expr),
                        property,
                        span: Span {
                            start,
                            end: self.prev_token_end,
                        },
                    })
                }
                token::Type::LBracket => {
                    self.advance();
                    let index = self.parse_expression()?;
                    let end = self.expect(token::Type::RBracket)?.end;

                    Expr::Index(IndexExpr {
                        object: Box::new(expr),
                        index: Box::new(index),
                        span: Span { start, end },
                    })
                }
                token::Type::LParen => {
                    let args_open = self.advance().start;
                    let args = self.parse_call_args(token::Type::RParen)?;
                    let end = self.expect(token::Type::RParen)?.end;

                    Expr::Call(CallExpr {
                        callee: Box::new(expr),
                        args,
                        args_open,
                        span: Span { start, end },
                    })
                }
                token::Type::As => {
                    self.advance();
                    let target_type = self.parse_cast_type()?;

                    Expr::TypeCast(TypeCastExpr {
                        expr: Box::new(expr),
                        target_type,
                        span: Span {
                            start,
                            end: self.prev_token_end,
                        },
                    })
                }
                token::Type::PlusPlus | token::Type::MinusMinus => {
                    let operator = if self.current_token == token::Type::PlusPlus {
                        UnaryOperator::PostInc
                    } else {
                        UnaryOperator::PostDec
                    };
                    let end = self.advance().end;

                    Expr::Unary(UnaryExpr {
                        operator,
                        operand: Box::new(expr),
                        span: Span { start, end },
                    })
                }
                _ => return Ok(expr),
            };
        }
    }

    fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        if let Some(value) = literal_value(&self.current_token) {
            let span = self.advance();

            return Ok(Expr::Lit(LitExpr { value, span }));
        }

        match self.current_token.clone() {
            token::Type::LParen => {
                let start = self.advance().start;
                let inner = self.parse_expression()?;
                let end = self.expect(token::Type::RParen)?.end;

                Ok(Expr::Paren(ParenExpr {
                    inner: Box::new(inner),
                    span: Span { start, end },
                }))
            }
            token::Type::Identifier(name) => Ok(Expr::Ident(IdentExpr {
                name,
                span: self.advance(),
            })),
            token::Type::Me => Ok(Expr::Me(self.advance())),
            token::Type::Self_ => Ok(Expr::Self_(self.advance())),
            token::Type::Bling => Ok(Expr::Bling(self.advance())),
            token::Type::Colon => {
                let start = self.advance().start;
                let name = self.parse_symbol_name()?;

                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Symbol(name),
                    span: Span {
                        start,
                        end: self.prev_token_end,
                    },
                }))
            }
            token::Type::LBracket => {
                let start = self.advance().start;
                let entries = self.parse_separated(&token::Type::RBracket, |parser| {
                    Ok(ArrayEntry {
                        value: parser.parse_expression()?,
                    })
                })?;
                self.advance();
                let is_byte_array = self.consume_byte_array_suffix();

                Ok(Expr::Array(ArrayExpr {
                    entries,
                    is_byte_array,
                    span: Span {
                        start,
                        end: self.prev_token_end,
                    },
                }))
            }
            token::Type::LBrace => {
                let start = self.advance().start;
                let entries = self.parse_separated(&token::Type::RBrace, |parser| {
                    let key = parser.parse_expression()?;
                    let arrow_pos = parser.expect(token::Type::FatArrow)?.start;
                    let value = parser.parse_expression()?;

                    Ok(DictEntry {
                        key,
                        arrow_pos,
                        value,
                    })
                })?;
                let end = self.advance().end;

                Ok(Expr::Dict(DictExpr {
                    entries,
                    span: Span { start, end },
                }))
            }
            token::Type::New => self.parse_new(),
            token_type => {
                Err(self.parse_error(format!("Unexpected token in expression: {token_type:?}")))
            }
        }
    }

    /// Parse `new Foo.Bar(...)`, `new self.classDef_(...)` or an array allocation like
    /// `new Array<T>[size]` and `new [size]`.
    fn parse_new(&mut self) -> Result<Expr, ParserError> {
        let start = self.advance().start;

        if self.current_token == token::Type::LBracket {
            return self.parse_new_array(start, None);
        }

        let class_start = self.current_token_start;
        let class = if matches!(self.current_token, token::Type::Self_ | token::Type::Me) {
            let name = self.current_token.to_string();
            self.advance();
            self.parse_dotted_identifier_continuation(name)?
        } else {
            self.parse_dotted_identifier()?
        };
        let generic_params = if self.current_token == token::Type::Less {
            self.parse_generic_params()?
        } else {
            Vec::new()
        };

        let class_span = Span {
            start: class_start,
            end: self.prev_token_end,
        };

        if self.current_token == token::Type::LBracket {
            let element_type = Type {
                kind: TypeKind::Named {
                    ident: class,
                    generic_params,
                },
                alternatives: Vec::new(),
                optional: false,
                span: class_span,
            };

            return self.parse_new_array(start, Some(element_type));
        }

        if !generic_params.is_empty() {
            return Err(
                self.parse_error("Generic parameters on `new` are only valid for array allocation")
            );
        }

        // `new Foo` without a trailing `(...)` is equivalent to `new Foo()`.
        if self.current_token != token::Type::LParen {
            return Ok(Expr::New(NewExpr {
                class,
                class_span,
                args: Separated::default(),
                args_open: None,
                span: Span {
                    start,
                    end: self.prev_token_end,
                },
            }));
        }

        let args_open = self.advance().start;
        let args = self.parse_call_args(token::Type::RParen)?;
        let end = self.expect(token::Type::RParen)?.end;

        Ok(Expr::New(NewExpr {
            class,
            class_span,
            args,
            args_open: Some(args_open),
            span: Span { start, end },
        }))
    }

    fn parse_new_array(
        &mut self,
        start: usize,
        element_type: Option<Type>,
    ) -> Result<Expr, ParserError> {
        self.expect(token::Type::LBracket)?;
        let size = self.parse_expression()?;
        self.expect(token::Type::RBracket)?;
        let is_byte_array = self.consume_byte_array_suffix();

        Ok(Expr::NewArray(NewArrayExpr {
            element_type,
            size: Box::new(size),
            is_byte_array,
            span: Span {
                start,
                end: self.prev_token_end,
            },
        }))
    }

    /// Consume the `b` that marks an array as a `Lang.ByteArray`, as in `[1, 2]b`.
    fn consume_byte_array_suffix(&mut self) -> bool {
        let is_byte_array = matches!(
            &self.current_token,
            token::Type::Identifier(name) if name == "b"
        );

        if is_byte_array {
            self.advance();
        }

        is_byte_array
    }

    /// Parse a comma-separated list of call arguments. Does not consume the closing delimiter.
    pub(crate) fn parse_call_args(
        &mut self,
        close: token::Type,
    ) -> Result<Separated<CallArg>, ParserError> {
        self.parse_separated(&close, |parser| {
            Ok(CallArg {
                value: parser.parse_expression()?,
            })
        })
    }

    /// Parse items with `parse_item` until `close`, which is left for the caller, recording where
    /// each `,` is.
    pub(crate) fn parse_separated<T>(
        &mut self,
        close: &token::Type,
        mut parse_item: impl FnMut(&mut Self) -> Result<T, ParserError>,
    ) -> Result<Separated<T>, ParserError> {
        let mut separated = Separated::default();

        while self.current_token != *close {
            separated.items.push(parse_item(self)?);

            if self.current_token == token::Type::Comma {
                separated.commas.push(self.advance().start);
            } else if self.current_token != *close {
                return Err(self.parse_error(format!(
                    "Expected ',' or '{}', got {:?}",
                    close, self.current_token
                )));
            }
        }

        Ok(separated)
    }
}

fn literal_value(token: &token::Type) -> Option<LiteralValue> {
    let value = match token {
        token::Type::Number(value) => LiteralValue::Number(value.clone()),
        token::Type::Long(value) => LiteralValue::Long(value.clone()),
        token::Type::Hex(value) => LiteralValue::Hex(value.clone()),
        token::Type::HexLong(value) => LiteralValue::HexLong(value.clone()),
        token::Type::Float(value) => LiteralValue::Float(value.clone()),
        token::Type::Double(value) => LiteralValue::Double(value.clone()),
        token::Type::String(value) => LiteralValue::String(value.clone()),
        token::Type::Char(value) => LiteralValue::Char(value.clone()),
        token::Type::Boolean(value) => LiteralValue::Boolean(*value),
        token::Type::Null => LiteralValue::Null,
        token::Type::NaN => LiteralValue::NaN,
        _ => return None,
    };

    Some(value)
}

fn binary_operator(token: &token::Type) -> Option<BinaryOperator> {
    let operator = match token {
        token::Type::Or => BinaryOperator::Or,
        token::Type::OrKeyword => BinaryOperator::OrKeyword,
        token::Type::And => BinaryOperator::And,
        token::Type::AndKeyword => BinaryOperator::AndKeyword,
        token::Type::EqualEqual => BinaryOperator::Eq,
        token::Type::BangEqual => BinaryOperator::NotEq,
        token::Type::Less => BinaryOperator::Lt,
        token::Type::LessEqual => BinaryOperator::LtEq,
        token::Type::Greater => BinaryOperator::Gt,
        token::Type::GreaterEqual => BinaryOperator::GtEq,
        token::Type::Plus => BinaryOperator::Add,
        token::Type::Minus => BinaryOperator::Sub,
        token::Type::BitOr => BinaryOperator::BitOr,
        token::Type::BitXor => BinaryOperator::BitXor,
        token::Type::Star => BinaryOperator::Mul,
        token::Type::Slash => BinaryOperator::Div,
        token::Type::Percent => BinaryOperator::Mod,
        token::Type::LeftShift => BinaryOperator::LeftShift,
        token::Type::RightShift => BinaryOperator::RightShift,
        token::Type::BitAnd => BinaryOperator::BitAnd,
        token::Type::InstanceOf => BinaryOperator::InstanceOf,
        token::Type::Has => BinaryOperator::Has,
        _ => return None,
    };

    Some(operator)
}

fn prefix_operator(token: &token::Type) -> Option<UnaryOperator> {
    let operator = match token {
        token::Type::Plus => UnaryOperator::Pos,
        token::Type::Minus => UnaryOperator::Neg,
        token::Type::Bang => UnaryOperator::Not,
        token::Type::Tilde => UnaryOperator::BitNot,
        token::Type::PlusPlus => UnaryOperator::PreInc,
        token::Type::MinusMinus => UnaryOperator::PreDec,
        token::Type::At => UnaryOperator::ResourceRef,
        _ => return None,
    };

    Some(operator)
}

fn assign_operator(token: &token::Type) -> Option<AssignOperator> {
    let operator = match token {
        token::Type::Assign => AssignOperator::Assign,
        token::Type::AddAssign => AssignOperator::AddAssign,
        token::Type::SubAssign => AssignOperator::SubAssign,
        token::Type::MulAssign => AssignOperator::MulAssign,
        token::Type::DivAssign => AssignOperator::DivAssign,
        token::Type::ModAssign => AssignOperator::ModAssign,
        token::Type::BitAndAssign => AssignOperator::BitAndAssign,
        token::Type::BitOrAssign => AssignOperator::BitOrAssign,
        token::Type::BitXorAssign => AssignOperator::BitXorAssign,
        token::Type::LeftShiftAssign => AssignOperator::LeftShiftAssign,
        token::Type::RightShiftAssign => AssignOperator::RightShiftAssign,
        _ => return None,
    };

    Some(operator)
}
