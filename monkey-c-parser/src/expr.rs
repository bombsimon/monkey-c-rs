use crate::ast::{
    ArrayEntry, ArrayExpr, AssignExpr, AssignOperator, BinaryExpr, BinaryOperator, CallArg,
    CallExpr, DictEntry, DictExpr, Expr, IdentExpr, IndexExpr, LitExpr, LiteralValue, MemberExpr,
    NewArrayExpr, NewExpr, ParenExpr, Span, TernaryExpr, Type, TypeCastExpr, TypeKind, UnaryExpr,
    UnaryOperator,
};
use crate::parser::{Parser, ParserError};
use crate::token;
use std::cell::Cell;

thread_local! {
    static RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
}

impl Parser<'_> {
    pub(crate) fn parse_expression(&mut self) -> Result<Expr, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_ternary()?;
        match self.current_token {
            token::Type::Assign
            | token::Type::AddAssign
            | token::Type::SubAssign
            | token::Type::MulAssign
            | token::Type::DivAssign
            | token::Type::ModAssign
            | token::Type::BitAndAssign
            | token::Type::BitOrAssign
            | token::Type::BitXorAssign
            | token::Type::LeftShiftAssign
            | token::Type::RightShiftAssign => {
                let operator_token = self.current_token.clone();
                let start = expr.span().start;
                self.next_token_span();
                let operator = match operator_token {
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
                    _ => unreachable!(),
                };
                let value = self.parse_expression()?;
                let end = value.span().end;
                Ok(Expr::Assign(AssignExpr {
                    target: Box::new(expr),
                    operator,
                    value: Box::new(value),
                    span: Span { start, end },
                }))
            }
            _ => Ok(expr),
        }
    }

    /// Parse a ternary `cond ? then : else`. Right-associative: the `else`
    /// branch is itself a ternary so `a ? b : c ? d : e` becomes
    /// `a ? b : (c ? d : e)`.
    fn parse_ternary(&mut self) -> Result<Expr, ParserError> {
        let cond = self.parse_logical_or()?;
        if self.current_token != token::Type::Question {
            return Ok(cond);
        }

        let start = cond.span().start;
        self.next_token_span(); // consume `?`
        let then_expr = self.parse_expression()?;
        self.assert_next_token(&[token::Type::Colon])?;
        let else_expr = self.parse_ternary()?;
        let end = else_expr.span().end;

        Ok(Expr::Ternary(TernaryExpr {
            cond: Box::new(cond),
            then_expr: Box::new(then_expr),
            else_expr: Box::new(else_expr),
            span: Span { start, end },
        }))
    }

    fn handle_logical_operator(
        &mut self,
        left: Expr,
        operator_token: token::Type,
    ) -> Result<Expr, ParserError> {
        let start = left.span().start;
        self.next_token_span();
        let operator = match operator_token {
            token::Type::Or => BinaryOperator::Or,
            token::Type::OrKeyword => BinaryOperator::OrKeyword,
            token::Type::And => BinaryOperator::And,
            token::Type::AndKeyword => BinaryOperator::AndKeyword,
            _ => unreachable!(),
        };
        let right = self.parse_logical_and()?;
        let end = right.span().end;
        Ok(Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        }))
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_logical_and()?;
        while matches!(self.current_token, token::Type::Or | token::Type::OrKeyword) {
            let tok = self.current_token.clone();
            expr = self.handle_logical_operator(expr, tok)?;
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_bitwise_or()?;
        while matches!(
            self.current_token,
            token::Type::And | token::Type::AndKeyword
        ) {
            let tok = self.current_token.clone();
            expr = self.handle_logical_operator(expr, tok)?;
        }

        Ok(expr)
    }

    fn parse_bitwise_or(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_bitwise_xor()?;
        self.handle_binary_operator(expr, &[token::Type::BitOr], Self::parse_bitwise_xor)
    }

    fn parse_bitwise_xor(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_bitwise_and()?;
        self.handle_binary_operator(expr, &[token::Type::BitXor], Self::parse_bitwise_and)
    }

    fn parse_bitwise_and(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_equality()?;
        self.handle_binary_operator(expr, &[token::Type::BitAnd], Self::parse_equality)
    }

    fn handle_equality_operator(
        &mut self,
        left: Expr,
        operator_token: token::Type,
    ) -> Result<Expr, ParserError> {
        let start = left.span().start;
        self.next_token_span();
        let operator = match operator_token {
            token::Type::EqualEqual => BinaryOperator::Eq,
            token::Type::BangEqual => BinaryOperator::NotEq,
            _ => unreachable!(),
        };

        let right = self.parse_comparison()?;
        let end = right.span().end;

        Ok(Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        }))
    }

    fn parse_equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison()?;
        while matches!(
            self.current_token,
            token::Type::EqualEqual | token::Type::BangEqual
        ) {
            let operator_token = self.current_token.clone();
            expr = self.handle_equality_operator(expr, operator_token)?;
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_shift()?;
        while matches!(
            self.current_token,
            token::Type::Less
                | token::Type::LessEqual
                | token::Type::Greater
                | token::Type::GreaterEqual
                | token::Type::InstanceOf
                | token::Type::Has
        ) {
            let operator_token = self.current_token.clone();
            let start = expr.span().start;
            self.next_token_span();

            let operator = match operator_token {
                token::Type::Less => BinaryOperator::Lt,
                token::Type::LessEqual => BinaryOperator::LtEq,
                token::Type::Greater => BinaryOperator::Gt,
                token::Type::GreaterEqual => BinaryOperator::GtEq,
                token::Type::InstanceOf => BinaryOperator::InstanceOf,
                token::Type::Has => BinaryOperator::Has,
                _ => unreachable!(),
            };

            let right = self.parse_shift()?;
            let end = right.span().end;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_shift(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_term()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::LeftShift, token::Type::RightShift],
            Self::parse_term,
        )
    }

    /// Build a left-associative chain at one precedence level: keeps folding
    /// `<left> <op> <right>` while the current token is in `operators`. The
    /// right-hand side is parsed by `parse_operand` — typically the parser
    /// for the next-tighter precedence level. Passing the same-level parser
    /// would yield right-associative parsing.
    fn handle_binary_operator(
        &mut self,
        left: Expr,
        operators: &[token::Type],
        parse_operand: fn(&mut Self) -> Result<Expr, ParserError>,
    ) -> Result<Expr, ParserError> {
        let mut expr = left;
        while self.current_token_is(operators) {
            let operator_token = self.current_token.clone();
            let start = expr.span().start;
            self.next_token_span();

            let operator = match operator_token {
                token::Type::Plus => BinaryOperator::Add,
                token::Type::Minus => BinaryOperator::Sub,
                token::Type::Star => BinaryOperator::Mul,
                token::Type::Slash => BinaryOperator::Div,
                token::Type::Percent => BinaryOperator::Mod,
                token::Type::BitOr => BinaryOperator::BitOr,
                token::Type::BitXor => BinaryOperator::BitXor,
                token::Type::BitAnd => BinaryOperator::BitAnd,
                token::Type::LeftShift => BinaryOperator::LeftShift,
                token::Type::RightShift => BinaryOperator::RightShift,
                _ => unreachable!(),
            };

            let right = parse_operand(self)?;
            let end = right.span().end;

            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_factor()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Plus, token::Type::Minus],
            Self::parse_factor,
        )
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_unary()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Star, token::Type::Slash, token::Type::Percent],
            Self::parse_unary,
        )
    }

    fn handle_unary_operator(&mut self) -> Result<Expr, ParserError> {
        let start = self.current_token_start;
        let operator_token = self.current_token.clone();
        self.next_token_span(); // advance past operator
        let operator = match operator_token {
            token::Type::Plus => UnaryOperator::Pos,
            token::Type::Minus => UnaryOperator::Neg,
            token::Type::Bang => UnaryOperator::Not,
            token::Type::Tilde => UnaryOperator::BitNot,
            token::Type::PlusPlus => UnaryOperator::PreInc,
            token::Type::MinusMinus => UnaryOperator::PreDec,
            _ => unreachable!(),
        };
        let right = self.parse_unary()?;
        let end = right.span().end;
        Ok(Expr::Unary(UnaryExpr {
            operator,
            operand: Box::new(right),
            span: Span { start, end },
        }))
    }

    fn parse_unary(&mut self) -> Result<Expr, ParserError> {
        let depth_check = RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            if d > 1000 {
                return Err(self.parse_error("Expression too complex".to_string()));
            }
            depth.set(d + 1);
            Ok(())
        });
        depth_check?;

        let result = if matches!(
            self.current_token,
            token::Type::Plus
                | token::Type::Minus
                | token::Type::Bang
                | token::Type::Tilde
                | token::Type::PlusPlus
                | token::Type::MinusMinus
        ) {
            self.handle_unary_operator()
        } else {
            self.parse_postfix()
        };

        RECURSION_DEPTH.with(|depth| depth.set(depth.get() - 1));

        result
    }

    fn parse_postfix(&mut self) -> Result<Expr, ParserError> {
        let start = self.current_token_start;
        let mut expr = self.parse_primary()?;

        loop {
            if self.current_token == token::Type::Dot {
                self.next_token_span(); // consume .
                let property = self.parse_identifier()?;
                let end = self.current_token_end;
                self.next_token_span(); // consume property name
                expr = Expr::Member(MemberExpr {
                    object: Box::new(expr),
                    property,
                    span: Span { start, end },
                });
                continue;
            }

            if self.current_token == token::Type::LBracket {
                self.next_token_span(); // consume [
                let index = self.parse_expression()?;
                let end = self.current_token_end; // end of ]
                expr = Expr::Index(IndexExpr {
                    object: Box::new(expr),
                    index: Box::new(index),
                    span: Span { start, end },
                });
                self.assert_next_token(&[token::Type::RBracket])?;
                continue;
            }

            if self.current_token == token::Type::LParen {
                let args_open = self.current_token_start;
                self.next_token_span(); // consume (
                let (args, args_trailing_comma) = self.parse_call_args(token::Type::RParen)?;
                let end = self.current_token_end; // end of )
                expr = Expr::Call(CallExpr {
                    callee: Box::new(expr),
                    args,
                    args_open,
                    args_trailing_comma,
                    span: Span { start, end },
                });
                self.assert_next_token(&[token::Type::RParen])?;
                continue;
            }

            if self.current_token == token::Type::As {
                self.next_token_span(); // consume `as`
                let target_type = self.parse_cast_type()?;
                // current_token_start is the start of the token after the type
                let end = self.current_token_start;
                expr = Expr::TypeCast(TypeCastExpr {
                    expr: Box::new(expr),
                    target_type,
                    span: Span { start, end },
                });
                continue;
            }

            if matches!(
                self.current_token,
                token::Type::PlusPlus | token::Type::MinusMinus
            ) {
                let operator_token = self.current_token.clone();
                let end = self.current_token_end;
                self.next_token_span(); // consume ++/--
                let operator = match operator_token {
                    token::Type::PlusPlus => UnaryOperator::PostInc,
                    token::Type::MinusMinus => UnaryOperator::PostDec,
                    _ => unreachable!(),
                };
                expr = Expr::Unary(UnaryExpr {
                    operator,
                    operand: Box::new(expr),
                    span: Span { start, end },
                });
                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn handle_primary_expression(&mut self, token_type: token::Type) -> Result<Expr, ParserError> {
        match token_type {
            token::Type::LParen => {
                let start = self.current_token_start;
                self.next_token_span();
                let inner = self.parse_expression()?;
                let end = self.current_token_end;
                self.assert_next_token(&[token::Type::RParen])?;

                Ok(Expr::Paren(ParenExpr {
                    inner: Box::new(inner),
                    span: Span { start, end },
                }))
            }
            token::Type::Identifier(name) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Ident(IdentExpr {
                    name,
                    span: Span { start, end },
                }))
            }
            token::Type::Me => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Me(Span { start, end }))
            }
            token::Type::Self_ => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Self_(Span { start, end }))
            }
            token::Type::Bling => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Bling(Span { start, end }))
            }
            token::Type::Colon => {
                let start = self.current_token_start;
                self.next_token_span(); // consume `:`
                let end = self.current_token_end;
                let name = self.parse_symbol_name()?;
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Symbol(name),
                    span: Span { start, end },
                }))
            }
            token::Type::Number(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Number(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Long(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Long(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Hex(digits) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Hex(digits),
                    span: Span { start, end },
                }))
            }
            token::Type::HexLong(digits) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::HexLong(digits),
                    span: Span { start, end },
                }))
            }
            token::Type::Float(lit) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Float(lit),
                    span: Span { start, end },
                }))
            }
            token::Type::Double(lit) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Double(lit),
                    span: Span { start, end },
                }))
            }
            token::Type::String(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::String(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Char(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Char(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Boolean(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Boolean(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Null => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Null,
                    span: Span { start, end },
                }))
            }
            token::Type::NaN => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::NaN,
                    span: Span { start, end },
                }))
            }
            token::Type::Minus => {
                let (start, _, _) = self.next_token_span();
                let operand = self.parse_primary()?;
                let end = operand.span().end;
                Ok(Expr::Unary(UnaryExpr {
                    operator: UnaryOperator::Neg,
                    operand: Box::new(operand),
                    span: Span { start, end },
                }))
            }
            token::Type::LBracket => {
                let start = self.current_token_start;
                self.next_token_span(); // consume [
                let (entries, trailing_comma) = self.parse_array_entries()?;
                let mut end = self.current_token_end; // end of ]
                self.next_token_span(); // consume ]

                let is_byte_array = matches!(
                    &self.current_token,
                    token::Type::Identifier(name) if name == "b"
                );
                if is_byte_array {
                    end = self.current_token_end;
                    self.next_token_span(); // consume `b`
                }

                Ok(Expr::Array(ArrayExpr {
                    entries,
                    trailing_comma,
                    is_byte_array,
                    span: Span { start, end },
                }))
            }
            token::Type::LBrace => {
                let start = self.current_token_start;
                self.next_token_span(); // consume {
                let (entries, trailing_comma) = self.parse_dict_entries()?;
                let end = self.current_token_end; // end of }
                self.next_token_span(); // consume }

                Ok(Expr::Dict(DictExpr {
                    entries,
                    trailing_comma,
                    span: Span { start, end },
                }))
            }
            token::Type::New => {
                let start = self.current_token_start;
                self.next_token_span(); // consume `new`

                // `new [size]` — untyped array allocation.
                if self.current_token == token::Type::LBracket {
                    return self.parse_new_array(start, None);
                }

                // Type follows. Could be `new Foo.Bar(...)` or `new Array<T>[size]`.
                let class = self.parse_dotted_identifier()?;
                let generic_params = if self.current_token == token::Type::Less {
                    self.parse_generic_params()?
                } else {
                    Vec::new()
                };

                // `new Type<T>[size]` — typed array allocation.
                if self.current_token == token::Type::LBracket {
                    let element_type = Type {
                        kind: TypeKind::Named {
                            ident: class,
                            generic_params,
                        },
                        alternatives: Vec::new(),
                        optional: false,
                    };

                    return self.parse_new_array(start, Some(element_type));
                }

                if !generic_params.is_empty() {
                    return Err(self.parse_error(
                        "Generic parameters on `new` are only valid for array allocation"
                            .to_string(),
                    ));
                }

                let args_open = self.current_token_start;
                self.assert_next_token(&[token::Type::LParen])?;
                let (args, args_trailing_comma) = self.parse_call_args(token::Type::RParen)?;
                let end = self.current_token_end;
                self.assert_next_token(&[token::Type::RParen])?;

                Ok(Expr::New(NewExpr {
                    class,
                    args,
                    args_open,
                    args_trailing_comma,
                    span: Span { start, end },
                }))
            }
            _ => Err(self.parse_error(format!("Unexpected token in expression: {token_type:?}"))),
        }
    }

    /// Parse a comma-separated list of call arguments, including trailing
    /// comments after each value. Returns `(args, tail_comments)` where
    /// `tail_comments` is non-empty only when `args` is empty. Does not
    /// consume the closing delimiter.
    pub(crate) fn parse_call_args(
        &mut self,
        close: token::Type,
    ) -> Result<(Vec<CallArg>, bool), ParserError> {
        let mut args: Vec<CallArg> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == close {
                break;
            }

            let value = self.parse_expression()?;
            args.push(CallArg { value });

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                if self.current_token == close {
                    trailing_comma = true;
                    break;
                }
            } else if self.current_token == close {
                break;
            } else {
                return Err(self.parse_error(format!(
                    "Expected ',' or '{}', got {:?}",
                    close, self.current_token
                )));
            }
        }

        Ok((args, trailing_comma))
    }

    /// Parse the body of an array literal. The opening `[` has already been
    /// consumed; the closing `]` is left for the caller. Comments are entirely
    /// ignored — they live in the comment table and attach to entries (or to
    /// the array span as dangling-inside) via `attach_comments`.
    fn parse_array_entries(&mut self) -> Result<(Vec<ArrayEntry>, bool), ParserError> {
        let mut entries: Vec<ArrayEntry> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == token::Type::RBracket {
                break;
            }

            let value = self.parse_expression()?;
            let entry = ArrayEntry { value };

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                entries.push(entry);
                if self.current_token == token::Type::RBracket {
                    trailing_comma = true;
                    break;
                }
            } else if self.current_token == token::Type::RBracket {
                entries.push(entry);
                break;
            } else {
                return Err(
                    self.parse_error(format!("Expected ',' or ']', got {:?}", self.current_token))
                );
            }
        }

        Ok((entries, trailing_comma))
    }

    /// Parse the body of a dict literal. The opening `{` has already been
    /// consumed; the closing `}` is left for the caller. Comments are ignored
    /// for the same reason as in [`parse_array_entries`](Self::parse_array_entries).
    fn parse_dict_entries(&mut self) -> Result<(Vec<DictEntry>, bool), ParserError> {
        let mut entries: Vec<DictEntry> = Vec::new();
        let mut trailing_comma = false;

        loop {
            if self.current_token == token::Type::RBrace {
                break;
            }

            // Key-value entry. Keys may be a symbol/string literal or any
            // variable-like reference (`x`, `Module.CONST`), parsed via the
            // postfix chain so `.member` access threads through correctly.
            let key = self.parse_postfix()?;
            self.assert_next_token(&[token::Type::FatArrow])?;
            let value = self.parse_expression()?;
            let entry = DictEntry { key, value };

            if self.current_token == token::Type::Comma {
                self.next_token_span();
                entries.push(entry);
                if self.current_token == token::Type::RBrace {
                    trailing_comma = true;
                    break;
                }
            } else if self.current_token == token::Type::RBrace {
                entries.push(entry);
                break;
            } else {
                return Err(self.parse_error(format!(
                    "Expected ',' or '}}', got {:?}",
                    self.current_token
                )));
            }
        }

        Ok((entries, trailing_comma))
    }

    /// Parse `[size_expr]` and build a [`NewArrayExpr`]. The opening `new`
    /// (and optional type) has already been consumed by the caller.
    fn parse_new_array(
        &mut self,
        start: usize,
        element_type: Option<Type>,
    ) -> Result<Expr, ParserError> {
        self.assert_next_token(&[token::Type::LBracket])?;
        let size = self.parse_expression()?;
        let mut end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBracket])?;

        let is_byte_array = matches!(
            &self.current_token,
            token::Type::Identifier(name) if name == "b"
        );
        if is_byte_array {
            end = self.current_token_end;
            self.next_token_span(); // consume `b`
        }

        Ok(Expr::NewArray(NewArrayExpr {
            element_type,
            size: Box::new(size),
            is_byte_array,
            span: Span { start, end },
        }))
    }

    /// Parse a generic parameter list `<T, U, ...>` and return the params.
    /// The leading `<` is the current token on entry. Each param may be a
    /// union (`A or B`), in which case the union lives in that param's
    /// `alternatives` field — `or` is not a param separator.
    fn parse_generic_params(&mut self) -> Result<Vec<Type>, ParserError> {
        self.next_token_span(); // consume `<`
        let mut params = Vec::new();
        if self.current_token != token::Type::Greater {
            params.push(self.parse_type()?);
            while self.current_token == token::Type::Comma {
                self.next_token_span();
                params.push(self.parse_type()?);
            }
        }
        self.consume_generic_close()?;

        Ok(params)
    }

    pub(crate) fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let token_type = self.current_token.clone();
        self.handle_primary_expression(token_type)
    }
}
