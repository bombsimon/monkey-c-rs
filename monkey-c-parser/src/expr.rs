use crate::ast::{
    ArrayExpr, AssignExpr, AssignOperator, BinaryExpr, BinaryOperator, CallExpr, DictExpr, Expr,
    IdentExpr, IndexExpr, LitExpr, LiteralValue, MemberExpr, NewExpr, Span, TypeCastExpr,
    UnaryExpr, UnaryOperator,
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
        let expr = self.parse_logical_or()?;
        match self.current_token {
            token::Type::Assign
            | token::Type::AddAssign
            | token::Type::SubAssign
            | token::Type::MulAssign
            | token::Type::DivAssign
            | token::Type::ModAssign
            | token::Type::BitAndAssign
            | token::Type::BitOrAssign
            | token::Type::BitXorAssign => {
                let operator_token = self.current_token.clone();
                let (start, _, _) = self.next_token_span();
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
                    _ => unreachable!(),
                };
                let value = self.parse_expression()?;
                let (_, _, end) = self.lexer.peek_token();
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

    fn handle_logical_operator(
        &mut self,
        left: Expr,
        operator_token: token::Type,
    ) -> Result<Expr, ParserError> {
        let (start, _, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::Or => BinaryOperator::Or,
            token::Type::And => BinaryOperator::And,
            _ => unreachable!(),
        };
        let right = self.parse_logical_and()?;
        let (_, _, end) = self.lexer.peek_token();
        Ok(Expr::Binary(BinaryExpr {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        }))
    }

    fn parse_logical_or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_logical_and()?;
        while self.current_token == token::Type::Or {
            expr = self.handle_logical_operator(expr, token::Type::Or)?;
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_equality()?;
        while self.current_token == token::Type::And {
            expr = self.handle_logical_operator(expr, token::Type::And)?;
        }

        Ok(expr)
    }

    fn handle_equality_operator(
        &mut self,
        left: Expr,
        operator_token: token::Type,
    ) -> Result<Expr, ParserError> {
        let (start, _, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::EqualEqual => BinaryOperator::Eq,
            token::Type::BangEqual => BinaryOperator::NotEq,
            _ => unreachable!(),
        };
        let right = self.parse_comparison()?;
        let (_, _, end) = self.lexer.peek_token();
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
        let mut expr = self.parse_term()?;
        while matches!(
            self.current_token,
            token::Type::Less
                | token::Type::LessEqual
                | token::Type::Greater
                | token::Type::GreaterEqual
                | token::Type::InstanceOf
        ) {
            let operator_token = self.current_token.clone();
            let (start, _, _) = self.next_token_span();
            let operator = match operator_token {
                token::Type::Less => BinaryOperator::Lt,
                token::Type::LessEqual => BinaryOperator::LtEq,
                token::Type::Greater => BinaryOperator::Gt,
                token::Type::GreaterEqual => BinaryOperator::GtEq,
                token::Type::InstanceOf => BinaryOperator::InstanceOf,
                _ => unreachable!(),
            };
            let right = self.parse_term()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn handle_binary_operator(
        &mut self,
        left: Expr,
        operators: &[token::Type],
    ) -> Result<Expr, ParserError> {
        let mut expr = left;
        while self.current_token_is(operators) {
            let operator_token = self.current_token.clone();
            let (start, _, _) = self.next_token_span();
            let operator = match operator_token {
                token::Type::Plus => BinaryOperator::Add,
                token::Type::Minus => BinaryOperator::Sub,
                token::Type::Star => BinaryOperator::Mul,
                token::Type::Slash => BinaryOperator::Div,
                token::Type::Percent => BinaryOperator::Mod,
                _ => unreachable!(),
            };
            let right = self.parse_factor()?;
            let (_, _, end) = self.lexer.peek_token();
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
        self.handle_binary_operator(expr, &[token::Type::Plus, token::Type::Minus])
    }

    fn parse_factor(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_unary()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Star, token::Type::Slash, token::Type::Percent],
        )
    }

    fn handle_unary_operator(&mut self) -> Result<Expr, ParserError> {
        let (start, operator_token, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::Minus => UnaryOperator::Neg,
            token::Type::Bang => UnaryOperator::Not,
            token::Type::Tilde => UnaryOperator::BitNot,
            token::Type::PlusPlus => UnaryOperator::PreInc,
            token::Type::MinusMinus => UnaryOperator::PreDec,
            _ => unreachable!(),
        };
        let right = self.parse_unary()?;
        let (_, _, end) = self.lexer.peek_token();
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
                return Err(ParserError::ParseError(
                    "Expression too complex".to_string(),
                ));
            }
            depth.set(d + 1);
            Ok(())
        });
        depth_check?;

        let result = if matches!(
            self.current_token,
            token::Type::Minus
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
                self.next_token_span(); // consume (
                let mut args = Vec::new();
                if self.current_token != token::Type::RParen {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.current_token == token::Type::RParen {
                            break;
                        }
                        self.assert_next_token(&[token::Type::Comma])?;
                    }
                }
                let end = self.current_token_end; // end of )
                expr = Expr::Call(CallExpr {
                    callee: Box::new(expr),
                    args,
                    span: Span { start, end },
                });
                self.assert_next_token(&[token::Type::RParen])?;
                continue;
            }

            if self.current_token == token::Type::As {
                self.next_token_span(); // consume `as`
                let target_type = self.parse_type()?;
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
                self.next_token_span();
                let expr = self.parse_expression()?;
                self.assert_next_token(&[token::Type::RParen])?;
                Ok(expr)
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
            token::Type::Long(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Long(value),
                    span: Span { start, end },
                }))
            }
            token::Type::Double(value) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Expr::Lit(LitExpr {
                    value: LiteralValue::Double(value),
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
                let (_, _, end) = self.lexer.peek_token();
                Ok(Expr::Unary(UnaryExpr {
                    operator: UnaryOperator::Neg,
                    operand: Box::new(operand),
                    span: Span { start, end },
                }))
            }
            token::Type::LBracket => {
                let start = self.current_token_start;
                self.next_token_span(); // consume [
                let mut elements = Vec::new();
                let mut trailing_comma = false;
                while self.current_token != token::Type::RBracket {
                    elements.push(self.parse_expression()?);
                    if self.current_token == token::Type::Comma {
                        self.next_token_span(); // consume ,
                        if self.current_token == token::Type::RBracket {
                            trailing_comma = true;
                            break;
                        }
                    } else if self.current_token == token::Type::RBracket {
                        break;
                    } else {
                        return Err(ParserError::ParseError(format!(
                            "Expected ',' or ']', got {:?}",
                            self.current_token
                        )));
                    }
                }

                let end = self.current_token_end; // end of ]
                self.next_token_span(); // consume ]

                Ok(Expr::Array(ArrayExpr {
                    elements,
                    trailing_comma,
                    span: Span { start, end },
                }))
            }
            token::Type::LBrace => {
                let start = self.current_token_start;
                self.next_token_span(); // consume {
                let mut pairs = Vec::new();
                let mut trailing_comma = false;
                while self.current_token != token::Type::RBrace {
                    let key = self.parse_primary()?;
                    self.assert_next_token(&[token::Type::Colon])?;
                    let value = self.parse_expression()?;
                    pairs.push((key, value));
                    if self.current_token == token::Type::Comma {
                        self.next_token_span(); // consume ,
                        if self.current_token == token::Type::RBrace {
                            trailing_comma = true;
                            break;
                        }
                    } else if self.current_token == token::Type::RBrace {
                        break;
                    } else {
                        return Err(ParserError::ParseError(format!(
                            "Expected ',' or '}}', got {:?}",
                            self.current_token
                        )));
                    }
                }

                let end = self.current_token_end; // end of }
                self.next_token_span(); // consume }

                Ok(Expr::Dict(DictExpr {
                    pairs,
                    trailing_comma,
                    span: Span { start, end },
                }))
            }
            token::Type::New => {
                let start = self.current_token_start;
                self.next_token_span(); // consume `new`
                let class = self.parse_dotted_identifier()?;
                self.assert_next_token(&[token::Type::LParen])?;
                let mut args = Vec::new();
                if self.current_token != token::Type::RParen {
                    loop {
                        args.push(self.parse_expression()?);
                        if self.current_token == token::Type::RParen {
                            break;
                        }
                        self.assert_next_token(&[token::Type::Comma])?;
                    }
                }
                let end = self.current_token_end;
                self.assert_next_token(&[token::Type::RParen])?;
                Ok(Expr::New(NewExpr {
                    class,
                    args,
                    span: Span { start, end },
                }))
            }
            _ => Err(ParserError::ParseError(format!(
                "Unexpected token in expression: {:?}",
                token_type
            ))),
        }
    }

    pub(crate) fn parse_primary(&mut self) -> Result<Expr, ParserError> {
        let token_type = self.current_token.clone();
        self.handle_primary_expression(token_type)
    }

    fn parse_primary_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let exceeded = RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            depth.set(d + 1);
            d > 128
        });

        if exceeded {
            return Err(ParserError::ParseError(
                "Recursion limit exceeded in parse_primary_no_postfix".to_string(),
            ));
        }

        let token_type = self.current_token.clone();
        let result = self.handle_primary_expression(token_type);
        RECURSION_DEPTH.with(|depth| depth.set(depth.get() - 1));

        result
    }

    pub(crate) fn parse_expression_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let exceeded = RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            depth.set(d + 1);
            d > 128
        });

        if exceeded {
            return Err(ParserError::ParseError(
                "Recursion limit exceeded in parse_expression_no_postfix".to_string(),
            ));
        }

        let result = self.parse_assignment_no_postfix();
        RECURSION_DEPTH.with(|depth| depth.set(depth.get() - 1));

        result
    }

    fn parse_assignment_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_logical_or_no_postfix()?;
        match self.current_token {
            token::Type::Assign
            | token::Type::AddAssign
            | token::Type::SubAssign
            | token::Type::MulAssign
            | token::Type::DivAssign
            | token::Type::ModAssign
            | token::Type::BitAndAssign
            | token::Type::BitOrAssign
            | token::Type::BitXorAssign => Err(ParserError::ParseError(
                "Assignment not allowed in this context".to_string(),
            )),
            _ => Ok(expr),
        }
    }

    fn parse_logical_or_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_logical_and_no_postfix()?;
        while self.next_token_of_type(&[token::Type::Or]) {
            let (start, _, _) = self.next_token_span();
            let right = self.parse_logical_and_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_logical_and_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_equality_no_postfix()?;
        while self.next_token_of_type(&[token::Type::And]) {
            let (start, _, _) = self.next_token_span();
            let right = self.parse_equality_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_equality_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_comparison_no_postfix()?;
        while matches!(
            self.current_token,
            token::Type::EqualEqual | token::Type::BangEqual
        ) {
            let operator_token = self.current_token.clone();
            let (start, _, _) = self.next_token_span();
            let operator = match operator_token {
                token::Type::EqualEqual => BinaryOperator::Eq,
                token::Type::BangEqual => BinaryOperator::NotEq,
                _ => unreachable!(),
            };
            let right = self.parse_comparison_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_comparison_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.parse_term_no_postfix()?;
        while matches!(
            self.current_token,
            token::Type::Less
                | token::Type::LessEqual
                | token::Type::Greater
                | token::Type::GreaterEqual
                | token::Type::InstanceOf
        ) {
            let operator_token = self.current_token.clone();
            let (start, _, _) = self.next_token_span();
            let operator = match operator_token {
                token::Type::Less => BinaryOperator::Lt,
                token::Type::LessEqual => BinaryOperator::LtEq,
                token::Type::Greater => BinaryOperator::Gt,
                token::Type::GreaterEqual => BinaryOperator::GtEq,
                token::Type::InstanceOf => BinaryOperator::InstanceOf,
                _ => unreachable!(),
            };
            let right = self.parse_term_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Expr::Binary(BinaryExpr {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            });
        }

        Ok(expr)
    }

    fn parse_term_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_factor_no_postfix()?;
        self.handle_binary_operator(expr, &[token::Type::Plus, token::Type::Minus])
    }

    fn parse_factor_no_postfix(&mut self) -> Result<Expr, ParserError> {
        let expr = self.parse_unary_no_postfix()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Star, token::Type::Slash, token::Type::Percent],
        )
    }

    fn parse_unary_no_postfix(&mut self) -> Result<Expr, ParserError> {
        if self.next_token_of_type(&[
            token::Type::Minus,
            token::Type::Bang,
            token::Type::Tilde,
            token::Type::PlusPlus,
            token::Type::MinusMinus,
        ]) {
            self.handle_unary_operator()
        } else {
            self.parse_primary_no_postfix()
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::{Ast, Expr, Stmt};
    use crate::parser::Parser;

    #[test]
    fn test_assignment_with_type_cast() {
        let input = "class Foo { function bar() { x = [0, 0] as Array<Number>; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");

        if let Ast::Document(nodes) = ast {
            assert_eq!(nodes.len(), 1);
            if let Ast::Class(class) = &nodes[0] {
                let func = class
                    .body
                    .iter()
                    .find_map(|node| {
                        if let Ast::Function(f) = node {
                            Some(f)
                        } else {
                            None
                        }
                    })
                    .expect("Should find a function node");

                let assign = func
                    .body
                    .stmts
                    .iter()
                    .find_map(|stmt| {
                        if let Stmt::Expr(Expr::Assign(a)) = stmt {
                            Some(a)
                        } else {
                            None
                        }
                    })
                    .expect("Should find an assignment");

                if let Expr::Ident(ident) = &*assign.target {
                    assert_eq!(ident.name, "x");
                } else {
                    panic!("Target is not identifier");
                }

                if let Expr::TypeCast(tc) = &*assign.value {
                    if let Expr::Array(arr) = &*tc.expr {
                        assert_eq!(arr.elements.len(), 2);
                    } else {
                        panic!("Type cast expr is not array");
                    }
                    assert_eq!(tc.target_type.ident, "Array");
                    assert_eq!(tc.target_type.generic_params.len(), 1);
                    assert_eq!(tc.target_type.generic_params[0].ident, "Number");
                } else {
                    panic!("Value is not a type cast");
                }
            } else {
                panic!("Not a class node");
            }
        }
    }

    #[test]
    fn test_parse_subtraction() {
        let input = "a - b";
        let mut parser = Parser::new(input);
        let expr = parser.parse_expression().unwrap();
        assert!(matches!(expr, Expr::Binary(_)));
    }

    #[test]
    fn test_parse_x_eq_null() {
        let input = "x == null";
        let mut parser = Parser::new(input);
        let result = parser.parse_expression();
        assert!(result.is_ok());
    }
}
