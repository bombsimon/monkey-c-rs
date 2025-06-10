use crate::ast::{AssignOperator, Ast, BinaryOperator, LiteralValue, Span, UnaryOperator};
use crate::parser::{Parser, ParserError};
use crate::token;
use std::cell::Cell;

thread_local! {
    static RECURSION_DEPTH: Cell<usize> = const { Cell::new(0) };
}

impl Parser<'_> {
    pub(crate) fn parse_expression(&mut self) -> Result<Ast, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Ast, ParserError> {
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

                Ok(Ast::Assign {
                    target: Box::new(expr),
                    operator,
                    value: Box::new(value),
                    span: Span { start, end },
                })
            }
            _ => Ok(expr),
        }
    }

    fn handle_logical_operator(
        &mut self,
        left: Ast,
        operator_token: token::Type,
    ) -> Result<Ast, ParserError> {
        let (start, _, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::Or => BinaryOperator::Or,
            token::Type::And => BinaryOperator::And,
            _ => unreachable!(),
        };
        let right = self.parse_logical_and()?;
        let (_, _, end) = self.lexer.peek_token();

        Ok(Ast::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        })
    }

    fn parse_logical_or(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_logical_and()?;
        while self.current_token == token::Type::Or {
            expr = self.handle_logical_operator(expr, token::Type::Or)?;
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_equality()?;
        while self.current_token == token::Type::And {
            expr = self.handle_logical_operator(expr, token::Type::And)?;
        }

        Ok(expr)
    }

    fn handle_equality_operator(
        &mut self,
        left: Ast,
        operator_token: token::Type,
    ) -> Result<Ast, ParserError> {
        let (start, _, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::EqualEqual => BinaryOperator::Eq,
            token::Type::BangEqual => BinaryOperator::NotEq,
            _ => unreachable!(),
        };
        let right = self.parse_comparison()?;
        let (_, _, end) = self.lexer.peek_token();

        Ok(Ast::Binary {
            left: Box::new(left),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        })
    }

    fn parse_equality(&mut self) -> Result<Ast, ParserError> {
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

    fn parse_comparison(&mut self) -> Result<Ast, ParserError> {
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

            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            };
        }

        Ok(expr)
    }

    fn handle_binary_operator(
        &mut self,
        left: Ast,
        operators: &[token::Type],
    ) -> Result<Ast, ParserError> {
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

            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_factor()?;
        self.handle_binary_operator(expr, &[token::Type::Plus, token::Type::Minus])
    }

    fn parse_factor(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_unary()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Star, token::Type::Slash, token::Type::Percent],
        )
    }

    fn handle_unary_operator(&mut self) -> Result<Ast, ParserError> {
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

        Ok(Ast::Unary {
            operator,
            operand: Box::new(right),
            span: Span { start, end },
        })
    }

    fn parse_unary(&mut self) -> Result<Ast, ParserError> {
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

        RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            depth.set(d - 1);
        });

        result
    }

    fn parse_postfix(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.current_token == token::Type::Dot {
                self.next_token_span();
                let property = self.parse_identifier()?;
                let (_, _, end) = self.lexer.peek_token();

                expr = Ast::Member {
                    object: Box::new(expr),
                    property,
                    span: Span { start: 0, end },
                };

                self.next_token_span();
                continue;
            }

            if self.current_token == token::Type::LBracket {
                self.next_token_span();
                let index = self.parse_expression()?;
                let (_, _, end) = self.lexer.peek_token();

                expr = Ast::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                    span: Span { start: 0, end },
                };

                self.assert_next_token(&[token::Type::RBracket])?;
                continue;
            }

            if self.current_token == token::Type::LParen {
                self.next_token_span();
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

                let (_, _, end) = self.lexer.peek_token();
                expr = Ast::Call {
                    callee: Box::new(expr),
                    args,
                    span: Span { start: 0, end },
                };

                self.assert_next_token(&[token::Type::RParen])?;
                continue;
            }

            if self.current_token == token::Type::As {
                self.next_token_span();

                let target_type = self.parse_type()?;
                let (_, _, end) = self.lexer.peek_token();

                expr = Ast::TypeCast {
                    expr: Box::new(expr),
                    target_type,
                    span: Span { start: 0, end },
                };

                continue;
            }

            if matches!(
                self.current_token,
                token::Type::PlusPlus | token::Type::MinusMinus
            ) {
                let operator_token = self.current_token.clone();
                let (start, _, _) = self.next_token_span();
                let operator = match operator_token {
                    token::Type::PlusPlus => UnaryOperator::PostInc,
                    token::Type::MinusMinus => UnaryOperator::PostDec,
                    _ => unreachable!(),
                };
                let (_, _, end) = self.lexer.peek_token();

                expr = Ast::Unary {
                    operator,
                    operand: Box::new(expr),
                    span: Span { start, end },
                };

                continue;
            }

            break;
        }

        Ok(expr)
    }

    fn handle_primary_expression(&mut self, token_type: token::Type) -> Result<Ast, ParserError> {
        match token_type {
            token::Type::LParen => {
                self.next_token_span();
                let expr = self.parse_expression()?;
                self.assert_next_token(&[token::Type::RParen])?;
                Ok(expr)
            }
            token::Type::Identifier(name) => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::Identifier(name, Span { start: 0, end }))
            }
            token::Type::Me => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::Me(Span { start: 0, end }))
            }
            token::Type::Self_ => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::Self_(Span { start: 0, end }))
            }
            token::Type::Long(value) => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(
                    LiteralValue::Long(value),
                    Span { start: 0, end },
                ))
            }
            token::Type::Double(value) => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(
                    LiteralValue::Double(value),
                    Span { start: 0, end },
                ))
            }
            token::Type::String(value) => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(
                    LiteralValue::String(value),
                    Span { start: 0, end },
                ))
            }
            token::Type::Boolean(value) => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(
                    LiteralValue::Boolean(value),
                    Span { start: 0, end },
                ))
            }
            token::Type::Null => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(LiteralValue::Null, Span { start: 0, end }))
            }
            token::Type::NaN => {
                let (_, _, end) = self.lexer.peek_token();
                self.next_token_span();
                Ok(Ast::BasicLit(LiteralValue::NaN, Span { start: 0, end }))
            }
            token::Type::Minus => {
                let (start, _, _) = self.next_token_span();
                let operand = self.parse_primary()?;
                let (_, _, end) = self.lexer.peek_token();
                Ok(Ast::Unary {
                    operator: UnaryOperator::Neg,
                    operand: Box::new(operand),
                    span: Span { start, end },
                })
            }
            token::Type::LBracket => {
                self.next_token_span(); // consume [
                let mut elements = Vec::new();
                while self.current_token != token::Type::RBracket {
                    elements.push(self.parse_expression()?);
                    if self.current_token == token::Type::Comma {
                        self.next_token_span();
                    } else if self.current_token == token::Type::RBracket {
                        break;
                    } else {
                        return Err(ParserError::ParseError(format!(
                            "Expected ',' or ']', got {:?}",
                            self.current_token
                        )));
                    }
                }
                self.next_token_span(); // consume ]
                let (_, _, end) = self.lexer.peek_token();
                Ok(Ast::Array(elements, Span { start: 0, end }))
            }
            token::Type::LBrace => {
                self.next_token_span(); // consume {
                let mut elements = Vec::new();
                while self.current_token != token::Type::RBrace {
                    let key = self.parse_primary()?;
                    self.assert_next_token(&[token::Type::Colon])?;
                    let value = self.parse_expression()?;
                    elements.push((key, value));

                    if self.current_token == token::Type::Comma {
                        self.next_token_span();
                    } else if self.current_token == token::Type::RBrace {
                        break;
                    } else {
                        return Err(ParserError::ParseError(format!(
                            "Expected ',' or '}}', got {:?}",
                            self.current_token
                        )));
                    }
                }
                self.next_token_span(); // consume }
                let (_, _, end) = self.lexer.peek_token();
                Ok(Ast::Dictionary(elements, Span { start: 0, end }))
            }
            _ => Err(ParserError::ParseError(format!(
                "Unexpected token in expression: {:?}",
                token_type
            ))),
        }
    }

    pub(crate) fn parse_primary(&mut self) -> Result<Ast, ParserError> {
        let token_type = self.current_token.clone();
        self.handle_primary_expression(token_type)
    }

    fn parse_primary_no_postfix(&mut self) -> Result<Ast, ParserError> {
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
        RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            depth.set(d - 1);
        });
        result
    }

    // Parse an expression without allowing postfixes (for array elements)
    pub(crate) fn parse_expression_no_postfix(&mut self) -> Result<Ast, ParserError> {
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
        RECURSION_DEPTH.with(|depth| {
            let d = depth.get();
            depth.set(d - 1);
        });
        result
    }

    fn parse_assignment_no_postfix(&mut self) -> Result<Ast, ParserError> {
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

    fn parse_logical_or_no_postfix(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_logical_and_no_postfix()?;
        while self.next_token_of_type(&[token::Type::Or]) {
            let (start, _, _) = self.next_token_span(); // advance past 'Or'
            let right = self.parse_logical_and_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Ast::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
                span: Span { start, end },
            };
        }
        Ok(expr)
    }

    fn parse_logical_and_no_postfix(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_equality_no_postfix()?;
        while self.next_token_of_type(&[token::Type::And]) {
            let (start, _, _) = self.next_token_span(); // advance past 'And'
            let right = self.parse_equality_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Ast::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
                span: Span { start, end },
            };
        }
        Ok(expr)
    }

    fn parse_equality_no_postfix(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_comparison_no_postfix()?;
        while matches!(
            self.current_token,
            token::Type::EqualEqual | token::Type::BangEqual
        ) {
            let operator_token = self.current_token.clone();
            let (start, _, _) = self.next_token_span(); // advance past operator and update current_token
            let operator = match operator_token {
                token::Type::EqualEqual => BinaryOperator::Eq,
                token::Type::BangEqual => BinaryOperator::NotEq,
                _ => unreachable!(),
            };
            let right = self.parse_comparison_no_postfix()?;
            let (_, _, end) = self.lexer.peek_token();
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            };
        }
        Ok(expr)
    }

    fn parse_comparison_no_postfix(&mut self) -> Result<Ast, ParserError> {
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
            let (start, _, _) = self.next_token_span(); // advance past operator and update current_token
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
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
                span: Span { start, end },
            };
        }
        Ok(expr)
    }

    fn parse_term_no_postfix(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_factor_no_postfix()?;
        self.handle_binary_operator(expr, &[token::Type::Plus, token::Type::Minus])
    }

    fn parse_factor_no_postfix(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_unary_no_postfix()?;
        self.handle_binary_operator(
            expr,
            &[token::Type::Star, token::Type::Slash, token::Type::Percent],
        )
    }

    fn parse_unary_no_postfix(&mut self) -> Result<Ast, ParserError> {
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

    fn current_token_is(&self, expect: &[token::Type]) -> bool {
        for expected in expect {
            if self.current_token == *expected {
                return true;
            }
        }

        false
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Ast;
    use crate::parser::Parser;

    #[test]
    fn test_assignment_with_type_cast() {
        let input = "class Foo { function bar() { x = [0, 0] as Array<Number>; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");

        /*
        let expected = Ast::Document(vec![Ast::Class {
            name: "Foo".to_string(),
            extends: None,
            annotations: Default::default(),
            body: vec![Ast::Function {
                name: "bar".to_string(),
                args: Default::default(),
                returns: None,
                annotations: Default::default(),
                body: vec![Ast::Assign {
                    target: Box::new(Ast::Identifier("x".to_string(), Span { start: 0, end: 32 })),
                    operator: AssignOperator::Assign,
                    value: Box::new(Ast::TypeCast {
                        expr: Box::new(Ast::Array(
                            vec![
                                Ast::BasicLit(LiteralValue::Long(0), Span { start: 0, end: 36 }),
                                Ast::BasicLit(LiteralValue::Long(0), Span { start: 0, end: 39 }),
                            ],
                            Span { start: 0, end: 48 },
                        )),
                        target_type: ast::Type {
                            ident: "Array".to_string(),
                            generic_params: vec![ast::Type {
                                ident: "Number".to_string(),
                                generic_params: Default::default(),
                                optional: false,
                            }],
                            optional: false,
                        },
                        span: Span { start: 0, end: 59 },
                    }),
                    span: Span { start: 33, end: 59 },
                }],
                visibility: None,
                is_static: false,
                is_hidden: false,
                span: Span { start: 0, end: 61 },
            }],
            span: Span { start: 0, end: 61 },
        }]);

        assert_eq!(expected, ast);
        */

        // Should parse as a document with one class
        if let Ast::Document(nodes) = ast {
            assert_eq!(nodes.len(), 1);
            if let Ast::Class { body, .. } = &nodes[0] {
                // Find the function
                let func = body
                    .iter()
                    .find_map(|node| {
                        if let Ast::Function { body, .. } = node {
                            Some(body)
                        } else {
                            None
                        }
                    })
                    .expect("Should find a function node");
                // Find the assignment statement
                let assign = func
                    .iter()
                    .find_map(|stmt| {
                        if let Ast::Assign { target, value, .. } = stmt {
                            Some((target, value))
                        } else {
                            None
                        }
                    })
                    .expect("Should find an assignment");
                // Target should be identifier 'x'
                if let Ast::Identifier(name, _) = &**assign.0 {
                    assert_eq!(name, "x");
                } else {
                    panic!("Target is not identifier");
                }
                // Value should be a type cast
                if let Ast::TypeCast {
                    expr, target_type, ..
                } = &**assign.1
                {
                    // expr should be an array
                    if let Ast::Array(elems, _) = &**expr {
                        assert_eq!(elems.len(), 2);
                    } else {
                        panic!("Type cast expr is not array");
                    }
                    assert_eq!(target_type.ident, "Array");
                    assert_eq!(target_type.generic_params.len(), 1);
                    assert_eq!(target_type.generic_params[0].ident, "Number");
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

        assert!(matches!(expr, Ast::Binary { .. }));
    }

    #[test]
    fn test_parse_x_eq_null() {
        let input = "x == null";
        let mut parser = Parser::new(input);
        let result = parser.parse_expression();

        assert!(result.is_ok());
    }
}
