#![allow(dead_code)]
use crate::ast::{AssignOperator, Ast, BinaryOperator, LiteralValue, UnaryOperator};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    pub(crate) fn parse_expression(&mut self) -> Result<Ast, ParserError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_logical_or()?;

        if self.next_token_of_type(&[
            token::Type::Assign,
            token::Type::AssignAdd,
            token::Type::AssignSubtract,
            token::Type::AssignMultiply,
            token::Type::AssignDivide,
            token::Type::AssignRemainder,
            token::Type::AssignAnd,
            token::Type::AssignOr,
            token::Type::AssignXor,
        ]) {
            let operator = match self.next_token_type() {
                token::Type::Assign => AssignOperator::Assign,
                token::Type::AssignAdd => AssignOperator::AddAssign,
                token::Type::AssignSubtract => AssignOperator::SubAssign,
                token::Type::AssignMultiply => AssignOperator::MulAssign,
                token::Type::AssignDivide => AssignOperator::DivAssign,
                token::Type::AssignRemainder => AssignOperator::ModAssign,
                token::Type::AssignAnd => AssignOperator::BitAndAssign,
                token::Type::AssignOr => AssignOperator::BitOrAssign,
                token::Type::AssignXor => AssignOperator::BitXorAssign,
                _ => unreachable!(),
            };

            let value = self.parse_expression()?;
            Ok(Ast::Assign {
                target: Box::new(expr),
                operator,
                value: Box::new(value),
            })
        } else {
            Ok(expr)
        }
    }

    fn parse_logical_or(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_logical_and()?;

        while self.next_token_of_type(&[token::Type::LogicalOr]) {
            self.consume_token();
            let right = self.parse_logical_and()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::Or,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_logical_and(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_equality()?;

        while self.next_token_of_type(&[token::Type::LogicalAnd]) {
            self.consume_token();
            let right = self.parse_equality()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator: BinaryOperator::And,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_equality(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_comparison()?;

        while self.next_token_of_type(&[token::Type::Equal, token::Type::NotEqual]) {
            let operator = match self.next_token_type() {
                token::Type::Equal => BinaryOperator::Eq,
                token::Type::NotEqual => BinaryOperator::NotEq,
                _ => unreachable!(),
            };
            let right = self.parse_comparison()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_comparison(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_cast()?;

        while self.next_token_of_type(&[
            token::Type::Less,
            token::Type::LessOrEqual,
            token::Type::Greater,
            token::Type::GreaterOrEqual,
        ]) {
            let operator = match self.next_token_type() {
                token::Type::Less => BinaryOperator::Lt,
                token::Type::LessOrEqual => BinaryOperator::LtEq,
                token::Type::Greater => BinaryOperator::Gt,
                token::Type::GreaterOrEqual => BinaryOperator::GtEq,
                _ => unreachable!(),
            };
            let right = self.parse_cast()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_cast(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_term()?;

        while self.next_token_of_type(&[token::Type::As]) {
            self.consume_token(); // consume 'as'
            let target_type = self.parse_type()?;
            expr = Ast::TypeCast {
                expr: Box::new(expr),
                target_type,
            };
        }

        Ok(expr)
    }

    fn parse_term(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_factor()?;

        while self.next_token_of_type(&[token::Type::Plus, token::Type::Minus]) {
            let operator = match self.next_token_type() {
                token::Type::Plus => BinaryOperator::Add,
                token::Type::Minus => BinaryOperator::Sub,
                _ => unreachable!(),
            };
            
            // Skip newlines after operator
            while self.next_token_of_type(&[token::Type::Newline]) {
                self.consume_token();
            }
            
            let right = self.parse_factor()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_factor(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_unary()?;

        while self.next_token_of_type(&[
            token::Type::Multiply,
            token::Type::Divide,
            token::Type::Modulu,
        ]) {
            let operator = match self.next_token_type() {
                token::Type::Multiply => BinaryOperator::Mul,
                token::Type::Divide => BinaryOperator::Div,
                token::Type::Modulu => BinaryOperator::Mod,
                _ => unreachable!(),
            };
            
            // Skip newlines after operator
            while self.next_token_of_type(&[token::Type::Newline]) {
                self.consume_token();
            }
            
            let right = self.parse_unary()?;
            expr = Ast::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }

        Ok(expr)
    }

    fn parse_unary(&mut self) -> Result<Ast, ParserError> {
        if self.next_token_of_type(&[
            token::Type::Minus,
            token::Type::LogicalNot,
            token::Type::BitwiseNot,
            token::Type::Increment,
            token::Type::Decrement,
        ]) {
            let operator = match self.next_token_type() {
                token::Type::Minus => UnaryOperator::Neg,
                token::Type::LogicalNot => UnaryOperator::Not,
                token::Type::BitwiseNot => UnaryOperator::BitNot,
                token::Type::Increment => UnaryOperator::PreInc,
                token::Type::Decrement => UnaryOperator::PreDec,
                _ => unreachable!(),
            };
            let operand = self.parse_unary()?;
            Ok(Ast::Unary {
                operator,
                operand: Box::new(operand),
            })
        } else {
            self.parse_postfix()
        }
    }

    fn parse_postfix(&mut self) -> Result<Ast, ParserError> {
        let mut expr = self.parse_primary()?;

        loop {
            if self.next_token_of_type(&[token::Type::LParen]) {
                self.consume_token(); // consume (
                let mut args = Vec::new();
                if !self.next_token_of_type(&[token::Type::RParen]) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.next_token_of_type(&[token::Type::Comma]) {
                            break;
                        }
                        self.consume_token(); // consume ,
                    }
                }
                self.assert_next_token(&[token::Type::RParen])?; // consume )
                expr = Ast::Call {
                    callee: Box::new(expr),
                    args,
                };
            } else if self.next_token_of_type(&[token::Type::Dot]) {
                self.consume_token(); // consume .
                let (_, token, _) = self.lexer.next_token();
                if let token::Type::Identifier(name) = token {
                    expr = Ast::Member {
                        object: Box::new(expr),
                        property: name,
                    };
                } else {
                    return Err(ParserError::ParseError(
                        "Expected identifier after .".to_string(),
                    ));
                }
            } else if self.next_token_of_type(&[token::Type::LSqBracket]) {
                self.consume_token(); // consume [
                let index = self.parse_expression()?;
                self.assert_next_token(&[token::Type::RSqBracket])?; // consume ]
                expr = Ast::Index {
                    object: Box::new(expr),
                    index: Box::new(index),
                };
            } else if self.next_token_of_type(&[token::Type::Increment]) {
                self.consume_token();
                expr = Ast::Unary {
                    operator: UnaryOperator::PostInc,
                    operand: Box::new(expr),
                };
            } else if self.next_token_of_type(&[token::Type::Decrement]) {
                self.consume_token();
                expr = Ast::Unary {
                    operator: UnaryOperator::PostDec,
                    operand: Box::new(expr),
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_primary(&mut self) -> Result<Ast, ParserError> {
        // Skip any newlines before parsing primary expression
        while self.next_token_of_type(&[token::Type::Newline]) {
            self.consume_token();
        }
        
        let token = self.next_token_type();
        match token {
            token::Type::LParen => {
                let expr = self.parse_expression()?;
                self.assert_next_token(&[token::Type::RParen])?;
                Ok(expr)
            }
            token::Type::Me => Ok(Ast::Me),
            token::Type::Self_ => Ok(Ast::Self_),
            token::Type::New => {
                let class_name = self.identifier_name()?;
                self.assert_next_token(&[token::Type::LParen])?;
                let mut args = Vec::new();
                if !self.next_token_of_type(&[token::Type::RParen]) {
                    loop {
                        args.push(self.parse_expression()?);
                        if !self.next_token_of_type(&[token::Type::Comma]) {
                            break;
                        }
                        self.consume_token(); // consume ,
                    }
                }
                self.assert_next_token(&[token::Type::RParen])?;
                Ok(Ast::New {
                    class: class_name,
                    args,
                })
            }
            token::Type::LSqBracket => {
                let mut elements = Vec::new();
                
                // Skip newlines after opening bracket
                while self.next_token_of_type(&[token::Type::Newline]) {
                    self.consume_token();
                }
                
                if !self.next_token_of_type(&[token::Type::RSqBracket]) {
                    loop {
                        elements.push(self.parse_expression()?);
                        
                        // Skip newlines after element
                        while self.next_token_of_type(&[token::Type::Newline]) {
                            self.consume_token();
                        }
                        
                        if !self.next_token_of_type(&[token::Type::Comma]) {
                            break;
                        }
                        self.consume_token(); // consume ,
                        
                        // Skip newlines after comma
                        while self.next_token_of_type(&[token::Type::Newline]) {
                            self.consume_token();
                        }
                        
                        // Check for trailing comma (closing bracket after comma and newlines)
                        if self.next_token_of_type(&[token::Type::RSqBracket]) {
                            break;
                        }
                    }
                }
                
                // Skip newlines before closing bracket
                while self.next_token_of_type(&[token::Type::Newline]) {
                    self.consume_token();
                }
                
                self.assert_next_token(&[token::Type::RSqBracket])?;
                Ok(Ast::Array(elements))
            }
            token::Type::LBracket => {
                let mut pairs = Vec::new();
                if !self.next_token_of_type(&[token::Type::RBracket]) {
                    loop {
                        let key = self.parse_expression()?;
                        self.assert_next_token(&[token::Type::Colon])?;
                        let value = self.parse_expression()?;
                        pairs.push((key, value));
                        if !self.next_token_of_type(&[token::Type::Comma]) {
                            break;
                        }
                        self.consume_token(); // consume ,
                    }
                }
                self.assert_next_token(&[token::Type::RBracket])?;
                Ok(Ast::Dictionary(pairs))
            }
            token::Type::Identifier(name) => Ok(Ast::Identifier(name)),
            token::Type::Long(v) => Ok(Ast::BasicLit(LiteralValue::Long(v))),
            token::Type::Double(v) => Ok(Ast::BasicLit(LiteralValue::Double(v))),
            token::Type::String(v) => Ok(Ast::BasicLit(LiteralValue::String(v))),
            token::Type::Boolean(v) => Ok(Ast::BasicLit(LiteralValue::Boolean(v))),
            token::Type::Null => Ok(Ast::BasicLit(LiteralValue::Null)),
            token::Type::NaN => Ok(Ast::BasicLit(LiteralValue::NaN)),
            _ => Err(ParserError::ParseError(format!(
                "Unexpected token in primary expression: {:?}",
                token
            ))),
        }
    }
}
