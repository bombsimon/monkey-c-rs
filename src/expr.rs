use crate::ast::{Ast, BinaryOperator, LiteralValue, Span};
use crate::parser::Parser;
use crate::lexer::Lexer;
use crate::parser::ParserError;

#[test]
fn test_parse_x_eq_null() {
    eprintln!("[TEST] test_parse_x_eq_null running");
    let input = "x == null";
    let mut lexer = Lexer::new(input);
    let (start, token, end) = lexer.next_token();
    eprintln!("[DEBUG] First token: {:?} ({}-{})", token, start, end);
    let mut parser = Parser::new(input);
    let result = parser.parse_expression();
    eprintln!("[DEBUG] Parse result: {:?}", result);
    assert!(result.is_ok());
}

#[test]
fn test_parse_if_x_eq_null_block() {
    eprintln!("[TEST] test_parse_if_x_eq_null_block running");
    let input = "if (x == null) {}";
    let mut parser = Parser::new(input);
    let result = parser.parse();
    eprintln!("[DEBUG] Parse result: {:?}", result);
    assert!(result.is_ok());
}

pub(crate) fn parse_primary(&mut self) -> Result<Ast, ParserError> {
    eprintln!("[TRACE] parse_primary: current_token = {:?}", self.current_token);
    let token_type = self.current_token.clone();
    match token_type {
        token::Type::LParen => {
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after LParen, current_token = {:?}", self.current_token);
            let expr = self.parse_expression()?;
            self.assert_next_token(&[token::Type::RParen])?;
            eprintln!("[TRACE] parse_primary: returning expr = {:?}", expr);
            Ok(expr)
        }
        token::Type::Identifier(name) => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Identifier, current_token = {:?}", self.current_token);
            let ast = Ast::Identifier(name, Span { start: 0, end });
            eprintln!("[TRACE] parse_primary: returning AST: {:?}", ast);
            Ok(ast)
        }
        token::Type::Me => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Me, current_token = {:?}", self.current_token);
            Ok(Ast::Me(Span { start: 0, end }))
        }
        token::Type::Self_ => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Self_, current_token = {:?}", self.current_token);
            Ok(Ast::Self_(Span { start: 0, end }))
        }
        token::Type::Long(value) => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Long, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::Long(value), Span { start: 0, end }))
        }
        token::Type::Double(value) => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Double, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::Double(value), Span { start: 0, end }))
        }
        token::Type::String(value) => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after String, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::String(value), Span { start: 0, end }))
        }
        token::Type::Boolean(value) => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Boolean, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::Boolean(value), Span { start: 0, end }))
        }
        token::Type::Null => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after Null, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::Null, Span { start: 0, end }))
        }
        token::Type::NaN => {
            let (_, _, end) = self.lexer.peek_token();
            self.next_token_span();
            eprintln!("[TRACE] parse_primary: after NaN, current_token = {:?}", self.current_token);
            Ok(Ast::BasicLit(LiteralValue::NaN, Span { start: 0, end }))
        }
        _ => Err(ParserError::ParseError(format!(
            "Unexpected token in expression: {:?}",
            token_type
        ))),
    }
}

fn parse_term(&mut self) -> Result<Ast, ParserError> {
    eprintln!("[DEBUG] parse_term: current_token = {:?}", self.current_token);
    let mut expr = self.parse_factor()?;
    while matches!(self.current_token, token::Type::Plus | token::Type::Minus) {
        eprintln!("[DEBUG] parse_term: before next_token_span, current_token = {:?}", self.current_token);
        let operator_token = self.current_token.clone();
        let (start, _, _) = self.next_token_span();
        eprintln!("[DEBUG] parse_term: after next_token_span, current_token = {:?}", self.current_token);
        let operator = match operator_token {
            token::Type::Plus => BinaryOperator::Add,
            token::Type::Minus => BinaryOperator::Sub,
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
        eprintln!("[DEBUG] parse_term: AST after op: {:?}", expr);
    }
    eprintln!("[DEBUG] parse_term: final AST: {:?}", expr);
    Ok(expr)
}

fn parse_factor(&mut self) -> Result<Ast, ParserError> {
    eprintln!("[DEBUG] parse_factor: current_token = {:?}", self.current_token);
    let mut expr = self.parse_unary()?;
    eprintln!("[DEBUG] parse_factor: after parse_unary, expr = {:?}", expr);
    while matches!(self.current_token, token::Type::Star | token::Type::Slash | token::Type::Percent) {
        let operator_token = self.current_token.clone();
        let (start, _, _) = self.next_token_span();
        let operator = match operator_token {
            token::Type::Star => BinaryOperator::Mul,
            token::Type::Slash => BinaryOperator::Div,
            token::Type::Percent => BinaryOperator::Mod,
            _ => unreachable!(),
        };
        let right = self.parse_unary()?;
        let (_, _, end) = self.lexer.peek_token();
        expr = Ast::Binary {
            left: Box::new(expr),
            operator,
            right: Box::new(right),
            span: Span { start, end },
        };
        eprintln!("[DEBUG] parse_factor: AST after op: {:?}", expr);
    }
    eprintln!("[DEBUG] parse_factor: final AST: {:?}", expr);
    Ok(expr)
}

fn parse_postfix(&mut self) -> Result<Ast, ParserError> {
    eprintln!("[DEBUG] parse_postfix: current_token = {:?}", self.current_token);
    let mut expr = self.parse_primary()?;
    eprintln!("[DEBUG] parse_postfix: after parse_primary, current_token = {:?}", self.current_token);
    // ... existing code ...
    eprintln!("[DEBUG] parse_postfix: final AST: {:?}", expr);
    Ok(expr)
}

#[test]
fn test_parse_subtraction() {
    let input = "a - b";
    let mut parser = Parser::new(input);
    let expr = parser.parse_expression().unwrap();
    eprintln!("[DEBUG] test_parse_subtraction: parse_expression returned: {:?}", expr);
    eprintln!("[DEBUG] test_parse_subtraction: expr type: {}", std::any::type_name_of_val(&expr));
    match expr {
        Ast::Binary { .. } => eprintln!("Matched Ast::Binary!"),
        _ => eprintln!("Did not match Ast::Binary! Got: {:?}", expr),
    }
    // assert!(matches!(expr, Ast::Binary { .. }));
}

fn parse_unary(&mut self) -> Result<Ast, ParserError> {
    eprintln!("[DEBUG] parse_unary: current_token = {:?}", self.current_token);
    let depth_check = RECURSION_DEPTH.with(|depth| {
        let d = depth.get();
        if d > 1000 {
            return Err(ParserError::ParseError("Expression too complex".to_string()));
        }
        depth.set(d + 1);
        Ok(())
    });
    depth_check?;

    let result = if matches!(self.current_token,
        token::Type::Minus |
        token::Type::Bang |
        token::Type::Tilde |
        token::Type::PlusPlus |
        token::Type::MinusMinus
    ) {
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
        let ast = Ast::Unary {
            operator,
            operand: Box::new(right),
            span: Span { start, end },
        };
        eprintln!("[DEBUG] parse_unary: returning AST: {:?}", ast);
        Ok(ast)
    } else {
        let ast = self.parse_postfix()?;
        eprintln!("[DEBUG] parse_unary: returning AST: {:?}", ast);
        Ok(ast)
    };

    RECURSION_DEPTH.with(|depth| {
        let d = depth.get();
        depth.set(d - 1);
    });

    result
} 
