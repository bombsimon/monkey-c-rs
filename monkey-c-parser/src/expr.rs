#![allow(dead_code)]
use crate::{lexer::Lexer, *};

#[derive(Debug)]
enum Expr {
    Num(i64),
    Add(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
}

impl Expr {
    fn eval(&self) -> i64 {
        match self {
            Expr::Num(n) => *n,
            Expr::Add(a, b) => a.eval() + b.eval(),
            Expr::Mul(a, b) => a.eval() * b.eval(),
        }
    }
}

fn parse_expression(lex: &mut Lexer) -> Result<Expr, &'static str> {
    parse_addition(lex)
}

fn parse_addition(lex: &mut Lexer) -> Result<Expr, &'static str> {
    let mut init = parse_multiplication(lex)?;

    while let (_, token::Type::Plus, _) = lex.peek_token() {
        lex.next_token();

        let next = parse_multiplication(lex)?;
        init = Expr::Add(Box::new(init), Box::new(next));
    }

    Ok(init)
}

fn parse_multiplication(lex: &mut Lexer) -> Result<Expr, &'static str> {
    let mut init = parse_atom(lex)?;

    while let (_, token::Type::Multiply, _) = lex.peek_token() {
        lex.next_token();

        let next = parse_atom(lex)?;
        init = Expr::Mul(Box::new(init), Box::new(next));
    }

    Ok(init)
}

fn parse_atom(lex: &mut Lexer) -> Result<Expr, &'static str> {
    match lex.next_token() {
        (_, token::Type::Long(n), _) => Ok(Expr::Num(n)),
        (_, token::Type::LParen, _) => {
            let expr = parse_expression(lex)?;
            if let (_, token::Type::RParen, _) = lex.next_token() {
                Ok(expr)
            } else {
                Err("Expected closing parenthesis")
            }
        }
        (_, tkn, _) => panic!("Unexpected token: {tkn:?}"),
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn parse_expression() {
        let input = "3 + (2 + 2) * 3";
        let mut lex = crate::lexer::Lexer::new(input);

        let expr = super::parse_expression(&mut lex).unwrap();
        println!("{expr:?}");
        assert_eq!(expr.eval(), 15);
    }
}
