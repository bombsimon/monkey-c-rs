use crate::*;

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

fn parse_expression(tokenizer: &mut Tokenizer) -> Result<Expr, &'static str> {
    parse_addition(tokenizer)
}

fn parse_addition(tokenizer: &mut Tokenizer) -> Result<Expr, &'static str> {
    let mut init = parse_multiplication(tokenizer)?;

    while let Token {
        token_type: TokenType::Add,
        ..
    } = tokenizer.peek_token()
    {
        tokenizer.next_token(); // Consume '+'
        let next = parse_multiplication(tokenizer)?;
        init = Expr::Add(Box::new(init), Box::new(next));
    }

    Ok(init)
}

fn parse_multiplication(tokenizer: &mut Tokenizer) -> Result<Expr, &'static str> {
    let mut init = parse_atom(tokenizer)?;

    while let Token {
        token_type: TokenType::Mul,
        ..
    } = tokenizer.peek_token()
    {
        tokenizer.next_token(); // Consume '*'
        let next = parse_atom(tokenizer)?;
        init = Expr::Mul(Box::new(init), Box::new(next));
    }

    Ok(init)
}

fn parse_atom(tokenizer: &mut Tokenizer) -> Result<Expr, &'static str> {
    match tokenizer.next_token() {
        Token {
            token_type: TokenType::BasicLiteral,
            literal,
            ..
        } => Ok(Expr::Num(literal.parse().unwrap())),
        Token {
            token_type: TokenType::LParen,
            ..
        } => {
            let expr = parse_expression(tokenizer)?;
            if let Token {
                token_type: TokenType::RParen,
                ..
            } = tokenizer.next_token()
            {
                Ok(expr)
            } else {
                Err("Expected closing parenthesis")
            }
        }
        _ => Err("Unexpected token"),
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn parse_expression() {
        let input = "3 + (2 + 2) * 3";
        let mut tokenizer = crate::Tokenizer::new(input);

        let expr = super::parse_expression(&mut tokenizer).unwrap();
        assert_eq!(expr.eval(), 15);
    }
}
