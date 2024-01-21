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

#[derive(Debug)]
enum TokenType {
    BasicLiteral,
    Identifier,
    Add,
    Mul,
    LParen,
    RParen,
    EOF,
}

#[derive(Debug)]
struct Token {
    token_type: TokenType,
    literal: String,
    start_position: usize,
    end_position: usize,
}

impl Token {
    fn new(
        token_type: TokenType,
        literal: String,
        start_position: usize,
        end_position: usize,
    ) -> Self {
        Token {
            token_type,
            literal,
            start_position,
            end_position,
        }
    }
}

struct Tokenizer<'a> {
    input: &'a str,
    position: usize,
}

impl<'a> Tokenizer<'a> {
    fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer { input, position: 0 }
    }

    fn next_token(&mut self) -> Token {
        let start_position = self.position;

        self.skip_whitespace();

        if self.position >= self.input.len() {
            return Token::new(
                TokenType::EOF,
                "".to_string(),
                start_position,
                self.position,
            );
        }

        let current_char = self.input.chars().nth(self.position).unwrap();

        match current_char {
            c if current_char.is_ascii_alphabetic() => {
                let literal = self.parse_identifier();
                Token::new(
                    TokenType::Identifier,
                    literal,
                    start_position,
                    self.position,
                )
            }
            '0'..='9' => {
                let num = self.parse_number();
                Token::new(
                    TokenType::BasicLiteral,
                    num.to_string(),
                    start_position,
                    self.position,
                )
            }
            '+' => {
                self.position += 1;
                Token::new(
                    TokenType::Add,
                    "+".to_string(),
                    start_position,
                    self.position,
                )
            }
            '*' => {
                self.position += 1;
                Token::new(
                    TokenType::Mul,
                    "+".to_string(),
                    start_position,
                    self.position,
                )
            }
            '(' => {
                self.position += 1;
                Token::new(
                    TokenType::LParen,
                    "+".to_string(),
                    start_position,
                    self.position,
                )
            }
            ')' => {
                self.position += 1;
                Token::new(
                    TokenType::RParen,
                    "+".to_string(),
                    start_position,
                    self.position,
                )
            }
            _ => panic!("Unexpected character: {}", current_char),
        }
    }

    fn peek_token(&mut self) -> Token {
        let saved_position = self.position;
        let token = self.next_token();
        self.position = saved_position; // Restore the position

        token
    }

    fn parse_identifier(&mut self) -> String {
        let start = self.position;

        let mut s = String::new();
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_ascii_alphabetic() {
                self.position += 1;
            } else {
                break;
            }
        }

        s
    }

    fn parse_number(&mut self) -> i64 {
        let start = self.position;

        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_digit(10) {
                self.position += 1;
            } else {
                break;
            }
        }

        let num_str = &self.input[start..self.position];
        num_str.parse().unwrap()
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.input.chars().nth(self.position) {
            if c.is_whitespace() {
                self.position += 1;
            } else {
                break;
            }
        }
    }
}

fn parse_document(tokenizer: &mut Tokenizer) -> Result<Expr, &'static str> {
    parse_addition(tokenizer)
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
    fn test_tokenizer() {
        let input = "2 + 2 * 3 + 3 * 4";
        let mut tokenizer = crate::Tokenizer::new(input);
        let t = tokenizer.peek_token();

        println!("first token: {t:?}");

        let expr = crate::parse_expression(&mut tokenizer).unwrap();
        println!("{expr:?}");
        println!("{}", expr.eval());
    }
}
