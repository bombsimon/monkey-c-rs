#![allow(dead_code)]
use crate::token::Type;
type Ident = String;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    TokenizerError(String),
}

#[derive(Debug)]
pub struct T {
    type_: Ident,
    optional: bool,
}

#[derive(Debug)]
pub struct Variable {
    name: Ident,
    type_: Option<T>,
}

#[derive(Debug)]
pub enum Ast {
    Document(Vec<Ast>),
    Import {
        name: Ident,
        alias: Option<Ident>,
    },
    Function {
        name: Ident,
        args: Vec<Variable>,
        returns: Option<T>,
    },
}

struct Parser<'a> {
    lexer: crate::lexer::Lexer<'a>,
}

impl<'a> Parser<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            lexer: crate::lexer::Lexer::new(source),
        }
    }

    fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut ast = Vec::new();

        loop {
            let statement = match self.lexer.next_token() {
                (_, Type::Newline, _) => continue,
                (_, Type::Import, _) => self.parse_import()?,
                (_, Type::Function, _) => self.parse_function()?,
                _ => break,
            };

            ast.push(statement);
        }

        Ok(Ast::Document(ast))
    }

    fn next_token_type(&mut self) -> Type {
        let (_, t, _) = self.lexer.next_token();
        t
    }

    fn next_token_of_type(&mut self, expect: &[Type]) -> bool {
        let (_, tkn, _) = self.lexer.peek_token();
        for expected in expect {
            if tkn == *expected {
                return true;
            }
        }

        false
    }

    fn assert_next_token(&mut self, expect: &[Type]) -> Result<Type, ParserError> {
        let (_, tkn, _) = self.lexer.next_token();
        for expected in expect {
            if tkn == *expected {
                return Ok(tkn);
            }
        }

        Err(ParserError::TokenizerError(format!(
            "got unexpected token: '{tkn:?}'"
        )))
    }

    fn parse_import(&mut self) -> Result<Ast, ParserError> {
        let mut name = String::new();
        let mut alias = None;
        let mut is_alias = false;

        loop {
            let (_, tkn, _) = self.lexer.next_token();
            match tkn {
                Type::Identifier(i) => {
                    if is_alias {
                        alias = Some(i);
                    } else {
                        name.push_str(&i);
                    }
                }
                Type::Dot => name.push('.'),
                Type::As => is_alias = true,
                Type::SemiColon => break,
                t => {
                    return Err(ParserError::TokenizerError(format!(
                        "unexpected token '{t:?}'"
                    )))
                }
            }
        }

        Ok(Ast::Import { name, alias })
    }

    fn parse_function(&mut self) -> Result<Ast, ParserError> {
        let name = match self.next_token_type() {
            Type::Identifier(name) => name,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token: '{tkn:?}'"
                )))
            }
        };
        self.assert_next_token(&[Type::LParen])?;

        let mut args = vec![];

        while !self.next_token_of_type(&[Type::RParen]) {
            let variable = self.parse_variable()?;
            args.push(variable);

            if self.next_token_of_type(&[Type::Comma]) {
                self.lexer.next_token(); // Consume Comma
            }
        }

        self.lexer.next_token(); // Consume LParen

        let returns = if self.next_token_of_type(&[Type::As]) {
            self.lexer.next_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        Ok(Ast::Function {
            name,
            args,
            returns,
        })
    }

    fn parse_variable(&mut self) -> Result<Variable, ParserError> {
        let name = match self.next_token_type() {
            Type::Identifier(name) => name,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token: '{tkn:?}'"
                )))
            }
        };

        let type_ = match self.lexer.peek_token() {
            (_, Type::As, _) => {
                self.lexer.next_token();
                Some(self.parse_type()?)
            }
            _ => None,
        };

        Ok(Variable { name, type_ })
    }

    fn parse_type(&mut self) -> Result<T, ParserError> {
        let type_ = match self.next_token_type() {
            Type::Identifier(type_) => type_,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token: '{tkn:?}'"
                )))
            }
        };

        let optional = self.next_token_of_type(&[Type::QuestionMark]);
        if optional {
            self.lexer.next_token();
        }

        Ok(T { type_, optional })
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_parse_to_ast() {
        let input = r#"
            import Toybox.Graphics;
            import Toybox.WatchUi as Ui;

            function onStart(state as Dictionary?) as Void {}
        "#;
        let mut parser = super::Parser::new(input);
        let ast = parser.parse();

        println!("{ast:#?}");
    }
}
