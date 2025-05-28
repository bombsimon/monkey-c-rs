#![allow(dead_code)]
use crate::ast::{self, Ast, Ident, Type, Variable, Visibility};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    TokenizerError(String),
    ParseError(String),
}

pub struct Parser<'a> {
    lexer: crate::lexer::Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: crate::lexer::Lexer::new(source),
        }
    }

    fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut ast = Vec::new();

        loop {
            let statement = self.parse_top_level()?;
            if statement == Ast::EOF {
                break;
            }

            ast.push(statement);
        }

        Ok(Ast::Document(ast))
    }

    fn next_token_type(&mut self) -> token::Type {
        let (_, t, _) = self.lexer.next_token();
        t
    }

    fn next_token_of_type(&mut self, expect: &[token::Type]) -> bool {
        let (_, tkn, _) = self.lexer.peek_token();
        for expected in expect {
            if tkn == *expected {
                return true;
            }
        }

        false
    }

    fn assert_next_token(&mut self, expect: &[token::Type]) -> Result<token::Type, ParserError> {
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

    fn consume_token(&mut self) {
        self.lexer.next_token();
    }

    fn identifier_name(&mut self) -> Result<Ident, ParserError> {
        match self.next_token_type() {
            token::Type::Identifier(name) => Ok(name),
            tkn => Err(ParserError::TokenizerError(format!(
                "got unexpected token: '{tkn:?}'"
            ))),
        }
    }

    fn parse_top_level(&mut self) -> Result<Ast, ParserError> {
        match self.next_token_type() {
            token::Type::Newline => self.parse_top_level(),
            token::Type::Import => self.parse_import(),
            token::Type::Class => self.parse_class(),
            token::Type::Function => self.parse_function(),
            token::Type::Long(v) => Ok(Ast::BasicLit(v.to_string())),
            token::Type::Double(v) => Ok(Ast::BasicLit(v.to_string())),
            token::Type::Char(v) => Ok(Ast::BasicLit(v.to_string())),
            token::Type::String(v) => Ok(Ast::BasicLit(v.to_string())),
            t @ token::Type::Private
            | t @ token::Type::Protected
            | t @ token::Type::Public
            | t @ token::Type::Var => self.parse_variable_binding(t),
            t => {
                dbg!(t);
                Ok(Ast::EOF)
            }
        }
    }

    fn parse_import(&mut self) -> Result<Ast, ParserError> {
        let mut name = String::new();
        let mut alias = None;
        let mut is_alias = false;

        loop {
            let (_, tkn, _) = self.lexer.next_token();
            match tkn {
                token::Type::Identifier(i) => {
                    if is_alias {
                        alias = Some(i);
                    } else {
                        name.push_str(&i);
                    }
                }
                token::Type::Dot => name.push('.'),
                token::Type::As => is_alias = true,
                token::Type::SemiColon => break,
                t => {
                    return Err(ParserError::TokenizerError(format!(
                        "unexpected token '{t:?}'"
                    )))
                }
            }
        }

        Ok(Ast::Import { name, alias })
    }

    fn parse_class(&mut self) -> Result<Ast, ParserError> {
        let name = self.identifier_name()?;

        while !self.next_token_of_type(&[token::Type::LBracket]) {
            self.lexer.next_token();
        }

        self.lexer.next_token(); // Consume RBracket

        let mut body = vec![];
        loop {
            let statement = self.parse_top_level()?;
            if statement == Ast::EOF {
                break;
            }

            body.push(statement);
        }

        Ok(Ast::Class { name, body })
    }

    fn parse_function(&mut self) -> Result<Ast, ParserError> {
        let name = self.identifier_name()?;
        self.assert_next_token(&[token::Type::LParen])?;

        let mut args = vec![];

        while !self.next_token_of_type(&[token::Type::RParen]) {
            let variable = self.parse_variable()?;
            args.push(variable);

            if self.next_token_of_type(&[token::Type::Comma]) {
                self.lexer.next_token(); // Consume Comma
            }
        }

        self.lexer.next_token(); // Consume LParen

        let returns = if self.next_token_of_type(&[token::Type::As]) {
            self.lexer.next_token(); // Consume As
            Some(self.parse_type()?)
        } else {
            None
        };

        self.lexer.next_token(); // Consume LBrace

        let mut body = vec![];
        loop {
            let statement = self.parse_top_level()?;
            if statement == Ast::EOF {
                break;
            }

            body.push(statement);
        }

        Ok(Ast::Function {
            name,
            args,
            returns,
            body,
        })
    }

    fn parse_variable(&mut self) -> Result<Variable, ParserError> {
        let name = match self.next_token_type() {
            token::Type::Identifier(name) => name,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token: '{tkn:?}'"
                )))
            }
        };

        let type_ = match self.lexer.peek_token() {
            (_, token::Type::As, _) => {
                self.lexer.next_token();
                Some(self.parse_type()?)
            }
            _ => None,
        };

        Ok(Variable {
            name,
            type_,
            visibility: None,
        })
    }

    fn parse_type(&mut self) -> Result<Type, ParserError> {
        let ident = match self.next_token_type() {
            token::Type::Identifier(type_) => type_,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token: '{tkn:?}'"
                )))
            }
        };

        let optional = self.next_token_of_type(&[token::Type::QuestionMark]);
        if optional {
            self.lexer.next_token();
        }

        Ok(Type { ident, optional })
    }

    fn parse_variable_binding(
        &mut self,
        visibility_token: token::Type,
    ) -> Result<Ast, ParserError> {
        let visibility = match visibility_token {
            token::Type::Private => Some(ast::Visibility::Private),
            token::Type::Protected => Some(ast::Visibility::Protected),
            token::Type::Public => Some(ast::Visibility::Public),
            _ => None,
        };

        if visibility.is_some() {
            self.lexer.next_token(); // Consume `var`
        }

        let mut variable = self.parse_variable()?;
        variable.visibility = visibility;

        if self.next_token_of_type(&[token::Type::Assign]) {
            self.consume_token();

            Ok(Ast::Assign {
                target: Box::new(Ast::Variable(variable)),
                value: Box::new(self.parse_top_level()?),
            })
        } else {
            self.assert_next_token(&[token::Type::SemiColon])?;

            Ok(Ast::Variable(variable))
        }
    }
}

#[cfg(test)]
mod test {
    #[test]
    fn test_parse_to_ast() {
        let input = r#"
            import Toybox.Graphics;
            import Toybox.WatchUi as Ui;

            class PaceCalculatorApp extends Application.AppBase {
                private var _speedConverter as SpeedConverter;

                function onStart(state as Dictionary?) as Void {
                    private var x = 3.14;
                }
            }
        "#;
        let mut parser = super::Parser::new(input);
        let ast = parser.parse();

        println!("{ast:#?}");
    }
}
