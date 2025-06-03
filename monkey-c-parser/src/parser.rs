#![allow(dead_code)]
use crate::ast::{self, AssignOperator, Ast, Ident, LiteralValue, Type, Variable};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    TokenizerError(String),
    ParseError(String),
}

pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: crate::lexer::Lexer::new(source),
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut ast = Vec::new();

        loop {
            let statement = self.parse_top_level()?;
            if statement == Ast::Eof {
                break;
            }

            ast.push(statement);
        }

        Ok(Ast::Document(ast))
    }

    pub(crate) fn next_token_type(&mut self) -> token::Type {
        let (_, t, _) = self.lexer.next_token();
        t
    }

    pub(crate) fn next_token_of_type(&mut self, expect: &[token::Type]) -> bool {
        let (_, tkn, _) = self.lexer.peek_token();
        for expected in expect {
            if tkn == *expected {
                return true;
            }
        }

        false
    }

    pub(crate) fn assert_next_token(
        &mut self,
        expect: &[token::Type],
    ) -> Result<token::Type, ParserError> {
        let (pos, tkn, literal) = self.lexer.next_token();
        for expected in expect {
            if tkn == *expected {
                return Ok(tkn);
            }
        }

        Err(ParserError::TokenizerError(format!(
            "got unexpected token from `assert_next_token`: '{tkn:?}' (literal: '{literal}') at position {pos:?}, expected one of: {expect:?}",
        )))
    }

    pub(crate) fn consume_token(&mut self) {
        self.lexer.next_token();
    }

    pub(crate) fn identifier_name(&mut self) -> Result<Ident, ParserError> {
        let mut name = match self.next_token_type() {
            token::Type::Identifier(name) => name,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token from `identifier_name`: '{tkn:?}'"
                )))
            }
        };

        while self.next_token_of_type(&[token::Type::Dot]) {
            self.consume_token(); // consume dot
            match self.next_token_type() {
                token::Type::Identifier(part) => {
                    name.push('.');
                    name.push_str(&part);
                }
                tkn => {
                    return Err(ParserError::TokenizerError(format!(
                        "got unexpected token after dot: '{tkn:?}'"
                    )))
                }
            }
        }

        Ok(name)
    }

    fn parse_top_level(&mut self) -> Result<Ast, ParserError> {
        match self.next_token_type() {
            token::Type::Newline => self.parse_top_level(),
            token::Type::Comment(content) => Ok(Ast::Comment(content)),
            token::Type::Annotation(content) => Ok(Ast::Annotation(content)),
            token::Type::Import => self.parse_import(),
            token::Type::Class => self.parse_class(),
            token::Type::Function => self.parse_function(),
            token::Type::Long(v) => Ok(Ast::BasicLit(LiteralValue::Long(v))),
            token::Type::Double(v) => Ok(Ast::BasicLit(LiteralValue::Double(v))),
            token::Type::String(v) => Ok(Ast::BasicLit(LiteralValue::String(v))),
            token::Type::Boolean(v) => Ok(Ast::BasicLit(LiteralValue::Boolean(v))),
            t @ token::Type::Private
            | t @ token::Type::Protected
            | t @ token::Type::Public
            | t @ token::Type::Var => self.parse_variable_binding(t),
            token::Type::Eof => Ok(Ast::Eof),
            token::Type::Identifier(_) => {
                // This could be an assignment or expression statement
                self.parse_expression_statement()
            }
            t => Err(ParserError::ParseError(format!(
                "Unexpected token at top level: {:?}",
                t
            ))),
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

        // Parse extends clause if present
        let extends = if self.next_token_of_type(&[token::Type::Extends]) {
            self.consume_token();
            Some(self.identifier_name()?)
        } else {
            None
        };

        self.assert_next_token(&[token::Type::LBracket])?;

        let mut body = vec![];
        while !self.next_token_of_type(&[token::Type::RBracket]) {
            if self.next_token_of_type(&[token::Type::Eof]) {
                return Err(ParserError::ParseError(
                    "Unexpected end of file".to_string(),
                ));
            }

            match self.lexer.peek_token() {
                (_, token::Type::Function, _) => {
                    self.consume_token(); // Consume the 'function' token we peeked
                    body.push(self.parse_function()?)
                }
                (
                    _,
                    t @ (token::Type::Private | token::Type::Protected | token::Type::Public),
                    _,
                ) => {
                    self.consume_token(); // Consume the visibility modifier
                                          // Look ahead to see if this is a function or variable
                    match self.lexer.peek_token() {
                        (_, token::Type::Function, _) => {
                            self.consume_token(); // Consume 'function'
                            let func = self.parse_function()?;
                            // Add visibility to the function annotations for now
                            // TODO: Properly handle function visibility
                            body.push(func);
                        }
                        (_, token::Type::Var, _) => {
                            body.push(self.parse_variable_binding(t)?);
                        }
                        (_, unexpected, _) => {
                            return Err(ParserError::ParseError(format!(
                                "Expected 'function' or 'var' after visibility modifier, found: {:?}",
                                unexpected
                            )));
                        }
                    }
                }
                (_, token::Type::Var, _) => {
                    self.consume_token(); // Consume the 'var' token we peeked
                    body.push(self.parse_variable_binding(token::Type::Var)?);
                }
                (_, token::Type::Newline, _) => {
                    self.consume_token();
                    continue;
                }
                (_, token::Type::Comment(_), _) => {
                    body.push(self.parse_top_level()?);
                }
                (_, token::Type::Annotation(_), _) => {
                    body.push(self.parse_top_level()?);
                }
                (_, t, _) => {
                    return Err(ParserError::ParseError(format!(
                        "Unexpected token in class body: {:?}",
                        t
                    )))
                }
            }
        }

        self.assert_next_token(&[token::Type::RBracket])?;
        Ok(Ast::Class {
            name,
            extends,
            annotations: vec![], // TODO: Parse annotations before class
            body,
        })
    }

    fn parse_function(&mut self) -> Result<Ast, ParserError> {
        let name = self.identifier_name()?;
        self.assert_next_token(&[token::Type::LParen])?;

        let mut args = vec![];
        while !self.next_token_of_type(&[token::Type::RParen]) {
            // Skip newlines
            while self.next_token_of_type(&[token::Type::Newline]) {
                self.consume_token();
            }

            // Check if we hit the closing paren after skipping newlines
            if self.next_token_of_type(&[token::Type::RParen]) {
                break;
            }

            if !args.is_empty() {
                self.assert_next_token(&[token::Type::Comma])?;
                // Skip newlines after comma
                while self.next_token_of_type(&[token::Type::Newline]) {
                    self.consume_token();
                }
            }
            let variable = self.parse_variable()?;
            args.push(variable);
        }

        // Skip any newlines before the closing parenthesis
        while self.next_token_of_type(&[token::Type::Newline]) {
            self.consume_token();
        }

        self.assert_next_token(&[token::Type::RParen])?;

        let returns = if self.next_token_of_type(&[token::Type::As]) {
            self.consume_token();
            Some(self.parse_type()?)
        } else {
            None
        };

        // Parse function body
        self.assert_next_token(&[token::Type::LBracket])?;

        let mut body = Vec::new();
        while !self.next_token_of_type(&[token::Type::RBracket]) {
            if self.next_token_of_type(&[token::Type::Eof]) {
                return Err(ParserError::ParseError(
                    "Unexpected end of file".to_string(),
                ));
            }

            // Skip newlines
            while self.next_token_of_type(&[token::Type::Newline]) {
                self.consume_token();
            }

            // If we hit the closing bracket after skipping newlines, we're done
            if self.next_token_of_type(&[token::Type::RBracket]) {
                break;
            }

            let statement = match self.lexer.peek_token() {
                (_, token::Type::Private | token::Type::Protected | token::Type::Public, _) => {
                    return Err(ParserError::TokenizerError(
                        "Visibility modifiers not allowed for local variables".to_string(),
                    ));
                }
                (_, token::Type::Var, _) => {
                    self.consume_token();
                    self.parse_variable_binding(token::Type::Var)?
                }
                _ => self.parse_statement()?,
            };
            body.push(statement);
        }

        self.assert_next_token(&[token::Type::RBracket])?;

        Ok(Ast::Function {
            name,
            args,
            returns,
            annotations: vec![], // TODO: Parse annotations before function
            body,
        })
    }

    fn parse_variable(&mut self) -> Result<Variable, ParserError> {
        let name = self.identifier_name()?;

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

    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        let ident = match self.next_token_type() {
            token::Type::Identifier(type_) => type_,
            tkn => {
                return Err(ParserError::TokenizerError(format!(
                    "got unexpected token from `parse_type`: '{tkn:?}'"
                )))
            }
        };

        // Parse generic parameters if present
        let generic_params = if self.next_token_of_type(&[token::Type::Less]) {
            self.consume_token(); // consume <
            let mut params = Vec::new();

            // Parse first parameter
            if !self.next_token_of_type(&[token::Type::Greater]) {
                params.push(self.parse_type()?);

                // Parse additional parameters
                while self.next_token_of_type(&[token::Type::Comma]) {
                    self.consume_token(); // consume ,
                    params.push(self.parse_type()?);
                }
            }

            self.assert_next_token(&[token::Type::Greater])?; // consume >
            params
        } else {
            Vec::new()
        };

        let optional = self.next_token_of_type(&[token::Type::QuestionMark]);
        if optional {
            self.lexer.next_token();
        }

        Ok(Type {
            ident,
            generic_params,
            optional,
        })
    }

    fn parse_variable_binding(
        &mut self,
        visibility_token: token::Type,
    ) -> Result<Ast, ParserError> {
        let visibility = match visibility_token {
            token::Type::Private => Some(ast::Visibility::Private),
            token::Type::Protected => Some(ast::Visibility::Protected),
            token::Type::Public => Some(ast::Visibility::Public),
            token::Type::Var => None,
            _ => {
                return Err(ParserError::TokenizerError(format!(
                    "Expected visibility modifier or var, got {:?}",
                    visibility_token
                )))
            }
        };

        // If we got a visibility modifier, consume 'var'
        if visibility.is_some() {
            if !self.next_token_of_type(&[token::Type::Var]) {
                return Err(ParserError::TokenizerError(
                    "Expected 'var' after visibility modifier".to_string(),
                ));
            }
            self.consume_token(); // Consume 'var'
        }

        // Now parse the variable declaration
        let mut variable = self.parse_variable()?;
        variable.visibility = visibility;

        let mut ast = Ast::Variable(variable);

        if self.next_token_of_type(&[token::Type::Assign]) {
            self.consume_token();

            ast = Ast::Assign {
                target: Box::new(ast),
                operator: AssignOperator::Assign,
                value: Box::new(self.parse_expression()?),
            };
        }

        self.assert_next_token(&[token::Type::SemiColon])?;
        Ok(ast)
    }

    fn parse_statement(&mut self) -> Result<Ast, ParserError> {
        match self.lexer.peek_token() {
            (_, token::Type::Comment(content), _) => {
                self.consume_token(); // consume the comment token
                Ok(Ast::Comment(content))
            }
            (_, token::Type::Annotation(content), _) => {
                self.consume_token(); // consume the annotation token
                Ok(Ast::Annotation(content))
            }
            (_, token::Type::If, _) => self.parse_if_statement(),
            (_, token::Type::While, _) => self.parse_while_statement(),
            (_, token::Type::For, _) => self.parse_for_statement(),
            (_, token::Type::Return, _) => self.parse_return_statement(),
            (_, token::Type::Break, _) => {
                self.consume_token();
                self.assert_next_token(&[token::Type::SemiColon])?;
                Ok(Ast::Break)
            }
            (_, token::Type::Continue, _) => {
                self.consume_token();
                self.assert_next_token(&[token::Type::SemiColon])?;
                Ok(Ast::Continue)
            }
            _ => self.parse_expression_statement(),
        }
    }

    fn parse_if_statement(&mut self) -> Result<Ast, ParserError> {
        self.consume_token(); // consume if
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;

        let then_branch = self.parse_block()?;

        let else_branch = if self.next_token_of_type(&[token::Type::Else]) {
            self.consume_token();
            Some(Box::new(if self.next_token_of_type(&[token::Type::If]) {
                self.parse_if_statement()?
            } else {
                self.parse_block()?
            }))
        } else {
            None
        };

        Ok(Ast::If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch,
        })
    }

    fn parse_while_statement(&mut self) -> Result<Ast, ParserError> {
        self.consume_token(); // consume while
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;

        let body = self.parse_block()?;

        Ok(Ast::While {
            condition: Box::new(condition),
            body: Box::new(body),
        })
    }

    fn parse_for_statement(&mut self) -> Result<Ast, ParserError> {
        self.consume_token(); // consume for
        self.assert_next_token(&[token::Type::LParen])?;

        // Initializer
        let init = if !self.next_token_of_type(&[token::Type::SemiColon]) {
            let init = self.parse_expression()?;
            Some(Box::new(init))
        } else {
            None
        };
        self.assert_next_token(&[token::Type::SemiColon])?;

        // Condition
        let condition = if !self.next_token_of_type(&[token::Type::SemiColon]) {
            let cond = self.parse_expression()?;
            Some(Box::new(cond))
        } else {
            None
        };
        self.assert_next_token(&[token::Type::SemiColon])?;

        // Increment
        let update = if !self.next_token_of_type(&[token::Type::RParen]) {
            let update = self.parse_expression()?;
            Some(Box::new(update))
        } else {
            None
        };
        self.assert_next_token(&[token::Type::RParen])?;

        let body = self.parse_block()?;

        Ok(Ast::For {
            init,
            condition,
            update,
            body: Box::new(body),
        })
    }

    fn parse_return_statement(&mut self) -> Result<Ast, ParserError> {
        self.consume_token(); // consume return

        let value = if !self.next_token_of_type(&[token::Type::SemiColon]) {
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        self.assert_next_token(&[token::Type::SemiColon])?;

        Ok(Ast::Return(value))
    }

    fn parse_expression_statement(&mut self) -> Result<Ast, ParserError> {
        let expr = self.parse_expression()?;
        self.assert_next_token(&[token::Type::SemiColon])?;
        Ok(expr)
    }

    fn parse_block(&mut self) -> Result<Ast, ParserError> {
        self.assert_next_token(&[token::Type::LBracket])?;
        let mut statements = Vec::new();

        while !self.next_token_of_type(&[token::Type::RBracket]) {
            if self.next_token_of_type(&[token::Type::Eof]) {
                return Err(ParserError::ParseError(
                    "Unexpected end of file".to_string(),
                ));
            }

            // Skip newlines
            while self.next_token_of_type(&[token::Type::Newline]) {
                self.consume_token();
            }

            // If we hit the closing bracket after skipping newlines, we're done
            if self.next_token_of_type(&[token::Type::RBracket]) {
                break;
            }

            let statement = match self.lexer.peek_token() {
                (_, token::Type::Private | token::Type::Protected | token::Type::Public, _) => {
                    return Err(ParserError::TokenizerError(
                        "Visibility modifiers not allowed for local variables".to_string(),
                    ));
                }
                (_, token::Type::Var, _) => {
                    self.consume_token(); // consume var
                    self.parse_variable_binding(token::Type::Var)?
                }
                _ => self.parse_statement()?,
            };
            statements.push(statement);
        }

        self.assert_next_token(&[token::Type::RBracket])?;
        Ok(Ast::Block(statements))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::ast::Visibility;

    #[test]
    fn test_parse_to_ast() {
        let input = r#"
            import Toybox.Graphics;
            import Toybox.WatchUi as Ui;

            class PaceCalculatorApp extends Application.AppBase {
                private var _speedConverter as SpeedConverter;

                function onStart(state as Dictionary?) as Void {
                    var x = 3.14;
                }
            }
        "#;
        let mut parser = Parser::new(input);
        let ast = match parser.parse() {
            Ok(ast) => ast,
            Err(e) => {
                println!("Before error:");
                println!("Current token: {:?}", parser.next_token_type());
                println!("Next token: {:?}", parser.lexer.peek_token());
                panic!("Failed to parse: {e:?}");
            }
        };

        // Validate the AST matches what we expect
        if let Ast::Document(nodes) = ast {
            assert_eq!(nodes.len(), 3); // Two imports and one class

            // Check imports
            if let Ast::Import { name, alias } = &nodes[0] {
                assert_eq!(name, "Toybox.Graphics");
                assert_eq!(alias, &None);
            } else {
                panic!("Expected first node to be an import");
            }

            if let Ast::Import { name, alias } = &nodes[1] {
                assert_eq!(name, "Toybox.WatchUi");
                assert_eq!(alias, &Some("Ui".to_string()));
            } else {
                panic!("Expected second node to be an import");
            }

            // Check class definition
            if let Ast::Class {
                name,
                extends,
                annotations: _,
                body,
            } = &nodes[2]
            {
                assert_eq!(name, "PaceCalculatorApp");
                assert_eq!(extends, &Some("Application.AppBase".to_string()));

                // Validate class body
                assert_eq!(body.len(), 2); // One field and one method

                // Check the field
                if let Ast::Variable(var) = &body[0] {
                    assert_eq!(var.name, "_speedConverter");
                    assert_eq!(var.visibility, Some(Visibility::Private));
                    assert_eq!(
                        var.type_.as_ref().map(|t| &t.ident),
                        Some(&"SpeedConverter".to_string())
                    );
                } else {
                    panic!("Expected first class member to be a field");
                }

                // Check the function
                if let Ast::Function {
                    name,
                    args,
                    returns,
                    annotations: _,
                    body: _,
                } = &body[1]
                {
                    assert_eq!(name, "onStart");
                    assert_eq!(args.len(), 1);
                    assert_eq!(args[0].name, "state");
                    assert_eq!(
                        args[0].type_.as_ref().map(|t| &t.ident),
                        Some(&"Dictionary".to_string())
                    );
                    assert!(args[0].type_.as_ref().unwrap().optional);
                    assert_eq!(
                        returns.as_ref().map(|t| &t.ident),
                        Some(&"Void".to_string())
                    );
                } else {
                    panic!("Expected second class member to be a function");
                }
            } else {
                panic!("Expected third node to be a class");
            }
        } else {
            panic!("Expected root node to be a Document");
        }
    }
}
