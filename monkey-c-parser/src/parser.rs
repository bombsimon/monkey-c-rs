use crate::ast::{Ast, Span, Type, Variable, Visibility};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    TokenizerError(String),
    ParseError(String),
}

pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
    pub current_token: token::Type,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = crate::lexer::Lexer::new(source);
        let (_, token_type, _) = lexer.next_token();
        Self {
            lexer,
            current_token: token_type,
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut ast = Vec::new();

        loop {
            let statement = self.parse_declaration()?;
            if statement == Ast::Eof {
                break;
            }

            ast.push(statement);
        }

        Ok(Ast::Document(ast))
    }

    pub(crate) fn next_token_span(&mut self) -> (usize, token::Type, usize) {
        let (start_pos, token_type, end) = self.lexer.next_token();
        self.current_token = token_type.clone();
        (start_pos, token_type, end)
    }

    pub(crate) fn peek_token_span(&mut self) -> (usize, token::Type, usize) {
        self.lexer.peek_token()
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
        for expected in expect {
            if self.current_token == *expected {
                let tkn = self.current_token.clone();
                self.next_token_span(); // Advance to next token
                return Ok(tkn);
            }
        }
        Err(ParserError::TokenizerError(format!(
            "got unexpected token from `assert_next_token`: '{:?}', expected one of: {:?}",
            self.current_token, expect
        )))
    }

    pub(crate) fn parse_identifier(&mut self) -> Result<String, ParserError> {
        match &self.current_token {
            token::Type::Identifier(name) => {
                let name = name.clone();
                Ok(name)
            }
            _ => Err(ParserError::ParseError(format!("Expected identifier, got {:?}", self.current_token)))
        }
    }

    pub(crate) fn parse_type(&mut self) -> Result<Type, ParserError> {
        let ident = self.parse_identifier()?;
        if self.current_token != token::Type::Less {
            self.next_token_span();
        }
        let generic_params = if self.current_token == token::Type::Less {
            self.next_token_span(); // consume <
            let mut params = Vec::new();

            // Parse first parameter
            if self.current_token != token::Type::Greater {
                params.push(self.parse_type()?);
                // Parse additional parameters
                while self.current_token == token::Type::Comma {
                    self.next_token_span(); // consume ,
                    params.push(self.parse_type()?);
                }
            }

            self.assert_next_token(&[token::Type::Greater])?;
            params
        } else {
            Vec::new()
        };

        let optional = self.current_token == token::Type::Question;
        if optional {
            self.next_token_span(); // consume ?
        }

        Ok(Type {
            ident,
            generic_params,
            optional,
        })
    }

    fn parse_dotted_identifier(&mut self) -> Result<String, ParserError> {
        let mut name = self.parse_identifier()?;
        self.next_token_span(); // advance after first identifier
        while self.current_token == token::Type::Dot {
            self.next_token_span(); // consume dot
            let next = self.parse_identifier()?;
            name.push('.');
            name.push_str(&next);
            self.next_token_span(); // advance after each identifier
        }
        Ok(name)
    }

    fn parse_declaration(&mut self) -> Result<Ast, ParserError> {
        // Collect modifiers
        let mut visibility = None;
        let mut is_static = false;
        let mut is_hidden = false;
        loop {
            match self.current_token {
                token::Type::Private => {
                    visibility = Some(Visibility::Private);
                    self.next_token_span();
                }
                token::Type::Protected => {
                    visibility = Some(Visibility::Protected);
                    self.next_token_span();
                }
                token::Type::Public => {
                    visibility = Some(Visibility::Public);
                    self.next_token_span();
                }
                token::Type::Static => {
                    is_static = true;
                    self.next_token_span();
                }
                token::Type::Hidden => {
                    is_hidden = true;
                    self.next_token_span();
                }
                _ => break,
            }
        }

        let token_type = self.current_token.clone();
        match token_type {
            token::Type::Import => {
                self.next_token_span(); // advance past Import token
                let name = self.parse_dotted_identifier()?;
                let alias = if self.current_token == token::Type::As {
                    self.next_token_span();
                    Some(self.parse_identifier()?)
                } else {
                    None
                };
                self.assert_next_token(&[token::Type::Semicolon])?; // require and consume semicolon
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::Import {
                    name,
                    alias,
                    span: Span { start: 0, end },
                })
            }
            token::Type::Class => {
                self.next_token_span(); // advance past Class token
                let name = self.parse_identifier()?;
                self.next_token_span(); // advance past identifier
                let extends = if self.current_token == token::Type::Extends {
                    self.next_token_span(); // advance past extends
                    let name = self.parse_identifier()?;
                    self.next_token_span(); // advance past identifier
                    Some(name)
                } else {
                    None
                };
                self.assert_next_token(&[token::Type::LBrace])?; // consume opening brace
                let body = self.parse_class_body()?;
                self.assert_next_token(&[token::Type::RBrace])?; // consume closing brace
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::Class {
                    name,
                    extends,
                    annotations: Vec::new(),
                    body,
                    span: Span { start: 0, end },
                })
            }
            token::Type::Function => {
                self.next_token_span(); // advance past Function token
                let name = self.parse_identifier()?;
                self.next_token_span(); // advance past identifier
                let args = self.parse_function_args()?;
                let returns = if self.current_token == token::Type::As {
                    self.next_token_span();
                    let t = self.parse_type()?;
                    Some(t)
                } else {
                    None
                };
                let annotations = Vec::new(); // TODO: Attach annotations if needed
                self.assert_next_token(&[token::Type::LBrace])?; // consume LBrace
                let function_body = self.parse_block()?;
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::Function {
                    name,
                    args,
                    returns,
                    annotations,
                    body: function_body,
                    visibility,
                    is_static,
                    is_hidden,
                    span: Span { start: 0, end },
                })
            }
            token::Type::Var => {
                self.next_token_span(); // advance past Var token
                let name = self.parse_identifier()?;
                self.next_token_span(); // advance past identifier
                let type_ = if self.current_token == token::Type::As {
                    self.next_token_span();
                    let t = self.parse_type()?;
                    Some(t)
                } else {
                    None
                };
                let initializer = if self.current_token == token::Type::Assign {
                    self.next_token_span(); // consume =
                    let expr = self.parse_expression()?;
                    Some(Box::new(expr))
                } else {
                    None
                };
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Ast::Variable(Variable {
                    name,
                    type_,
                    visibility,
                    initializer,
                    is_static,
                    is_hidden,
                }, Span { start: 0, end: 0 }))
            }
            token::Type::Identifier(name) => {
                let (_, _, end) = self.peek_token_span();
                self.next_token_span(); // Advance the token to prevent infinite loop
                Ok(Ast::Identifier(name, Span { start: 0, end }))
            }
            token::Type::Comment(content) => {
                let content = content.clone();
                self.next_token_span();
                Ok(Ast::Comment(content, Span { start: 0, end: 0 }))
            }
            token::Type::Annotation(content) => {
                let content = content.clone();
                self.next_token_span();
                Ok(Ast::Annotation(content, Span { start: 0, end: 0 }))
            }
            token::Type::Eof => Ok(Ast::Eof),
            _ => Err(ParserError::ParseError(format!(
                "Unexpected token at top level: {:?}",
                self.current_token
            ))),
        }
    }

    fn parse_function_args(&mut self) -> Result<Vec<Variable>, ParserError> {
        self.assert_next_token(&[token::Type::LParen])?;
        let mut args = Vec::new();

        while self.current_token != token::Type::RParen {
            let name = self.parse_identifier()?;
            self.next_token_span(); // Advance past identifier

            let type_ = if self.current_token == token::Type::As {
                self.next_token_span(); // Advance past 'as'
                let t = self.parse_type()?;
                Some(t)
            } else {
                None
            };

            args.push(Variable { 
                name, 
                type_, 
                visibility: None,
                initializer: None,
                is_static: false,
                is_hidden: false,
            });

            if self.current_token == token::Type::Comma {
                self.next_token_span(); // consume comma
            } else if self.current_token != token::Type::RParen {
                return Err(ParserError::ParseError(format!(
                    "Expected ',' or ')' in function arguments, got {:?}",
                    self.current_token
                )));
            }
        }

        self.next_token_span(); // consume RParen
        Ok(args)
    }

    fn parse_class_body(&mut self) -> Result<Vec<Ast>, ParserError> {
        let mut body = Vec::new();
        while self.current_token != token::Type::RBrace {
            let member = self.parse_declaration()?;
            body.push(member);
        }
        Ok(body)
    }

    fn parse_block(&mut self) -> Result<Vec<Ast>, ParserError> {
        // The opening LBrace should already be consumed by the caller
        let mut statements = Vec::new();
        while self.current_token != token::Type::RBrace {
            let statement = self.parse_statement()?;
            statements.push(statement);
        }
        self.next_token_span(); // consume RBrace
        Ok(statements)
    }

    fn parse_statement(&mut self) -> Result<Ast, ParserError> {
        match &self.current_token {
            token::Type::Var => {
                self.next_token_span(); // advance past Var token
                let name = self.parse_identifier()?;
                self.next_token_span(); // advance past identifier
                let type_ = if self.current_token == token::Type::As {
                    self.next_token_span();
                    let t = self.parse_type()?;
                    Some(t)
                } else {
                    None
                };
                let initializer = if self.current_token == token::Type::Assign {
                    self.next_token_span(); // consume =
                    let expr = self.parse_expression()?;
                    Some(Box::new(expr))
                } else {
                    None
                };
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Ast::Variable(Variable {
                    name,
                    type_,
                    visibility: None,
                    initializer,
                    is_static: false,
                    is_hidden: false,
                }, Span { start: 0, end: 0 }))
            }
            token::Type::Return => {
                self.next_token_span(); // advance past Return token
                let value = if self.current_token == token::Type::Semicolon {
                    None
                } else {
                    let expr = self.parse_expression()?;
                    Some(Box::new(expr))
                };
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Ast::Return(value, Span { start: 0, end: 0 }))
            }
            token::Type::If => {
                self.next_token_span(); // advance past If token
                self.assert_next_token(&[token::Type::LParen])?; // consume opening parenthesis
                let condition = Box::new(self.parse_expression_no_postfix()?);
                self.assert_next_token(&[token::Type::RParen])?; // consume closing parenthesis
                self.assert_next_token(&[token::Type::LBrace])?; // consume opening brace
                let then_branch = Box::new(Ast::Block(self.parse_block()?, Span { start: 0, end: 0 }));
                let else_branch = if self.current_token == token::Type::Else {
                    self.next_token_span();
                    Some(Box::new(Ast::Block(self.parse_block()?, Span { start: 0, end: 0 })))
                } else {
                    None
                };
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::If {
                    condition,
                    then_branch,
                    else_branch,
                    span: Span { start: 0, end },
                })
            }
            token::Type::While => {
                self.next_token_span(); // advance past While token
                let condition = Box::new(self.parse_expression()?);
                let body = Box::new(Ast::Block(self.parse_block()?, Span { start: 0, end: 0 }));
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::While {
                    condition,
                    body,
                    span: Span { start: 0, end },
                })
            }
            token::Type::For => {
                self.next_token_span(); // advance past For token
                let init = if self.current_token == token::Type::Semicolon {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                self.assert_next_token(&[token::Type::Semicolon])?;
                let condition = if self.current_token == token::Type::Semicolon {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                self.assert_next_token(&[token::Type::Semicolon])?;
                let update = if self.current_token == token::Type::LBrace {
                    None
                } else {
                    Some(Box::new(self.parse_expression()?))
                };
                let body = Box::new(Ast::Block(self.parse_block()?, Span { start: 0, end: 0 }));
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::For {
                    init,
                    condition,
                    update,
                    body,
                    span: Span { start: 0, end },
                })
            }
            token::Type::Comment(content) => {
                let content = content.clone();
                self.next_token_span();
                Ok(Ast::Comment(content, Span { start: 0, end: 0 }))
            }
            _ => {
                if self.current_token == token::Type::LBrace {
                    let block = self.parse_block()?;
                    // Instead of wrapping in Ast::Block, return the first statement in the block, or Eof if empty
                    if block.is_empty() {
                        return Ok(Ast::Eof);
                    } else if block.len() == 1 {
                        return Ok(block.into_iter().next().unwrap());
                    } else {
                        // If multiple statements, wrap in a Block node
                        return Ok(Ast::Block(block, Span { start: 0, end: 0 }));
                    }
                }
                let expr = self.parse_expression();
                match expr {
                    Err(ParserError::ParseError(msg)) if msg == "Block parsing should not be handled in parse_primary" => {
                        let block = self.parse_block()?;
                        if block.is_empty() {
                            Ok(Ast::Eof)
                        } else if block.len() == 1 {
                            Ok(block.into_iter().next().unwrap())
                        } else {
                            Ok(Ast::Block(block, Span { start: 0, end: 0 }))
                        }
                    }
                    Ok(expr) => {
                        self.assert_next_token(&[token::Type::Semicolon])?;
                        Ok(expr)
                    }
                    Err(e) => Err(e)
                }
            }
        }
    }

    pub fn parse_class(&mut self) -> Result<Ast, ParserError> {
        let name = self.parse_identifier()?;
        let extends = if self.current_token == token::Type::Extends {
            self.next_token_span();
            Some(self.parse_identifier()?)
        } else {
            None
        };
        let annotations = Vec::new(); // TODO: Parse annotations
        let body = self.parse_class_body()?;
        let (_, _, end) = self.peek_token_span();
        Ok(Ast::Class {
            name,
            extends,
            annotations,
            body,
            span: Span { start: 0, end },
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::LiteralValue;
    

    #[test]
    fn test_parse_simple_class_with_var() {
        let input = "class MyClass { var x as Float; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "MyClass");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "x");
        assert_eq!(var.type_.as_ref().unwrap().ident, "Float");
    }

    #[test]
    fn test_parse_generic_type_declaration() {
        let input = "class MyClass { var speedPickerDefaults as Array<Float>; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "MyClass");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "speedPickerDefaults");
        assert_eq!(var.type_.as_ref().unwrap().ident, "Array");
        assert_eq!(var.type_.as_ref().unwrap().generic_params.len(), 1);
        assert_eq!(var.type_.as_ref().unwrap().generic_params[0].ident, "Float");
    }

    #[test]
    fn test_parse_class_with_assignment_in_function() {
        let input = "class Foo { function bar() { x = 1.0; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Parser should handle assignment in function body: {:?}", ast);
    }

    #[test]
    fn test_parse_assignment_with_array_type_cast() {
        let input = "class Foo { function bar() { x = [0, 0] as Array<Number>; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Parser should handle assignment of array literal with type cast: {:?}", ast);
    }

    #[test]
    fn test_parse_variable_declaration_with_initialization() {
        let input = "class Foo { var x as Float = 1.0; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "Foo");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "x");
        assert_eq!(var.type_.as_ref().unwrap().ident, "Float");
        // Verify the initialization value
        let initializer = var.initializer.as_ref().expect("Should have an initializer");
        if let Ast::BasicLit(LiteralValue::Double(value), _) = initializer.as_ref() {
            assert_eq!(*value, 1.0);
        } else {
            panic!("Initializer should be a double literal");
        }
    }

    #[test]
    fn test_parse_variable_declaration_with_complex_initialization() {
        let input = "class Foo { var x as Array<Number> = [1, 2, 3] as Array<Number>; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "Foo");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "x");
        assert_eq!(var.type_.as_ref().unwrap().ident, "Array");
        assert_eq!(var.type_.as_ref().unwrap().generic_params[0].ident, "Number");
        // Verify the initialization value
        let initializer = var.initializer.as_ref().expect("Should have an initializer");
        // Should be a type-cast expression
        if let Ast::TypeCast { expr, target_type: ty, .. } = initializer.as_ref() {
            assert_eq!(ty.ident, "Array");
            assert_eq!(ty.generic_params[0].ident, "Number");
            // The inner expr should be the array literal
            if let Ast::Array(elements, _) = expr.as_ref() {
                assert_eq!(elements.len(), 3);
                for element in elements {
                    if let Ast::BasicLit(LiteralValue::Long(value), _) = element {
                        assert!(*value >= 1 && *value <= 3);
                    } else {
                        panic!("Array element should be a number literal");
                    }
                }
            } else {
                panic!("Type cast inner expr should be an array literal");
            }
        } else {
            panic!("Initializer should be a type-cast expression");
        }
    }

    #[test]
    fn test_parse_variable_declaration_with_function_call() {
        let input = "class Foo { var x = Storage.getValue(\"pace\"); }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "Foo");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "x");
        // Verify the initialization value is a function call
        let initializer = var.initializer.as_ref().expect("Should have an initializer");
        if let Ast::Call { callee, args, .. } = initializer.as_ref() {
            if let Ast::Member { object, property, .. } = callee.as_ref() {
                if let Ast::Identifier(name, _) = object.as_ref() {
                    assert_eq!(name, "Storage");
                } else {
                    panic!("Function call object should be an identifier");
                }
                assert_eq!(property, "getValue");
            } else {
                panic!("Function call should be a member access");
            }
            assert_eq!(args.len(), 1);
            if let Ast::BasicLit(LiteralValue::String(value), _) = &args[0] {
                assert_eq!(value, "pace");
            } else {
                panic!("Function call argument should be a string literal");
            }
        } else {
            panic!("Initializer should be a function call");
        }
    }

    #[test]
    fn test_parse_if_with_binary_condition() {
        let input = "class Foo { function bar() { if (x == null) { y = 1; } } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        // Find the function
        let func = class.1.iter().find_map(|node| {
            if let Ast::Function { name, body, .. } = node {
                Some((name, body))
            } else {
                None
            }
        }).expect("Should find a function node");
        // Find the if statement
        let if_stmt = func.1.iter().find_map(|node| {
            if let Ast::If { condition, then_branch, .. } = node {
                Some((condition, then_branch))
            } else {
                None
            }
        }).expect("Should find an if statement");
        // The condition should be a binary expression: x == null
        if let Ast::Binary { left, operator, right, .. } = if_stmt.0.as_ref() {
            if let Ast::Identifier(name, _) = left.as_ref() {
                assert_eq!(name, "x");
            } else {
                panic!("Left side of binary should be identifier");
            }
            assert_eq!(*operator, crate::ast::BinaryOperator::Eq);
            if let Ast::BasicLit(LiteralValue::Null, _) = right.as_ref() {
            } else {
                panic!("Right side of binary should be null literal");
            }
        } else {
            panic!("Condition should be a binary expression");
        }
    }

    #[test]
    fn test_parse_function_with_underscore_parameter() {
        let input = "class Foo { function setSpeed(_speed as Float) { x = _speed; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "Foo");
        // Find the function node
        let func = class.1.iter().find_map(|node| {
            if let Ast::Function { name, args, .. } = node {
                Some((name, args))
            } else {
                None
            }
        }).expect("Should find a function node");
        assert_eq!(func.0, "setSpeed");
        assert_eq!(func.1.len(), 1);
        assert_eq!(func.1[0].name, "_speed");
        assert_eq!(func.1[0].type_.as_ref().unwrap().ident, "Float");
    }

    #[test]
    fn test_parse_function_with_return_type_and_body() {
        let input = "class Foo { function bar() as Void { x = 1; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Parser should handle function with return type and body: {:?}", ast);
    }

    #[test]
    fn test_parse_var_with_complex_initializer() {
        let input = "class Foo { function bar() { var x = arr[0] + arr[1] / 2; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Parser should handle var with complex initializer: {:?}", ast);
    }

    #[test]
    fn test_parse_array_literal_with_method_calls() {
        let input = r#"
            class Foo {
                function bar() {
                    var pace = Lang.format("$1$:$2$", [
                        minutesAndSeconds[0].format("%d"),
                        minutesAndSeconds[1].format("%02d"),
                    ]);
                }
            }
        "#;
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Parser should handle array literal with method calls as elements: {:?}", ast);
    }

    #[test]
    fn test_parse_class_with_private_variable() {
        let input = "class Foo { private var x as Float; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        // Match the document node and find the class
        let class = match ast {
            Ast::Document(nodes) => nodes.into_iter().find_map(|node| {
                if let Ast::Class { name, body, .. } = node {
                    Some((name, body))
                } else {
                    None
                }
            }).expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        };
        assert_eq!(class.0, "Foo");
        // Find the variable node
        let var = class.1.iter().find_map(|node| {
            if let Ast::Variable(var, _) = node {
                Some(var)
            } else {
                None
            }
        }).expect("Should find a variable node");
        assert_eq!(var.name, "x");
        assert_eq!(var.type_.as_ref().unwrap().ident, "Float");
        assert_eq!(var.visibility, Some(Visibility::Private));
    }

    #[test]
    fn test_parse_private_function() {
        let input = "class Foo { private function bar() {} }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse private function: {:?}", ast);
    }

    #[test]
    fn test_parse_static_hidden_function() {
        let input = "class Foo { static hidden function bar() {} }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse static hidden function: {:?}", ast);
    }

    #[test]
    fn test_parse_static_function() {
        let input = "class Foo { static function bar() {} }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse static function: {:?}", ast);
    }

    #[test]
    fn test_parse_hidden_function() {
        let input = "class Foo { hidden function bar() {} }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse hidden function: {:?}", ast);
    }

    #[test]
    fn test_parse_hidden_var() {
        let input = "class Foo { hidden var x = 1; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse hidden var: {:?}", ast);
    }

    #[test]
    fn test_parse_static_var() {
        let input = "class Foo { static var x = 1; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse static var: {:?}", ast);
    }
}


