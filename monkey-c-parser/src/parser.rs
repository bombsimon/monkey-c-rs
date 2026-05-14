use crate::ast::{
    Ast, BlockStmt, ClassDecl, ForInit, ForStmt, FunctionDecl, IfStmt, ImportDecl, ReturnStmt,
    Span, Stmt, Type, VarStmt, Variable, Visibility, WhileStmt,
};
use crate::token;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParserError {
    TokenizerError(String),
    ParseError(String),
}

pub struct Parser<'a> {
    pub(crate) lexer: crate::lexer::Lexer<'a>,
    pub current_token: token::Type,
    /// Byte offset of the start of `current_token`.
    pub(crate) current_token_start: usize,
    /// Byte offset of the end (exclusive) of `current_token`.
    pub(crate) current_token_end: usize,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = crate::lexer::Lexer::new(source);
        let (start, token_type, end) = lexer.next_token();

        Self {
            lexer,
            current_token: token_type,
            current_token_start: start,
            current_token_end: end,
        }
    }

    pub fn parse(&mut self) -> Result<Ast, ParserError> {
        let mut nodes = Vec::new();

        loop {
            let decl = self.parse_declaration()?;
            if decl == Ast::Eof {
                break;
            }
            nodes.push(decl);
        }

        Ok(Ast::Document(nodes))
    }

    pub(crate) fn next_token_span(&mut self) -> (usize, token::Type, usize) {
        let (start, token_type, end) = self.lexer.next_token();
        self.current_token = token_type.clone();
        self.current_token_start = start;
        self.current_token_end = end;
        (start, token_type, end)
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
                self.next_token_span();

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
            token::Type::Identifier(name) => Ok(name.clone()),
            _ => Err(ParserError::ParseError(format!(
                "Expected identifier, got {:?}",
                self.current_token
            ))),
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

            if self.current_token != token::Type::Greater {
                params.push(self.parse_type()?);
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
        self.next_token_span();
        while self.current_token == token::Type::Dot {
            self.next_token_span(); // consume dot
            let next = self.parse_identifier()?;
            name.push('.');
            name.push_str(&next);
            self.next_token_span();
        }

        Ok(name)
    }

    /// Parse the contents of a `var` declaration after the `var` keyword has been consumed.
    /// Does NOT consume a trailing semicolon — callers set the final span after consuming it.
    fn parse_var_contents(
        &mut self,
        visibility: Option<Visibility>,
        is_static: bool,
        is_hidden: bool,
    ) -> Result<VarStmt, ParserError> {
        let name = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier

        let type_ = if self.current_token == token::Type::As {
            self.next_token_span();
            Some(self.parse_type()?)
        } else {
            None
        };

        let initializer = if self.current_token == token::Type::Assign {
            self.next_token_span(); // consume =
            Some(Box::new(self.parse_expression()?))
        } else {
            None
        };

        Ok(VarStmt {
            variable: Variable {
                name,
                type_,
                visibility,
                initializer,
                is_static,
                is_hidden,
            },
            // Callers fill in the real span after consuming the trailing semicolon.
            span: Span { start: 0, end: 0 },
        })
    }

    fn parse_declaration(&mut self) -> Result<Ast, ParserError> {
        let start = self.current_token_start;
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

        match self.current_token.clone() {
            token::Type::Import => {
                self.next_token_span();
                let name = self.parse_dotted_identifier()?;
                let alias = if self.current_token == token::Type::As {
                    self.next_token_span();
                    Some(self.parse_identifier()?)
                } else {
                    None
                };
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Ast::Import(ImportDecl {
                    name,
                    alias,
                    span: Span {
                        start,
                        end: semi_end,
                    },
                }))
            }
            token::Type::Class => {
                self.next_token_span();
                let name = self.parse_identifier()?;
                self.next_token_span();
                let extends = if self.current_token == token::Type::Extends {
                    self.next_token_span();
                    let n = self.parse_identifier()?;
                    self.next_token_span();
                    Some(n)
                } else {
                    None
                };

                self.assert_next_token(&[token::Type::LBrace])?;
                let body = self.parse_class_body()?;
                let rbrace_end = self.current_token_end;
                self.assert_next_token(&[token::Type::RBrace])?;
                Ok(Ast::Class(ClassDecl {
                    name,
                    extends,
                    annotations: Vec::new(),
                    body,
                    span: Span {
                        start,
                        end: rbrace_end,
                    },
                }))
            }
            token::Type::Function => {
                self.next_token_span();
                let name = self.parse_identifier()?;
                self.next_token_span();
                let args = self.parse_function_args()?;
                let returns = if self.current_token == token::Type::As {
                    self.next_token_span();
                    Some(self.parse_type()?)
                } else {
                    None
                };

                let brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                let body = self.parse_block(brace_start)?;
                let end = body.span.end;

                Ok(Ast::Function(FunctionDecl {
                    name,
                    args,
                    returns,
                    annotations: Vec::new(),
                    body,
                    visibility,
                    is_static,
                    is_hidden,
                    span: Span { start, end },
                }))
            }
            token::Type::Var => {
                self.next_token_span();
                let mut var_stmt = self.parse_var_contents(visibility, is_static, is_hidden)?;
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                var_stmt.span = Span {
                    start,
                    end: semi_end,
                };
                Ok(Ast::Variable(var_stmt))
            }
            token::Type::Comment(content) => {
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Ast::Comment(content, Span { start, end }))
            }
            token::Type::Annotation(content) => {
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Ast::Annotation(content, Span { start, end }))
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
            self.next_token_span();

            let type_ = if self.current_token == token::Type::As {
                self.next_token_span();
                Some(self.parse_type()?)
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
                self.next_token_span();
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

    /// Parse a block body. `brace_start` is the byte offset of the already-consumed `{`.
    /// Consumes the closing `}`.
    pub(crate) fn parse_block(&mut self, brace_start: usize) -> Result<BlockStmt, ParserError> {
        let mut stmts = Vec::new();
        while self.current_token != token::Type::RBrace {
            stmts.push(self.parse_statement()?);
        }
        let end = self.current_token_end;
        self.next_token_span(); // consume }

        Ok(BlockStmt {
            stmts,
            span: Span {
                start: brace_start,
                end,
            },
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        match self.current_token.clone() {
            token::Type::Var => {
                let start = self.current_token_start;
                self.next_token_span();
                let mut var_stmt = self.parse_var_contents(None, false, false)?;
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                var_stmt.span = Span {
                    start,
                    end: semi_end,
                };
                Ok(Stmt::Var(var_stmt))
            }
            token::Type::Return => {
                let start = self.current_token_start;
                self.next_token_span();
                let value = if self.current_token == token::Type::Semicolon {
                    None
                } else {
                    Some(self.parse_expression()?)
                };
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Stmt::Return(ReturnStmt {
                    value,
                    span: Span {
                        start,
                        end: semi_end,
                    },
                }))
            }
            token::Type::Break => {
                let start = self.current_token_start;
                self.next_token_span();
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Stmt::Break(Span {
                    start,
                    end: semi_end,
                }))
            }
            token::Type::Continue => {
                let start = self.current_token_start;
                self.next_token_span();
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Stmt::Continue(Span {
                    start,
                    end: semi_end,
                }))
            }
            token::Type::If => {
                let start = self.current_token_start;
                self.next_token_span();
                self.assert_next_token(&[token::Type::LParen])?;
                let condition = self.parse_expression_no_postfix()?;
                self.assert_next_token(&[token::Type::RParen])?;

                let then_brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                let then_branch = self.parse_block(then_brace_start)?;

                let else_branch = if self.current_token == token::Type::Else {
                    self.next_token_span(); // consume `else`
                    let else_brace_start = self.current_token_start;
                    self.next_token_span(); // consume `{`
                    Some(self.parse_block(else_brace_start)?)
                } else {
                    None
                };

                let end = else_branch
                    .as_ref()
                    .map(|b| b.span.end)
                    .unwrap_or(then_branch.span.end);

                Ok(Stmt::If(IfStmt {
                    condition,
                    then_branch,
                    else_branch,
                    span: Span { start, end },
                }))
            }
            token::Type::While => {
                let start = self.current_token_start;
                self.next_token_span();
                let condition = self.parse_expression()?;

                let brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                let body = self.parse_block(brace_start)?;
                let end = body.span.end;

                Ok(Stmt::While(WhileStmt {
                    condition,
                    body,
                    span: Span { start, end },
                }))
            }
            token::Type::For => {
                let start = self.current_token_start;
                self.assert_next_token(&[token::Type::For])?;
                self.assert_next_token(&[token::Type::LParen])?;

                let init = match self.current_token {
                    token::Type::Semicolon => {
                        self.next_token_span();
                        None
                    }
                    token::Type::Var => {
                        let var_start = self.current_token_start;
                        self.next_token_span(); // consume `var`
                        let mut var_stmt = self.parse_var_contents(None, false, false)?;
                        let semi_end = self.current_token_end;
                        self.assert_next_token(&[token::Type::Semicolon])?;
                        var_stmt.span = Span {
                            start: var_start,
                            end: semi_end,
                        };
                        Some(ForInit::Var(var_stmt))
                    }
                    _ => {
                        let expr = self.parse_expression()?;
                        self.assert_next_token(&[token::Type::Semicolon])?;
                        Some(ForInit::Expr(expr))
                    }
                };

                let condition = if self.current_token == token::Type::Semicolon {
                    None
                } else {
                    Some(self.parse_expression()?)
                };

                self.assert_next_token(&[token::Type::Semicolon])?;

                let update = if self.current_token == token::Type::RParen {
                    None
                } else {
                    Some(self.parse_expression()?)
                };

                self.assert_next_token(&[token::Type::RParen])?;

                let brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                let body = self.parse_block(brace_start)?;
                let end = body.span.end;

                Ok(Stmt::For(ForStmt {
                    init,
                    condition,
                    update,
                    body,
                    span: Span { start, end },
                }))
            }
            token::Type::Comment(content) => {
                let start = self.current_token_start;
                let end = self.current_token_end;
                self.next_token_span();
                Ok(Stmt::Comment(content, Span { start, end }))
            }
            token::Type::LBrace => {
                let brace_start = self.current_token_start;
                self.next_token_span(); // consume {
                let block = self.parse_block(brace_start)?;
                Ok(Stmt::Block(block))
            }
            _ => {
                let expr = self.parse_expression()?;
                self.assert_next_token(&[token::Type::Semicolon])?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    pub fn parse_class(&mut self) -> Result<Ast, ParserError> {
        let start = self.current_token_start;
        let name = self.parse_identifier()?;
        let extends = if self.current_token == token::Type::Extends {
            self.next_token_span();
            Some(self.parse_identifier()?)
        } else {
            None
        };

        let body = self.parse_class_body()?;
        let rbrace_end = self.current_token_end;
        self.next_token_span(); // consume }

        Ok(Ast::Class(ClassDecl {
            name,
            extends,
            annotations: Vec::new(),
            body,
            span: Span {
                start,
                end: rbrace_end,
            },
        }))
    }

    pub(crate) fn current_token_is(&self, expect: &[token::Type]) -> bool {
        expect.contains(&self.current_token)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::{Expr, LitExpr, LiteralValue, Stmt, VarStmt, Visibility};

    fn find_class(ast: Ast) -> ClassDecl {
        match ast {
            Ast::Document(nodes) => nodes
                .into_iter()
                .find_map(|node| {
                    if let Ast::Class(c) = node {
                        Some(c)
                    } else {
                        None
                    }
                })
                .expect("Should find a class node"),
            _ => panic!("Should be a document node"),
        }
    }

    fn find_var_in_body(body: &[Ast]) -> &VarStmt {
        body.iter()
            .find_map(|node| {
                if let Ast::Variable(v) = node {
                    Some(v)
                } else {
                    None
                }
            })
            .expect("Should find a variable node")
    }

    #[test]
    fn test_parse_simple_class_with_var() {
        let input = "class MyClass { var x as Float; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        let class = find_class(ast);
        assert_eq!(class.name, "MyClass");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "x");
        assert_eq!(var.variable.type_.as_ref().unwrap().ident, "Float");
    }

    #[test]
    fn test_parse_generic_type_declaration() {
        let input = "class MyClass { var speedPickerDefaults as Array<Float>; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        let class = find_class(ast);
        assert_eq!(class.name, "MyClass");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "speedPickerDefaults");
        assert_eq!(var.variable.type_.as_ref().unwrap().ident, "Array");
        assert_eq!(var.variable.type_.as_ref().unwrap().generic_params.len(), 1);
        assert_eq!(
            var.variable.type_.as_ref().unwrap().generic_params[0].ident,
            "Float"
        );
    }

    #[test]
    fn test_parse_class_with_assignment_in_function() {
        let input = "class Foo { function bar() { x = 1.0; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(
            ast.is_ok(),
            "Parser should handle assignment in function body: {:?}",
            ast
        );
    }

    #[test]
    fn test_parse_assignment_with_array_type_cast() {
        let input = "class Foo { function bar() { x = [0, 0] as Array<Number>; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(
            ast.is_ok(),
            "Parser should handle assignment of array literal with type cast: {:?}",
            ast
        );
    }

    #[test]
    fn test_parse_variable_declaration_with_initialization() {
        let input = "class Foo { var x as Float = 1.0; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        let class = find_class(ast);
        assert_eq!(class.name, "Foo");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "x");
        assert_eq!(var.variable.type_.as_ref().unwrap().ident, "Float");

        let initializer = var
            .variable
            .initializer
            .as_ref()
            .expect("Should have initializer");
        if let Expr::Lit(LitExpr {
            value: LiteralValue::Double(value),
            ..
        }) = initializer.as_ref()
        {
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
        let class = find_class(ast);
        assert_eq!(class.name, "Foo");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "x");
        assert_eq!(var.variable.type_.as_ref().unwrap().ident, "Array");
        assert_eq!(
            var.variable.type_.as_ref().unwrap().generic_params[0].ident,
            "Number"
        );

        let initializer = var
            .variable
            .initializer
            .as_ref()
            .expect("Should have initializer");
        if let Expr::TypeCast(tc) = initializer.as_ref() {
            assert_eq!(tc.target_type.ident, "Array");
            assert_eq!(tc.target_type.generic_params[0].ident, "Number");
            if let Expr::Array(arr) = tc.expr.as_ref() {
                assert_eq!(arr.elements.len(), 3);
                for element in &arr.elements {
                    if let Expr::Lit(LitExpr {
                        value: LiteralValue::Long(value),
                        ..
                    }) = element
                    {
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
        let input = r#"class Foo { var x = Storage.getValue("pace"); }"#;
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        let class = find_class(ast);
        assert_eq!(class.name, "Foo");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "x");

        let initializer = var
            .variable
            .initializer
            .as_ref()
            .expect("Should have initializer");
        if let Expr::Call(call) = initializer.as_ref() {
            if let Expr::Member(member) = call.callee.as_ref() {
                if let Expr::Ident(ident) = member.object.as_ref() {
                    assert_eq!(ident.name, "Storage");
                } else {
                    panic!("Function call object should be an identifier");
                }
                assert_eq!(member.property, "getValue");
            } else {
                panic!("Function call should be a member access");
            }
            assert_eq!(call.args.len(), 1);
            if let Expr::Lit(LitExpr {
                value: LiteralValue::String(value),
                ..
            }) = &call.args[0]
            {
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
        let class = find_class(ast);

        let func = class
            .body
            .iter()
            .find_map(|node| {
                if let Ast::Function(f) = node {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("Should find a function node");

        let if_stmt = func
            .body
            .stmts
            .iter()
            .find_map(|stmt| {
                if let Stmt::If(s) = stmt {
                    Some(s)
                } else {
                    None
                }
            })
            .expect("Should find an if statement");

        if let Expr::Binary(bin) = &if_stmt.condition {
            if let Expr::Ident(ident) = bin.left.as_ref() {
                assert_eq!(ident.name, "x");
            } else {
                panic!("Left side of binary should be identifier");
            }
            assert_eq!(bin.operator, crate::ast::BinaryOperator::Eq);
            if let Expr::Lit(LitExpr {
                value: LiteralValue::Null,
                ..
            }) = bin.right.as_ref()
            {
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
        let class = find_class(ast);
        assert_eq!(class.name, "Foo");

        let func = class
            .body
            .iter()
            .find_map(|node| {
                if let Ast::Function(f) = node {
                    Some(f)
                } else {
                    None
                }
            })
            .expect("Should find a function node");

        assert_eq!(func.name, "setSpeed");
        assert_eq!(func.args.len(), 1);
        assert_eq!(func.args[0].name, "_speed");
        assert_eq!(func.args[0].type_.as_ref().unwrap().ident, "Float");
    }

    #[test]
    fn test_parse_function_with_return_type_and_body() {
        let input = "class Foo { function bar() as Void { x = 1; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(
            ast.is_ok(),
            "Parser should handle function with return type and body: {:?}",
            ast
        );
    }

    #[test]
    fn test_parse_var_with_complex_initializer() {
        let input = "class Foo { function bar() { var x = arr[0] + arr[1] / 2; } }";
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(
            ast.is_ok(),
            "Parser should handle var with complex initializer: {:?}",
            ast
        );
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
        assert!(
            ast.is_ok(),
            "Parser should handle array literal with method calls as elements: {:?}",
            ast
        );
    }

    #[test]
    fn test_parse_class_with_private_variable() {
        let input = "class Foo { private var x as Float; }";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse successfully");
        let class = find_class(ast);
        assert_eq!(class.name, "Foo");
        let var = find_var_in_body(&class.body);
        assert_eq!(var.variable.name, "x");
        assert_eq!(var.variable.type_.as_ref().unwrap().ident, "Float");
        assert_eq!(var.variable.visibility, Some(Visibility::Private));
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
        assert!(
            ast.is_ok(),
            "Should parse static hidden function: {:?}",
            ast
        );
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

    #[test]
    fn test_parse_complex_expressions() {
        let input = r#"
            function complexMath(x as Number, y as Number) as Number {
                var result = (x * y) + (x / y);
                if (result > 100) {
                    return result;
                } else {
                    return result * 2;
                }
            }
        "#;
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse complex expression: {:?}", ast);
    }

    #[test]
    fn test_parse_collections() {
        let input = r#"
            function collections() as Void {
                var arr = [1, 2, 3, 4, 5];
                var dict = {"key1": "value1", "key2": "value2"};
                var sum = 0;
                for (var i = 0; i < arr.size(); i++) {
                    sum = sum + arr[i];
                }
            }
        "#;
        let mut parser = Parser::new(input);
        let ast = parser.parse();
        assert!(ast.is_ok(), "Should parse collections: {:?}", ast);
    }

    #[test]
    fn test_span_comment() {
        let input = "// hello\nvar x = 1;";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse");
        if let Ast::Document(nodes) = ast {
            if let Ast::Comment(_, span) = &nodes[0] {
                assert_eq!(span.start, 0);
                assert!(
                    span.end > span.start,
                    "comment span should have non-zero length"
                );
            } else {
                panic!("First node should be a comment");
            }
        }
    }

    #[test]
    fn test_span_var() {
        let input = "var x = 1;";
        let mut parser = Parser::new(input);
        let ast = parser.parse().expect("Should parse");
        if let Ast::Document(nodes) = ast {
            if let Ast::Variable(var) = &nodes[0] {
                assert_eq!(var.span.start, 0);
                assert!(var.span.end > 0, "var span end should be > 0");
            } else {
                panic!("Should be a variable");
            }
        }
    }
}
