            token::Type::If => {
                self.next_token_span(); // advance past If token
                self.assert_next_token(&[token::Type::LParen])?; // consume opening parenthesis
                let condition = Box::new(self.parse_expression()?);
                self.assert_next_token(&[token::Type::RParen])?; // consume closing parenthesis
                self.assert_next_token(&[token::Type::LBrace])?; // consume opening brace
                let body = Box::new(Ast::Block(self.parse_block()?, Span { start: 0, end: 0 }));
                let (_, _, end) = self.peek_token_span();
                Ok(Ast::If {
                    condition,
                    body,
                    else_branch: None,
                    span: Span { start: 0, end },
                })
            } 
