use crate::ast::{
    BlockStmt, CaseLabel, CatchClause, DoWhileStmt, ElseBranch, ForHeader, ForInit, ForStmt,
    IfStmt, Parens, ReturnStmt, Span, Stmt, SwitchCase, SwitchStmt, ThrowStmt, TryStmt, WhileStmt,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    /// Parse a block body. `brace_start` is the byte offset of the already-consumed `{`.
    /// Consumes the closing `}`.
    pub(crate) fn parse_block(&mut self, brace_start: usize) -> Result<BlockStmt, ParserError> {
        let mut stmts = Vec::new();
        loop {
            self.skip_semicolons();

            if self.current_token == token::Type::RBrace {
                break;
            }

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
            token::Type::Break => self.parse_break_stmt(),
            token::Type::Continue => self.parse_continue_stmt(),
            token::Type::Var => self.parse_var_stmt(),
            token::Type::Return => self.parse_return_stmt(),
            token::Type::If => self.parse_if_stmt(),
            token::Type::While => self.parse_while_stmt(),
            token::Type::Do => self.parse_do_while_stmt(),
            token::Type::For => self.parse_for_stmt(),
            token::Type::Switch => self.parse_switch_stmt(),
            token::Type::Try => self.parse_try_stmt(),
            token::Type::Throw => self.parse_throw_stmt(),
            token::Type::LBrace => self.parse_block_stmt(),
            _ => self.parse_expr_stmt(),
        }
    }

    fn parse_block_stmt(&mut self) -> Result<Stmt, ParserError> {
        let brace_start = self.current_token_start;
        self.next_token_span(); // consume `{`
        let block = self.parse_block(brace_start)?;

        Ok(Stmt::Block(block))
    }

    fn parse_break_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Break(Span {
            start,
            end: semi_end,
        }))
    }

    fn parse_continue_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Continue(Span {
            start,
            end: semi_end,
        }))
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expression()?;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Expr(expr))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `for`
        let header_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;

        let (init, first_semi) = match self.current_token {
            token::Type::Semicolon => {
                let fs = self.current_token_start;
                self.next_token_span();
                (None, fs)
            }
            token::Type::Var => {
                let var_start = self.current_token_start;
                self.next_token_span(); // consume `var`
                let mut var_decl = self.parse_var_contents(None, false)?;
                let fs = self.current_token_start;
                let semi_end = self.current_token_end;
                self.assert_next_token(&[token::Type::Semicolon])?;
                var_decl.semi_pos = fs;
                var_decl.span = Span {
                    start: var_start,
                    end: semi_end,
                };
                (Some(ForInit::Var(var_decl)), fs)
            }
            _ => {
                let mut exprs = vec![self.parse_expression()?];
                while self.current_token == token::Type::Comma {
                    self.next_token_span(); // consume `,`
                    exprs.push(self.parse_expression()?);
                }
                let fs = self.current_token_start;
                self.assert_next_token(&[token::Type::Semicolon])?;
                (Some(ForInit::Expr(exprs)), fs)
            }
        };

        let condition = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let second_semi = self.current_token_start;
        self.assert_next_token(&[token::Type::Semicolon])?;

        let update = if self.current_token == token::Type::RParen {
            None
        } else {
            let mut exprs = vec![self.parse_expression()?];
            while self.current_token == token::Type::Comma {
                self.next_token_span(); // consume `,`
                exprs.push(self.parse_expression()?);
            }

            Some(exprs)
        };
        let header_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let header = Parens {
            open: header_open,
            inner: ForHeader {
                init,
                condition,
                update,
            },
            close: header_close,
        };

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(Stmt::For(ForStmt {
            header,
            body,
            first_semi,
            second_semi,
            span: Span { start, end },
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, ParserError> {
        let inner = self.parse_if_inner()?;

        Ok(Stmt::If(inner))
    }

    fn parse_if_inner(&mut self) -> Result<IfStmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let cond_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let cond_inner = self.parse_expression()?;
        let cond_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let condition = Parens {
            open: cond_open,
            inner: cond_inner,
            close: cond_close,
        };

        let then_brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let then_branch = self.parse_block(then_brace_start)?;

        let else_kw_start = if self.current_token == token::Type::Else {
            Some(self.current_token_start)
        } else {
            None
        };

        let else_branch = if else_kw_start.is_some() {
            self.next_token_span(); // consume `else`
            if self.current_token == token::Type::If {
                Some(ElseBranch::If(Box::new(self.parse_if_inner()?)))
            } else {
                let else_brace_start = self.current_token_start;
                self.assert_next_token(&[token::Type::LBrace])?;
                Some(ElseBranch::Block(self.parse_block(else_brace_start)?))
            }
        } else {
            None
        };

        let end = else_branch
            .as_ref()
            .map(|b| b.span().end)
            .unwrap_or(then_branch.span.end);

        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
            else_kw_start,
            span: Span { start, end },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let value = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let semi_pos = self.current_token_start;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Return(ReturnStmt {
            value,
            semi_pos,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `throw`
        let value = self.parse_expression()?;
        let semi_pos = self.current_token_start;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::Throw(ThrowStmt {
            value,
            semi_pos,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }

    fn parse_switch_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `switch`
        let disc_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let disc_inner = self.parse_expression()?;
        let disc_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let discriminant = Parens {
            open: disc_open,
            inner: disc_inner,
            close: disc_close,
        };
        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;

        let mut cases = Vec::new();
        while self.current_token != token::Type::RBrace {
            cases.push(self.parse_switch_case()?);
        }

        let end = self.current_token_end;
        self.assert_next_token(&[token::Type::RBrace])?;

        Ok(Stmt::Switch(SwitchStmt {
            discriminant,
            cases,
            brace_start,
            span: Span { start, end },
        }))
    }

    /// Parse one `case <…>:` or `default:` arm and the statements that
    /// follow, up to the next case/default or the closing `}`. Leading
    /// comments are filled in by [`Parser::parse_switch_stmt`].
    fn parse_switch_case(&mut self) -> Result<SwitchCase, ParserError> {
        let start = self.current_token_start;
        let label = match self.current_token.clone() {
            token::Type::Case => {
                self.next_token_span(); // consume `case`
                if self.current_token == token::Type::InstanceOf {
                    self.next_token_span(); // consume `instanceof`
                    CaseLabel::InstanceOf(self.parse_type()?)
                } else {
                    CaseLabel::Value(self.parse_expression()?)
                }
            }
            token::Type::Default => {
                self.next_token_span(); // consume `default`
                CaseLabel::Default
            }
            _ => {
                return Err(self.parse_error(format!(
                    "Expected `case` or `default` in switch body, got {:?}",
                    self.current_token
                )));
            }
        };

        let colon_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Colon])?;

        let mut stmts = Vec::new();
        while !matches!(
            self.current_token,
            token::Type::Case | token::Type::Default | token::Type::RBrace
        ) {
            stmts.push(self.parse_statement()?);
        }

        let end = stmts
            .last()
            .map(|s| s.span().end)
            .unwrap_or(self.current_token_end);

        Ok(SwitchCase {
            label,
            stmts,
            label_span: Span {
                start,
                end: colon_end,
            },
            span: Span { start, end },
        })
    }

    fn parse_try_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let header_end = self.current_token_end;
        self.next_token_span(); // consume `try`

        let body_brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(body_brace_start)?;

        let mut catches = Vec::new();
        while self.current_token == token::Type::Catch {
            catches.push(self.parse_catch_clause()?);
        }

        let finally = if self.current_token == token::Type::Finally {
            self.next_token_span(); // consume `finally`
            let brace_start = self.current_token_start;
            self.assert_next_token(&[token::Type::LBrace])?;
            Some(self.parse_block(brace_start)?)
        } else {
            None
        };

        if catches.is_empty() && finally.is_none() {
            return Err(self.parse_error(
                "`try` block must be followed by at least one `catch` or `finally`".to_string(),
            ));
        }

        let end = finally
            .as_ref()
            .map(|b| b.span.end)
            .or_else(|| catches.last().map(|c| c.span.end))
            .unwrap_or(body.span.end);

        Ok(Stmt::Try(TryStmt {
            body,
            catches,
            finally,
            header_end,
            span: Span { start, end },
        }))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `catch`
        self.assert_next_token(&[token::Type::LParen])?;
        let binding = self.parse_identifier()?;
        self.next_token_span(); // advance past identifier
        let type_filter = if self.current_token == token::Type::InstanceOf {
            self.next_token_span(); // consume `instanceof`
            Some(self.parse_type()?)
        } else {
            None
        };
        self.assert_next_token(&[token::Type::RParen])?;
        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;
        let end = body.span.end;

        Ok(CatchClause {
            binding,
            type_filter,
            body,
            span: Span { start, end },
        })
    }

    fn parse_var_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span();
        let mut var_decl = self.parse_var_contents(None, false)?;
        let semi_pos = self.current_token_start;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;
        var_decl.semi_pos = semi_pos;
        var_decl.span = Span {
            start,
            end: semi_end,
        };

        Ok(Stmt::Var(var_decl))
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        self.next_token_span(); // consume `while`
        let cond_open = self.current_token_start;
        self.assert_next_token(&[token::Type::LParen])?;
        let cond_inner = self.parse_expression()?;
        let cond_close = self.current_token_end;
        self.assert_next_token(&[token::Type::RParen])?;
        let condition = Parens {
            open: cond_open,
            inner: cond_inner,
            close: cond_close,
        };

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

    fn parse_do_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;
        let header_end = self.current_token_end;
        self.next_token_span(); // consume `do`

        let brace_start = self.current_token_start;
        self.assert_next_token(&[token::Type::LBrace])?;
        let body = self.parse_block(brace_start)?;

        self.assert_next_token(&[token::Type::While])?;
        self.assert_next_token(&[token::Type::LParen])?;
        let condition = self.parse_expression()?;
        self.assert_next_token(&[token::Type::RParen])?;
        let semi_end = self.current_token_end;
        self.assert_next_token(&[token::Type::Semicolon])?;

        Ok(Stmt::DoWhile(DoWhileStmt {
            body,
            condition,
            header_end,
            span: Span {
                start,
                end: semi_end,
            },
        }))
    }
}
