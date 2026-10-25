use crate::ast::{
    BlockStmt, CaseLabel, CatchClause, DoWhileStmt, ElseBranch, Expr, ExprStmt, ForHeader, ForInit,
    ForStmt, IfStmt, Modifiers, Parens, ReturnStmt, Span, Stmt, SwitchCase, SwitchStmt, ThrowStmt,
    TryStmt, WhileStmt,
};
use crate::parser::{Parser, ParserError};
use crate::token;

impl Parser<'_> {
    /// Parse a `{ … }` block, consuming both braces.
    pub(crate) fn parse_block(&mut self) -> Result<BlockStmt, ParserError> {
        let start = self.expect(token::Type::LBrace)?.start;
        let mut stmts = Vec::new();
        loop {
            self.skip_semicolons();

            if self.current_token == token::Type::RBrace {
                break;
            }

            stmts.push(self.parse_statement()?);
        }

        let end = self.advance().end;

        Ok(BlockStmt {
            stmts,
            span: Span { start, end },
        })
    }

    fn parse_statement(&mut self) -> Result<Stmt, ParserError> {
        let start = self.current_token_start;

        match self.current_token {
            token::Type::Break => Ok(Stmt::Break(self.parse_keyword_stmt()?)),
            token::Type::Continue => Ok(Stmt::Continue(self.parse_keyword_stmt()?)),
            token::Type::Var => Ok(Stmt::Var(self.parse_var_decl(start, Modifiers::default())?)),
            token::Type::Return => self.parse_return_stmt(),
            token::Type::If => Ok(Stmt::If(self.parse_if_stmt()?)),
            token::Type::While => self.parse_while_stmt(),
            token::Type::Do => self.parse_do_while_stmt(),
            token::Type::For => self.parse_for_stmt(),
            token::Type::Switch => self.parse_switch_stmt(),
            token::Type::Try => self.parse_try_stmt(),
            token::Type::Throw => self.parse_throw_stmt(),
            token::Type::LBrace => Ok(Stmt::Block(self.parse_block()?)),
            _ => self.parse_expr_stmt(),
        }
    }

    /// Parse a statement that is only a keyword and its `;`, like `break;`.
    fn parse_keyword_stmt(&mut self) -> Result<Span, ParserError> {
        let start = self.advance().start;
        let end = self.expect(token::Type::Semicolon)?.end;

        Ok(Span { start, end })
    }

    fn parse_paren_expression(&mut self) -> Result<Parens<Expr>, ParserError> {
        let open = self.expect(token::Type::LParen)?.start;
        let inner = self.parse_expression()?;
        let close = self.expect(token::Type::RParen)?.end;

        Ok(Parens { open, inner, close })
    }

    fn parse_expression_list(&mut self) -> Result<Vec<Expr>, ParserError> {
        let mut exprs = vec![self.parse_expression()?];
        while self.current_token == token::Type::Comma {
            self.advance();
            exprs.push(self.parse_expression()?);
        }

        Ok(exprs)
    }

    fn parse_expr_stmt(&mut self) -> Result<Stmt, ParserError> {
        let expr = self.parse_expression()?;
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(Stmt::Expr(ExprStmt {
            span: Span {
                start: expr.span().start,
                end: semi.end,
            },
            expr,
            semi_pos: semi.start,
        }))
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let open = self.expect(token::Type::LParen)?.start;

        let (init, first_semi) = match self.current_token {
            token::Type::Semicolon => (None, self.advance().start),
            token::Type::Var => {
                let var_decl =
                    self.parse_var_decl(self.current_token_start, Modifiers::default())?;
                let first_semi = var_decl.semi_pos;

                (Some(ForInit::Var(var_decl)), first_semi)
            }
            _ => {
                let exprs = self.parse_expression_list()?;

                (
                    Some(ForInit::Expr(exprs)),
                    self.expect(token::Type::Semicolon)?.start,
                )
            }
        };

        let condition = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let second_semi = self.expect(token::Type::Semicolon)?.start;

        let update = if self.current_token == token::Type::RParen {
            None
        } else {
            Some(self.parse_expression_list()?)
        };
        let close = self.expect(token::Type::RParen)?.end;

        let body = self.parse_block()?;

        Ok(Stmt::For(ForStmt {
            header: Parens {
                open,
                inner: ForHeader {
                    init,
                    first_semi,
                    condition,
                    second_semi,
                    update,
                },
                close,
            },
            span: Span {
                start,
                end: body.span.end,
            },
            body,
        }))
    }

    fn parse_if_stmt(&mut self) -> Result<IfStmt, ParserError> {
        let start = self.advance().start;
        let condition = self.parse_paren_expression()?;
        let then_branch = self.parse_block()?;

        let else_kw_start = if self.current_token == token::Type::Else {
            Some(self.advance().start)
        } else {
            None
        };

        let else_branch = match (else_kw_start, &self.current_token) {
            (None, _) => None,
            (Some(_), token::Type::If) => Some(ElseBranch::If(Box::new(self.parse_if_stmt()?))),
            (Some(_), _) => Some(ElseBranch::Block(self.parse_block()?)),
        };

        Ok(IfStmt {
            condition,
            then_branch,
            else_branch,
            else_kw_start,
            span: Span {
                start,
                end: self.prev_token_end,
            },
        })
    }

    fn parse_return_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let value = if self.current_token == token::Type::Semicolon {
            None
        } else {
            Some(self.parse_expression()?)
        };
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(Stmt::Return(ReturnStmt {
            value,
            semi_pos: semi.start,
            span: Span {
                start,
                end: semi.end,
            },
        }))
    }

    fn parse_throw_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let value = self.parse_expression()?;
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(Stmt::Throw(ThrowStmt {
            value,
            semi_pos: semi.start,
            span: Span {
                start,
                end: semi.end,
            },
        }))
    }

    fn parse_switch_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let discriminant = self.parse_paren_expression()?;
        let brace_start = self.expect(token::Type::LBrace)?.start;

        let mut cases = Vec::new();
        while self.current_token != token::Type::RBrace {
            cases.push(self.parse_switch_case()?);
        }

        let end = self.expect(token::Type::RBrace)?.end;

        Ok(Stmt::Switch(SwitchStmt {
            discriminant,
            cases,
            brace_start,
            span: Span { start, end },
        }))
    }

    /// Parse one `case <…>:` or `default:` arm and the statements that follow, up to the next
    /// case/default or the closing `}`.
    fn parse_switch_case(&mut self) -> Result<SwitchCase, ParserError> {
        let start = self.current_token_start;
        let label = match self.current_token {
            token::Type::Case => {
                self.advance();
                if self.current_token == token::Type::InstanceOf {
                    self.advance();
                    CaseLabel::InstanceOf(self.parse_type()?)
                } else {
                    CaseLabel::Value(self.parse_expression()?)
                }
            }
            token::Type::Default => {
                self.advance();
                CaseLabel::Default
            }
            _ => {
                return Err(self.parse_error(format!(
                    "Expected `case` or `default` in switch body, got {:?}",
                    self.current_token
                )));
            }
        };

        let colon_end = self.expect(token::Type::Colon)?.end;

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
        let start = self.advance().start;
        let body = self.parse_block()?;

        let mut catches = Vec::new();
        while self.current_token == token::Type::Catch {
            catches.push(self.parse_catch_clause()?);
        }

        let finally = if self.current_token == token::Type::Finally {
            self.advance();
            Some(self.parse_block()?)
        } else {
            None
        };

        if catches.is_empty() && finally.is_none() {
            return Err(self
                .parse_error("`try` block must be followed by at least one `catch` or `finally`"));
        }

        Ok(Stmt::Try(TryStmt {
            body,
            catches,
            finally,
            span: Span {
                start,
                end: self.prev_token_end,
            },
        }))
    }

    fn parse_catch_clause(&mut self) -> Result<CatchClause, ParserError> {
        let start = self.advance().start;
        self.expect(token::Type::LParen)?;
        let binding = self.parse_identifier()?;
        let type_filter = if self.current_token == token::Type::InstanceOf {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };
        self.expect(token::Type::RParen)?;
        let body = self.parse_block()?;

        Ok(CatchClause {
            binding,
            type_filter,
            span: Span {
                start,
                end: body.span.end,
            },
            body,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let condition = self.parse_paren_expression()?;
        let body = self.parse_block()?;

        Ok(Stmt::While(WhileStmt {
            condition,
            span: Span {
                start,
                end: body.span.end,
            },
            body,
        }))
    }

    fn parse_do_while_stmt(&mut self) -> Result<Stmt, ParserError> {
        let start = self.advance().start;
        let body = self.parse_block()?;
        let while_kw_start = self.expect(token::Type::While)?.start;
        let condition = self.parse_paren_expression()?;
        let semi = self.expect(token::Type::Semicolon)?;

        Ok(Stmt::DoWhile(DoWhileStmt {
            body,
            while_kw_start,
            condition,
            semi_pos: semi.start,
            span: Span {
                start,
                end: semi.end,
            },
        }))
    }
}
