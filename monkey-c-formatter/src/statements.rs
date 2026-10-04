//! Statements, blocks and the `keyword (condition)` headers of `if`, `while` and `switch`.

use monkey_c_parser::ast::{
    BlockStmt, CaseLabel, CommentStmt, ElseBranch, Expr, ForInit, IfStmt, Parens, Span, Stmt,
    SwitchStmt, TryStmt,
};

use crate::Formatter;
use crate::doc::Doc;

impl Formatter {
    /// Render `<keyword> (cond)` with appropriate comment handling between
    /// `)` and the body block. `brace_start` is the byte offset of the opening
    /// `{` of the body; it bounds the after-paren trailing drain so comments
    /// that belong to the body are not accidentally captured here.
    fn paren_condition_header(
        &self,
        keyword: &str,
        condition: &Parens<Expr>,
        brace_start: usize,
    ) -> Doc {
        // Comments between the keyword and `(` stay there.
        let opening = Doc::concat(vec![
            Doc::text(format!("{keyword} ")),
            self.drain_leading_doc(condition.open),
            Doc::text("("),
        ]);
        let cond = &condition.inner;
        let paren_close = condition.close;
        let cond_start = cond.span().start;
        let cond_end = cond.span().end;

        // A comment that ends the `(` line would otherwise pull the condition up after it.
        if self.node_starts_new_line(condition.open + 1, cond_start) {
            let after_open = self.drain_trailing_doc(condition.open + 1, cond_start);
            let leading = self.drain_leading_doc(cond_start);
            let cond_doc = Doc::group(vec![self.condition_to_doc(cond)]);
            let cond_trailing = self.drain_trailing_doc(cond_end, paren_close);
            let before_close = self.drain_dangling_comments_doc(paren_close);
            let mut inner = vec![Doc::HardLine, leading, cond_doc, cond_trailing];
            if !matches!(before_close, Doc::Empty) {
                inner.push(Doc::HardLine);
                inner.push(before_close);
            }

            return Doc::concat(vec![
                opening,
                after_open,
                Doc::Indent(inner),
                Doc::HardLine,
                Doc::text(") "),
            ]);
        }

        let wrappable = matches!(cond, Expr::Binary(_));
        let cond_doc = self.condition_to_doc(cond);

        if wrappable {
            // Comments physically inside the condition parens (between cond_end and
            // paren_close): keep them before `)` so they don't move outside.
            let has_line_inside = self.has_trailing_line_comment(cond_end, paren_close);
            let cond_trailing = self.drain_trailing_doc(cond_end, paren_close);
            // Standalone comments between the condition end and `)` (e.g. `// c3`).
            let dangling = self.drain_dangling_comments_doc(paren_close);
            let has_inside = has_line_inside || !matches!(dangling, Doc::Empty);

            // Comments between `)` and the body `{`, on the same line as `)`.
            // Bounded by `brace_start` so we don't steal comments that belong
            // to the body (e.g. `// trailing` after a same-line `{ ... }`).
            let has_line_outside = self.has_trailing_line_comment(paren_close, brace_start);
            let after_paren = self.drain_trailing_doc(paren_close, brace_start);

            let mut group_parts = vec![opening, Doc::Indent(vec![cond_doc]), cond_trailing];

            if !matches!(dangling, Doc::Empty) {
                group_parts.push(Doc::HardLine);
                group_parts.push(dangling);
            }

            if has_inside {
                // A trailing `//` comment consumes the rest of its line, so `)` must
                // appear on the next line to avoid being swallowed by the comment.
                group_parts.push(Doc::HardLine);
            }

            group_parts.push(Doc::text(")"));

            if matches!(after_paren, Doc::Empty) {
                group_parts.push(Doc::flat_or_break(Doc::text(" "), Doc::HardLine));
            } else if has_line_outside {
                group_parts.push(after_paren);
                group_parts.push(Doc::HardLine);
            } else {
                group_parts.push(after_paren);
                group_parts.push(Doc::flat_or_break(Doc::text(" "), Doc::HardLine));
            }

            Doc::Group(group_parts)
        } else {
            // Comments after `)` are handled by the body or statement-level trailing drain.
            Doc::concat(vec![
                opening,
                cond_doc,
                self.condition_close(cond_end, paren_close),
                Doc::text(" "),
            ])
        }
    }

    /// The comments between a condition ending at `cond_end` and its `)`, then the `)`. A comment
    /// that ends the line puts the `)` on the next one.
    fn condition_close(&self, cond_end: usize, paren_close: usize) -> Doc {
        let breaks = self.has_trailing_line_comment(cond_end, paren_close);
        let mut parts = vec![self.drain_trailing_doc(cond_end, paren_close)];
        let before_close = self.drain_dangling_comments_doc(paren_close);
        if !matches!(before_close, Doc::Empty) {
            parts.push(Doc::Indent(vec![Doc::HardLine, before_close]));
            parts.push(Doc::HardLine);
        } else if breaks {
            parts.push(Doc::HardLine);
        }

        parts.push(Doc::text(")"));

        Doc::concat(parts)
    }

    fn if_stmt_to_doc(&self, s: &IfStmt) -> Doc {
        let header = self.paren_condition_header("if", &s.condition, s.then_branch.span.start);
        let mut parts = vec![header, self.block_to_doc(&s.then_branch)];

        // Bound the same-line trailing drain to the `else` keyword so that
        // comments on the `else {` / `else if` line are not captured here.
        let max_pos = s.else_kw_start.unwrap_or(usize::MAX);

        let has_trailing = self.has_trailing_line_comment(s.then_branch.span.end, max_pos);
        let trailing = self.drain_trailing_doc(s.then_branch.span.end, max_pos);

        // Drain standalone comments that sit between `}` and the `else` keyword
        // on their own lines (e.g. `}\n// note\nelse {`).  They must be consumed
        // before `block_after_keyword_to_doc` is called, otherwise `drain_leading_doc` inside
        // the block picks them up and places them between `else` and `{`.
        let before_else = s
            .else_kw_start
            .map(|kw| self.drain_leading_doc(kw))
            .unwrap_or(Doc::Empty);

        match &s.else_branch {
            None => {
                parts.push(trailing);
            }
            Some(ElseBranch::Block(b)) => {
                parts.push(trailing);
                if matches!(&before_else, Doc::Empty) {
                    if !has_trailing {
                        parts.push(Doc::text(" else"));
                    } else {
                        parts.push(Doc::HardLine);
                        parts.push(Doc::text("else"));
                    }
                } else {
                    // before_else is a standalone comment (its own line).
                    // Always needs a HardLine before it whether or not
                    // has_trailing, since trailing itself has no newline.
                    parts.push(Doc::HardLine);
                    parts.push(before_else);
                    parts.push(Doc::text("else"));
                }

                parts.push(self.block_after_keyword_to_doc(b));
            }
            Some(ElseBranch::If(inner)) => {
                parts.push(trailing);
                if matches!(&before_else, Doc::Empty) {
                    if !has_trailing {
                        parts.push(Doc::text(" else "));
                    } else {
                        parts.push(Doc::HardLine);
                        parts.push(Doc::text("else "));
                    }
                } else {
                    parts.push(Doc::HardLine);
                    parts.push(before_else);
                    parts.push(Doc::text("else "));
                }

                parts.push(self.if_stmt_to_doc(inner));
            }
        }

        Doc::Concat(parts)
    }

    fn switch_stmt_to_doc(&self, s: &SwitchStmt) -> Doc {
        let switch_body_span = Span {
            start: s.brace_start,
            end: s.span.end,
        };

        // The header comes first so it keeps the comments in front of its `(`. Before-brace
        // comments are drained before building the body so they aren't stolen by
        // drain_leading_doc inside the case-rendering loop.
        let header = self.paren_condition_header("switch", &s.discriminant, s.brace_start);
        let before_brace = self.drain_leading_doc(s.brace_start);
        // Drain same-line comment after `{` to place it on the `{` line, not indented below.
        let after_open = self.drain_after_open_brace(s.brace_start, s.span.end);
        let mut body = Vec::new();

        // Drain any comments before the first case.
        let first_case_start = s.cases.first().map(|c| c.span.start).unwrap_or(s.span.end);
        let before_first = self.drain_leading_doc(first_case_start);
        if !matches!(before_first, Doc::Empty) {
            body.push(before_first);
        }

        let mut previous_case_end: Option<usize> = None;

        for (i, case) in s.cases.iter().enumerate() {
            if let Some(prev_end) = previous_case_end {
                let blanks = self
                    .line_index
                    .blank_lines_between(prev_end as u32, case.span.start as u32);

                if blanks > 0 {
                    body.push(Doc::BlankLine);
                } else {
                    body.push(Doc::HardLine);
                }
            }

            body.push(self.drain_leading_doc(case.span.start));

            let mut header = vec![Doc::text("case ")];
            match &case.label {
                CaseLabel::Value(e) => {
                    header.push(self.expr_with_leading(e));
                    // Drain inline block comments between the value and ':',
                    // e.g. `case 1 /*NAME*/ :`.
                    header.push(self.drain_trailing_doc(e.span().end, case.label_span.end));
                }
                CaseLabel::InstanceOf(ty) => {
                    header.push(Doc::text("instanceof "));
                    header.push(self.type_to_doc(ty));
                }
                CaseLabel::Default => {
                    header = vec![Doc::text("default")];
                }
            }

            header.push(Doc::text(":"));
            let stmts_end = case
                .stmts
                .last()
                .map_or(case.label_span.end, |s| s.span().end);
            // Comments after the last statement stay in this case's body, except for the ones
            // directly above the next label which describe that label instead.
            let body_end = s.cases.get(i + 1).map_or(s.span.end, |next| {
                self.case_body_end(stmts_end, next.span.start)
            });
            let body_span = Span {
                start: case.label_span.end,
                end: body_end,
            };
            // A statement on the label's line, such as a block's `{`, keeps the comments after it.
            let label_line = self.line_index.line(case.label_span.end as u32 - 1);
            let header_comments_end = case
                .stmts
                .first()
                .map(|stmt| stmt.span().start)
                .filter(|start| self.line_index.line(*start as u32) == label_line)
                .unwrap_or(case.span.end);
            header.push(self.drain_after_open_brace(case.label_span.end, header_comments_end));
            body.push(Doc::Concat(header));

            let case_inner = self.stmts_to_doc(&case.stmts, body_span);
            if !matches!(case_inner, Doc::Empty) {
                body.push(Doc::Indent(vec![Doc::HardLine, case_inner]));
            }

            let comments_end = self.comment_cursor.borrow().last_end();
            previous_case_end = Some(stmts_end.max(comments_end));
        }

        // Drain any comments after the last case.
        let prev_end = s.cases.last().map_or(s.brace_start, |c| c.span.end);
        body.push(self.drain_remaining_comments(Some(prev_end), switch_body_span.end));

        Doc::concat(vec![
            header,
            Doc::bracketed(
                Doc::concat(vec![before_brace, Doc::text("{")]),
                after_open,
                Doc::concat(body),
                Doc::text("}"),
            ),
        ])
    }

    fn try_stmt_to_doc(&self, s: &TryStmt) -> Doc {
        let mut parts = vec![Doc::text("try "), self.block_to_doc(&s.body)];

        for catch in &s.catches {
            let mut header = vec![Doc::text(" catch ("), Doc::text(&catch.binding)];
            if let Some(ty) = &catch.type_filter {
                header.push(Doc::text(" instanceof "));
                header.push(self.type_to_doc(ty));
            }

            header.push(Doc::text(")"));
            parts.push(Doc::Concat(header));
            parts.push(self.block_after_keyword_to_doc(&catch.body));
        }

        if let Some(f) = &s.finally {
            parts.push(Doc::text(" finally"));
            parts.push(self.block_after_keyword_to_doc(f));
        }

        Doc::Concat(parts)
    }

    /// Render a block `{ … }` where the caller has already emitted the preceding space or token.
    /// Drains comments that appear between the last caller token and `{` (before-bracket zone)
    /// and after `{` on the same line (after-open-brace zone).
    pub(crate) fn block_to_doc(&self, block: &BlockStmt) -> Doc {
        let before_brace = self.drain_leading_doc(block.span.start);

        // Only drain same-line comments after `{` when the first statement is on a
        // different line. For single-line blocks like `{ /* note */ var x; }` the
        // inline comment belongs to the first statement and is captured by its
        // drain_leading_doc call inside stmts_to_doc.
        let brace_line = self.line_index.line(block.span.start as u32);
        let first_stmt_line = block
            .stmts
            .first()
            .map(|s| self.line_index.line(s.span().start as u32));
        let after_open = if first_stmt_line != Some(brace_line) {
            self.drain_after_open_brace(block.span.start, block.span.end)
        } else {
            Doc::Empty
        };

        Doc::bracketed(
            Doc::concat(vec![before_brace, Doc::text("{")]),
            after_open,
            self.stmts_to_doc(&block.stmts, block.span),
            Doc::text("}"),
        )
    }

    /// Render ` { … }` after `else`, `catch` or `finally`. A comment on its own line in front of
    /// the `{` keeps that line, so the block starts on a new line instead of after a space.
    fn block_after_keyword_to_doc(&self, block: &BlockStmt) -> Doc {
        let comment_on_own_line = self
            .comment_cursor
            .borrow()
            .peek_before(block.span.start)
            .is_some_and(|c| self.line_index.starts_line(c.span.start as u32));
        let separator = if comment_on_own_line {
            Doc::HardLine
        } else {
            Doc::text(" ")
        };

        Doc::concat(vec![separator, self.block_to_doc(block)])
    }

    /// Where a case body ends when the next label starts at `next_case_start`: before the run of
    /// own-line comments directly above that label, with no blank line in between.
    fn case_body_end(&self, stmts_end: usize, next_case_start: usize) -> usize {
        let stmts_end_line = self.line_index.line(stmts_end.saturating_sub(1) as u32);
        let comments: Vec<CommentStmt> = self
            .comment_cursor
            .borrow()
            .peek_in(stmts_end, next_case_start)
            .cloned()
            .collect();

        let mut body_end = next_case_start;
        for comment in comments.iter().rev() {
            let starts_on_stmts_line =
                self.line_index.line(comment.span.start as u32) == stmts_end_line;
            let blanks = self
                .line_index
                .blank_lines_between(comment.span.end as u32, body_end as u32);
            if starts_on_stmts_line || blanks > 0 {
                break;
            }

            body_end = comment.span.start;
        }

        body_end
    }

    fn stmts_to_doc(&self, stmts: &[Stmt], container: Span) -> Doc {
        let has_content = !stmts.is_empty()
            || self
                .comment_cursor
                .borrow()
                .has_comment_in(container.start + 1, container.end);
        if !has_content {
            return Doc::Empty;
        }

        let mut docs = Vec::new();
        let mut prev_end: Option<usize> = None;

        for stmt in stmts {
            let span = *stmt.span();
            let eff_start = self.effective_start(span);

            if let Some(pe) = prev_end {
                docs.push(self.gap_between_positions(pe, eff_start));
            }

            docs.push(self.stmt_to_doc_bounded(stmt, container.end));
            prev_end = Some(self.effective_end(span));
        }

        docs.push(self.drain_remaining_comments(prev_end, container.end));

        Doc::concat(docs)
    }

    /// Render a statement with its comments, capping the trailing comment drain at `max_pos`.
    /// Pass `container.end` when the stmt is inside a block so that comments
    /// after the closing `}` are not accidentally captured by the last stmt.
    fn stmt_to_doc_bounded(&self, stmt: &Stmt, max_pos: usize) -> Doc {
        let span = *stmt.span();
        let leading = self.drain_leading_doc(span.start);
        let inner = self.stmt_inner_to_doc(stmt);
        let trailing = self.drain_trailing_doc(span.end, max_pos);

        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => inner,
            _ => Doc::concat(vec![leading, inner, trailing]),
        }
    }

    fn stmt_inner_to_doc(&self, stmt: &Stmt) -> Doc {
        match stmt {
            Stmt::Block(block) => self.block_to_doc(block),
            Stmt::If(s) => self.if_stmt_to_doc(s),
            Stmt::While(s) => Doc::concat(vec![
                self.paren_condition_header("while", &s.condition, s.body.span.start),
                self.block_to_doc(&s.body),
            ]),
            Stmt::DoWhile(s) => {
                let condition = &s.condition;
                let mut parts = vec![
                    Doc::text("do "),
                    self.block_to_doc(&s.body),
                    Doc::text(" "),
                    self.drain_leading_doc(s.while_kw_start),
                    Doc::text("while "),
                    self.drain_leading_doc(condition.open),
                    Doc::text("("),
                    self.expr_with_leading_bounded(&condition.inner, condition.close),
                    self.condition_close(condition.inner.span().end, condition.close),
                ];
                self.push_before_semi(&mut parts, s.semi_pos);

                Doc::Concat(parts)
            }
            Stmt::For(s) => {
                let opening = Doc::concat(vec![
                    Doc::text("for "),
                    self.drain_leading_doc(s.header.open),
                    Doc::text("("),
                ]);
                let init_doc = match &s.header.inner.init {
                    None => Doc::Empty,
                    Some(ForInit::Var(v)) => Doc::concat(vec![
                        self.drain_leading_doc(v.span.start),
                        self.var_decl_to_doc(v),
                    ]),
                    Some(ForInit::Expr(exprs)) => self.expr_list_to_doc(exprs),
                };

                // Built in source order, so each `;` takes the comments written before it.
                let mut parts = vec![opening, init_doc];
                if s.header.inner.init.is_some() {
                    self.push_before_semi(&mut parts, s.header.inner.first_semi);
                } else {
                    // Nothing precedes the first `;`, so its comments hug the `(`.
                    parts.push(self.drain_hugging_doc(s.header.inner.first_semi));
                    parts.push(Doc::text(";"));
                }

                if let Some(condition) = &s.header.inner.condition {
                    parts.push(Doc::text(" "));
                    parts.push(self.expr_with_leading_bounded(condition, s.header.close));
                }

                self.push_before_semi(&mut parts, s.header.inner.second_semi);

                if let Some(update) = &s.header.inner.update {
                    parts.push(Doc::text(" "));
                    parts.push(self.expr_list_to_doc(update));
                }

                parts.push(self.drain_before_close_doc(s.header.close));
                parts.push(Doc::text(") "));
                parts.push(self.block_to_doc(&s.body));

                Doc::Concat(parts)
            }
            Stmt::Return(s) => match &s.value {
                None => Doc::text("return;"),
                Some(v) => {
                    let keyword_end = s.span.start + "return".len();
                    let mut parts = vec![
                        Doc::text("return"),
                        self.after_token(keyword_end, v.span().start, || self.expr_with_leading(v)),
                    ];
                    self.push_before_semi(&mut parts, s.semi_pos);

                    Doc::Concat(parts)
                }
            },
            Stmt::Break(_) => Doc::text("break;"),
            Stmt::Continue(_) => Doc::text("continue;"),
            Stmt::Throw(s) => {
                let keyword_end = s.span.start + "throw".len();
                let mut parts = vec![
                    Doc::text("throw"),
                    self.after_token(keyword_end, s.value.span().start, || {
                        self.expr_with_leading(&s.value)
                    }),
                ];
                self.push_before_semi(&mut parts, s.semi_pos);

                Doc::Concat(parts)
            }
            Stmt::Switch(s) => self.switch_stmt_to_doc(s),
            Stmt::Try(s) => self.try_stmt_to_doc(s),
            Stmt::Var(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Stmt::Expr(s) => {
                let mut parts = vec![self.expr_inner_to_doc(&s.expr)];
                self.push_before_semi(&mut parts, s.semi_pos);

                Doc::Concat(parts)
            }
        }
    }
}
