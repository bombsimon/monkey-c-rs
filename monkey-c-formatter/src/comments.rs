//! Comment placement: draining comments from the cursor at the right spots and rendering them around
//! the tokens they were written next to.

use monkey_c_parser::ast::{CommentStmt, Expr, Span, Spanned};

use crate::Formatter;
use crate::doc::Doc;

impl Formatter {
    /// Drain comments with `span.start < pos` and render them as leading
    /// content (each on its own line, or inline for a same-line block comment).
    /// Drain any comments before the spanned node, then prepend them to its text doc.
    pub(crate) fn at(&self, spanned: &Spanned<String>) -> Doc {
        let doc = Doc::text(&spanned.node);
        let leading = self.drain_leading_doc(spanned.start());

        match leading {
            Doc::Empty => doc,
            d => Doc::Concat(vec![d, doc]),
        }
    }

    pub(crate) fn drain_leading_doc(&self, pos: usize) -> Doc {
        let comments = self.comment_cursor.borrow_mut().drain_before(pos);
        if comments.is_empty() {
            return Doc::Empty;
        }

        let node_line = self.line_index.line(pos as u32);
        let mut parts = Vec::new();
        for (i, c) in comments.iter().enumerate() {
            let comment_end_line = self.line_index.line(c.span.end.saturating_sub(1) as u32);
            parts.push(self.comment_to_doc(c));

            if c.is_block && comment_end_line == node_line {
                parts.push(Doc::text(" "));
            } else {
                let next_start = comments.get(i + 1).map(|nc| nc.span.start).unwrap_or(pos);
                let blanks = self
                    .line_index
                    .blank_lines_between(c.span.end as u32, next_start as u32);

                if blanks > 0 {
                    parts.push(Doc::BlankLine);
                } else {
                    parts.push(Doc::HardLine);
                }
            }
        }

        Doc::Concat(parts)
    }

    /// Drain all remaining comments before `pos` and render them separated by
    /// `HardLine` (or `BlankLine` when blank lines appear in source), but
    /// WITHOUT a trailing newline after the last comment.
    ///
    /// Used for trailing/dangling comments at the end of a block body where
    /// the enclosing structure already provides the final newline before `}`.
    pub(crate) fn drain_dangling_comments_doc(&self, pos: usize) -> Doc {
        let comments = self.comment_cursor.borrow_mut().drain_before(pos);
        if comments.is_empty() {
            return Doc::Empty;
        }

        let mut parts = Vec::new();
        for (i, c) in comments.iter().enumerate() {
            if i > 0 {
                let prev = &comments[i - 1];
                let blanks = self
                    .line_index
                    .blank_lines_between(prev.span.end as u32, c.span.start as u32);

                parts.push(if blanks > 0 {
                    Doc::BlankLine
                } else {
                    Doc::HardLine
                });
            }

            parts.push(self.comment_to_doc(c));
        }

        Doc::Concat(parts)
    }

    /// Drain same-line trailing comments after a node whose last byte is at
    /// `end_pos` (exclusive span end). Returns a doc that starts with a space
    /// before the first comment.
    pub(crate) fn drain_trailing_doc(&self, end_pos: usize) -> Doc {
        self.drain_trailing_doc_bounded(end_pos, usize::MAX)
    }

    /// Like [`Self::drain_trailing_doc`] but only captures comments whose
    /// `span.start < max_pos`. Use `max_pos = next_sibling.span.start` in
    /// binary chains and collections to prevent stealing comments that belong
    /// to a later node on the same source line.
    pub(crate) fn drain_trailing_doc_bounded(&self, end_pos: usize, max_pos: usize) -> Doc {
        let end_line = self.line_index.line(end_pos.saturating_sub(1) as u32);
        let comments = self.comment_cursor.borrow_mut().drain_trailing(
            end_pos,
            max_pos,
            end_line,
            &self.line_index,
        );
        if comments.is_empty() {
            return Doc::Empty;
        }

        let mut parts = Vec::new();
        let mut last_line = end_line;
        for c in &comments {
            let comment_start_line = self.line_index.line(c.span.start as u32);
            if comment_start_line == last_line {
                parts.push(self.same_line_comment_to_doc(c));
            } else {
                parts.push(Doc::HardLine);
                parts.push(self.comment_to_doc(c));
            }
            last_line = self.line_index.line(c.span.end.saturating_sub(1) as u32);
        }

        Doc::Concat(parts)
    }

    /// Peek: is there a `//` line comment on the line containing `end_pos`?
    /// Like the unbounded variant but bounded by `max_pos`.
    pub(crate) fn has_trailing_line_comment_bounded(&self, end_pos: usize, max_pos: usize) -> bool {
        let end_line = self.line_index.line(end_pos.saturating_sub(1) as u32);
        self.comment_cursor.borrow().has_line_comment_between(
            end_pos,
            max_pos,
            end_line,
            &self.line_index,
        )
    }

    /// Peek: any undrained comment with start inside `span`?
    pub(crate) fn has_comments_in(&self, span: Span) -> bool {
        self.comment_cursor
            .borrow()
            .has_comment_in(span.start, span.end)
    }

    /// True when any same-line comment immediately after `brace_pos` (up to
    /// `close_pos`) is a `//` line comment — used to decide whether a list
    /// must be forced multi-line.
    pub(crate) fn after_open_has_line_comment(&self, brace_pos: usize, close_pos: usize) -> bool {
        let brace_line = self.line_index.line(brace_pos as u32);
        self.comment_cursor.borrow().has_line_comment_between(
            brace_pos,
            close_pos,
            brace_line,
            &self.line_index,
        )
    }

    /// Drain and render comments that start on the same line as `brace_pos`
    /// (the opening `{`). These are `// C` or `/* C */` immediately after `{`.
    /// Drain same-line comments that appear immediately after an opening
    /// delimiter at `brace_pos`. Only captures comments whose `span.start` is
    /// in `[brace_pos, close_pos)` so that comments outside the delimited
    /// region are not accidentally consumed.
    pub(crate) fn drain_after_open_brace(&self, brace_pos: usize, close_pos: usize) -> Doc {
        let brace_line = self.line_index.line(brace_pos as u32);
        let comments = self.comment_cursor.borrow_mut().drain_trailing(
            brace_pos,
            close_pos,
            brace_line,
            &self.line_index,
        );
        if comments.is_empty() {
            return Doc::Empty;
        }

        let mut parts = Vec::new();
        for c in &comments {
            parts.push(self.same_line_comment_to_doc(c));
        }

        Doc::Concat(parts)
    }

    /// Like [`Self::drain_after_open_brace`] but hugs a leading block comment as `(/* c */ x)`.
    pub(crate) fn drain_after_open_paren(&self, paren_pos: usize, close_pos: usize) -> Doc {
        let paren_line = self.line_index.line(paren_pos as u32);
        let comments = self.comment_cursor.borrow_mut().drain_trailing(
            paren_pos,
            close_pos,
            paren_line,
            &self.line_index,
        );
        if comments.is_empty() {
            return Doc::Empty;
        }

        let mut parts = Vec::new();
        for (i, c) in comments.iter().enumerate() {
            if i == 0 && c.is_block {
                parts.push(self.block_comment_to_doc(c));
            } else {
                parts.push(self.same_line_comment_to_doc(c));
            }
        }

        Doc::Concat(parts)
    }

    /// Drain comments between a name and its `(`, kept in place as `foo /* c */()`. A `//` comment
    /// ends its line, so the `(` continues indented on the next one.
    pub(crate) fn drain_before_open_paren(&self, paren_pos: usize) -> Doc {
        let comments = self.comment_cursor.borrow_mut().drain_before(paren_pos);
        let mut parts = Vec::new();
        for c in &comments {
            parts.push(self.same_line_comment_to_doc(c));

            if !c.is_block {
                parts.push(Doc::Indent(vec![Doc::HardLine]));
            }
        }

        Doc::Concat(parts)
    }

    /// Whether the token at `token_pos`, such as `=>` or an operator, has to start a new line
    /// because a comment between it and the node ending at `node_end` ends a line: a `//`
    /// comment, or one on a line of its own. See [`Self::node_starts_new_line`] for the gap after
    /// a token.
    pub(crate) fn token_starts_new_line(&self, node_end: usize, token_pos: usize) -> bool {
        self.comment_cursor
            .borrow()
            .peek_in(node_end, token_pos)
            .any(|c| !c.is_block || self.line_index.starts_line(c.span.start as u32))
    }

    /// Whether the node at `node_start` can't share a line with the token ending at `token_end`,
    /// such as `return` or `=`, because a comment between them ends on an earlier line than the
    /// node. See [`Self::token_starts_new_line`] for the gap before a token.
    pub(crate) fn node_starts_new_line(&self, token_end: usize, node_start: usize) -> bool {
        let node_line = self.line_index.line(node_start as u32);

        self.comment_cursor
            .borrow()
            .peek_in(token_end, node_start)
            .any(|c| self.line_index.line(c.span.end.saturating_sub(1) as u32) != node_line)
    }

    /// The break in front of a token that starts a new line when its group breaks, made hard when a
    /// comment before the token ends the line anyway.
    pub(crate) fn line_before_token(&self, node_end: usize, token_pos: usize) -> Doc {
        if self.token_starts_new_line(node_end, token_pos) {
            Doc::HardLine
        } else {
            Doc::Line
        }
    }

    /// Drain the comments between a node ending at `node_end` and the token at `token_pos` after
    /// it, such as `=>`, `?` or an operator. Comments on the node's line stay after it and the
    /// rest keep a line of their own.
    pub(crate) fn drain_before_token(&self, node_end: usize, token_pos: usize) -> Doc {
        let mut parts = vec![self.drain_trailing_doc_bounded(node_end, token_pos)];
        for comment in self.comment_cursor.borrow_mut().drain_before(token_pos) {
            parts.push(Doc::HardLine);
            parts.push(self.comment_to_doc(&comment));
        }

        Doc::concat(parts)
    }

    /// The expression `render`s, starting at `expr_start`, after a token ending at `token_end` such
    /// as `return`, `=` or `=>`, including the space between them. When a comment ends the line in
    /// between, the expression goes on an indented line of its own instead of joining the comment.
    pub(crate) fn after_token(
        &self,
        token_end: usize,
        expr_start: usize,
        render: impl FnOnce() -> Doc,
    ) -> Doc {
        if !self.node_starts_new_line(token_end, expr_start) {
            return Doc::concat(vec![Doc::text(" "), render()]);
        }

        let same_line = self.drain_trailing_doc_bounded(token_end, expr_start);

        Doc::concat(vec![same_line, Doc::Indent(vec![Doc::HardLine, render()])])
    }

    /// ` <token> <expr>` after a node ending at `node_end`, with comments kept on the side of the
    /// token they were written on. A comment that ends the line before the token moves the token
    /// to an indented line of its own.
    pub(crate) fn token_then_expr(
        &self,
        node_end: usize,
        token: &str,
        token_pos: usize,
        expr: &Expr,
    ) -> Doc {
        let breaks = self.token_starts_new_line(node_end, token_pos);
        let before = self.drain_before_token(node_end, token_pos);
        let after = self.after_token(token_pos + token.len(), expr.span().start, || {
            self.expr_with_leading(expr)
        });

        if breaks {
            return Doc::concat(vec![
                before,
                Doc::Indent(vec![Doc::HardLine, Doc::text(token), after]),
            ]);
        }

        Doc::concat(vec![before, Doc::text(format!(" {token}")), after])
    }

    /// Effective start of `span` — the position of its first leading comment
    /// (if one exists just before the span) or `span.start` itself.
    pub(crate) fn effective_start(&self, span: Span) -> usize {
        self.comment_cursor
            .borrow()
            .peek_before(span.start)
            .map(|c| c.span.start)
            .unwrap_or(span.start)
    }

    /// Effective end after draining trailing comments — max of `span.end` and
    /// the end of the last drained comment.
    pub(crate) fn effective_end(&self, span: Span) -> usize {
        self.comment_cursor.borrow().last_end().max(span.end)
    }

    /// Render a comment as a [`Doc`].
    pub(crate) fn comment_to_doc(&self, c: &CommentStmt) -> Doc {
        if c.is_block {
            self.block_comment_to_doc(c)
        } else {
            Doc::line_comment(format!("//{}", c.text.trim_end()))
        }
    }

    /// Render a comment that follows code on the same line, including the space separating them.
    /// For a `//` comment the space is part of the comment doc, so neither counts toward whether
    /// the code before it fits.
    fn same_line_comment_to_doc(&self, c: &CommentStmt) -> Doc {
        if c.is_block {
            return Doc::concat(vec![Doc::text(" "), self.block_comment_to_doc(c)]);
        }

        Doc::line_comment(format!(" //{}", c.text.trim_end()))
    }

    /// Render a `/* … */` comment as written, apart from indentation. Every line moves by as
    /// much as the line the comment starts on, so banners, ` *` gutters and indented content keep
    /// their layout relative to the code they belong to.
    fn block_comment_to_doc(&self, comment: &CommentStmt) -> Doc {
        let text = &comment.text;
        let Some((first_line, rest)) = text.split_once('\n') else {
            return Doc::text(format!("/*{text}*/"));
        };

        let start_line = self.line_index.line(comment.span.start as u32);
        let source_indent = self.line_index.indent(start_line) as usize;
        let dedent = |line: &str| -> String {
            let indent_width = line.len() - line.trim_start_matches([' ', '\t']).len();

            line[indent_width.min(source_indent)..].to_string()
        };

        // Drop what trails each line, including the `\r` of CRLF line endings, but keep the
        // whitespace before an inline `*/`, as in `line 2 */`.
        let mut lines: Vec<&str> = rest.split('\n').collect();
        let last_line = lines.pop().unwrap_or_default();
        let mut parts = vec![Doc::text(format!("/*{}", first_line.trim_end()))];
        for line in lines {
            let line = dedent(line.trim_end());

            // A raw newline keeps blank lines free of the indentation a hard line would add.
            if line.is_empty() {
                parts.push(Doc::text("\n"));
            } else {
                parts.push(Doc::HardLine);
                parts.push(Doc::text(line));
            }
        }

        parts.push(Doc::HardLine);
        parts.push(Doc::text(format!("{}*/", dedent(last_line))));

        Doc::concat(parts)
    }

    /// Drain any comments that appear between the last expression/token and
    /// the closing `;`, then push the semicolon. Preserves block comments like
    /// `expr /* c */ ;` while keeping the common `expr;` case minimal.
    pub(crate) fn push_before_semi(&self, parts: &mut Vec<Doc>, semi_pos: usize) {
        let before = self.drain_leading_doc(semi_pos);

        match before {
            Doc::Empty => parts.push(Doc::text(";")),
            d => {
                parts.push(Doc::text(" "));
                parts.push(d);
                parts.push(Doc::text(";"));
            }
        }
    }

    pub(crate) fn gap_between_positions(&self, prev_end: usize, next_start: usize) -> Doc {
        if prev_end == 0 || next_start <= prev_end {
            return Doc::HardLine;
        }

        let blanks = self
            .line_index
            .blank_lines_between(prev_end as u32, next_start as u32);

        if blanks > 0 {
            Doc::BlankLine
        } else {
            Doc::HardLine
        }
    }
}
