pub mod doc;
mod member_chain;
mod operators;

use doc::{Doc, display_width, render};
use monkey_c_parser::ast::{
    AnnotationEntry, ArrayExpr, Ast, BinaryOperator, Binding, BlockStmt, CallArg, CallExpr,
    CaseLabel, CommentStmt, ConstDecl, DictExpr, DictTypeEntry, DictTypeKey, DoubleLit, ElseBranch,
    EnumDecl, EnumVariant, Expr, FloatLit, ForInit, FunctionDecl, IfStmt, InterfaceMember,
    LiteralValue, Modifiers, Parameter, Parens, Separated, Span, Spanned, Stmt, SwitchStmt,
    TryStmt, Type, TypeKind, UnaryOperator, VarDecl, Visibility,
};
use monkey_c_parser::comments::CommentCursor;
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::line_index::LineIndex;
use monkey_c_parser::parser::ParseOutput;
use monkey_c_parser::token;

use std::cell::RefCell;
use std::collections::HashMap;

/// An item in a delimited list (array entry, dict pair, call arg, etc.) along
/// with any comments that trail it inside the bracketed list. Used by
/// [`Formatter::format_list`].
struct ListItem {
    content: Doc,
    before_separator: Doc,
    trailing: Doc,
    /// True when the trailing comment is a `//` line comment, which forces
    /// the whole list to render multi-line (a line comment consumes the rest
    /// of the source line, so args after it would be commented out in flat mode).
    trailing_is_line_comment: bool,
}

/// Formats a Monkey C AST back into source text.
///
/// Construct with [`Formatter::new`], optionally configure with builder
/// methods, then call [`Formatter::format`].
///
/// The formatter uses the Wadler-Lindig algorithm (see [`doc`]) to make
/// line-breaking decisions globally rather than per-node, so a dict or array
/// that fits on one line is kept there automatically.
pub struct Formatter {
    line_index: LineIndex,
    /// Maximum line width before a [`doc::Doc::Group`] is broken.
    line_width: usize,
    /// When `true`, runs of related entries are rendered with their separator
    /// operators column-aligned, as are trailing comments on consecutive lines.
    align_pairs: bool,
    /// Positional drain cursor over all source comments. Advanced forward as
    /// the formatter builds the Doc tree; each comment is emitted exactly once
    /// at the first output position that follows its source location.
    comment_cursor: RefCell<CommentCursor>,
}

impl Formatter {
    /// Create a formatter for `source`.
    pub fn new(source: impl AsRef<str>) -> Self {
        let source = source.as_ref();

        Self {
            line_index: LineIndex::new(source),
            line_width: 100,
            align_pairs: false,
            comment_cursor: RefCell::new(CommentCursor::default()),
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    /// Enable column-aligned separators and trailing comments across related entries (opt-in).
    pub fn with_alignment(mut self, align_pairs: bool) -> Self {
        self.align_pairs = align_pairs;
        self
    }

    /// Format a parsed file and return the result as a `String`.
    pub fn format(&self, output: &ParseOutput) -> String {
        *self.comment_cursor.borrow_mut() = CommentCursor::new(&output.comments);

        let doc = self.ast_to_doc(&output.ast);
        let rendered = render(&doc, self.line_width);
        let mut formatted = if self.align_pairs {
            align_trailing_comments(&rendered)
        } else {
            rendered
        };

        if !formatted.ends_with('\n') {
            formatted.push('\n');
        }

        formatted
    }

    /// "No comment left behind": source comments that did **not** survive into
    /// `rendered`. Re-lexes `rendered` and matches each source comment against
    /// the output comments by kind plus whitespace-normalised content.
    pub fn lost_comments(output: &ParseOutput, rendered: &str) -> Vec<CommentStmt> {
        fn normalize(text: &str) -> String {
            text.split_whitespace().collect::<Vec<_>>().join(" ")
        }

        let mut present: HashMap<(bool, String), usize> = HashMap::new();
        let mut lexer = Lexer::new(rendered);
        loop {
            let (_start, tok, _end) = lexer.next_token();
            match tok {
                token::Type::Eof => break,
                token::Type::Comment(t) => *present.entry((false, normalize(&t))).or_default() += 1,
                token::Type::BlockComment(t) => {
                    *present.entry((true, normalize(&t))).or_default() += 1;
                }
                _ => {}
            }
        }

        let mut lost = Vec::new();
        for c in &output.comments.comments {
            let key = (c.is_block, normalize(&c.text));
            match present.get_mut(&key) {
                Some(n) if *n > 0 => *n -= 1,
                _ => lost.push(c.clone()),
            }
        }

        lost
    }

    // --- Positional drain helpers ---

    /// Drain comments with `span.start < pos` and render them as leading
    /// content (each on its own line, or inline for a same-line block comment).
    /// Drain any comments before the spanned node, then prepend them to its text doc.
    fn at(&self, spanned: &Spanned<String>) -> Doc {
        let doc = Doc::text(&spanned.node);
        let leading = self.drain_leading_doc(spanned.start());

        match leading {
            Doc::Empty => doc,
            d => Doc::Concat(vec![d, doc]),
        }
    }

    fn drain_leading_doc(&self, pos: usize) -> Doc {
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
    fn drain_dangling_comments_doc(&self, pos: usize) -> Doc {
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
    fn drain_trailing_doc(&self, end_pos: usize) -> Doc {
        self.drain_trailing_doc_bounded(end_pos, usize::MAX)
    }

    /// Like [`drain_trailing_doc`] but only captures comments whose
    /// `span.start < max_pos`. Use `max_pos = next_sibling.span.start` in
    /// binary chains and collections to prevent stealing comments that belong
    /// to a later node on the same source line.
    fn drain_trailing_doc_bounded(&self, end_pos: usize, max_pos: usize) -> Doc {
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
    fn has_trailing_line_comment_bounded(&self, end_pos: usize, max_pos: usize) -> bool {
        let end_line = self.line_index.line(end_pos.saturating_sub(1) as u32);
        self.comment_cursor.borrow().has_line_comment_between(
            end_pos,
            max_pos,
            end_line,
            &self.line_index,
        )
    }

    /// Peek: any undrained comment with start inside `span`?
    fn has_comments_in(&self, span: Span) -> bool {
        self.comment_cursor
            .borrow()
            .has_comment_in(span.start, span.end)
    }

    /// True when any same-line comment immediately after `brace_pos` (up to
    /// `close_pos`) is a `//` line comment — used to decide whether a list
    /// must be forced multi-line.
    fn after_open_has_line_comment(&self, brace_pos: usize, close_pos: usize) -> bool {
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
    fn drain_after_open_brace(&self, brace_pos: usize, close_pos: usize) -> Doc {
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
    fn drain_after_open_paren(&self, paren_pos: usize, close_pos: usize) -> Doc {
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
    fn drain_before_open_paren(&self, paren_pos: usize) -> Doc {
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

    /// Whether a comment between a node ending at `node_end` and the token at `token_pos` after it
    /// ends a line, either as a `//` comment or by sitting on a line of its own. The token then
    /// has to start a new line.
    fn comment_breaks_line_before(&self, node_end: usize, token_pos: usize) -> bool {
        self.comment_cursor
            .borrow()
            .peek_in(node_end, token_pos)
            .any(|c| !c.is_block || self.line_index.starts_line(c.span.start as u32))
    }

    /// The break in front of a token that starts a new line when its group breaks, made hard when a
    /// comment before the token ends the line anyway.
    fn line_before_token(&self, node_end: usize, token_pos: usize) -> Doc {
        if self.comment_breaks_line_before(node_end, token_pos) {
            Doc::HardLine
        } else {
            Doc::Line
        }
    }

    /// Drain the comments between a node ending at `node_end` and the token at `token_pos` after
    /// it, such as `=>`, `?` or an operator. Comments on the node's line stay after it and the
    /// rest keep a line of their own.
    fn drain_before_token(&self, node_end: usize, token_pos: usize) -> Doc {
        let mut parts = vec![self.drain_trailing_doc_bounded(node_end, token_pos)];
        for comment in self.comment_cursor.borrow_mut().drain_before(token_pos) {
            parts.push(Doc::HardLine);
            parts.push(self.comment_to_doc(&comment));
        }

        Doc::concat(parts)
    }

    /// Whether a comment between a token ending at `token_end` and the node at `node_start` after
    /// it ends on an earlier line than the node, so the two can't share a line.
    fn comment_ends_line_before(&self, token_end: usize, node_start: usize) -> bool {
        let node_line = self.line_index.line(node_start as u32);

        self.comment_cursor
            .borrow()
            .peek_in(token_end, node_start)
            .any(|c| self.line_index.line(c.span.end.saturating_sub(1) as u32) != node_line)
    }

    /// The expression `render`s, starting at `expr_start`, after a token ending at `token_end` such
    /// as `return`, `=` or `=>`, including the space between them. When a comment ends the line in
    /// between, the expression goes on an indented line of its own instead of joining the comment.
    fn after_token(
        &self,
        token_end: usize,
        expr_start: usize,
        render: impl FnOnce() -> Doc,
    ) -> Doc {
        if !self.comment_ends_line_before(token_end, expr_start) {
            return Doc::concat(vec![Doc::text(" "), render()]);
        }

        let same_line = self.drain_trailing_doc_bounded(token_end, expr_start);

        Doc::concat(vec![same_line, Doc::Indent(vec![Doc::HardLine, render()])])
    }

    /// ` <token> <expr>` after a node ending at `node_end`, with comments kept on the side of the
    /// token they were written on. A comment that ends the line before the token moves the token
    /// to an indented line of its own.
    fn token_then_expr(&self, node_end: usize, token: &str, token_pos: usize, expr: &Expr) -> Doc {
        let breaks = self.comment_breaks_line_before(node_end, token_pos);
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

    /// A list entry and the comments around the `,` after it at `comma`, each kept on the side of
    /// the comma it was written on. A comment that ends the line before the comma leaves the comma
    /// to start the next line, as in the source. A block comment in front of the next entry on the
    /// same line is left for that entry.
    fn list_item(
        &self,
        content: Doc,
        item_end: usize,
        comma: Option<usize>,
        next_item_start: Option<usize>,
        list_end: usize,
    ) -> ListItem {
        let next_start = next_item_start.unwrap_or(list_end);
        let trailing_is_line_comment = self.has_trailing_line_comment_bounded(item_end, next_start);
        let Some(comma) = comma else {
            return ListItem {
                content,
                before_separator: Doc::Empty,
                trailing: self.drain_trailing_doc_bounded(item_end, next_start),
                trailing_is_line_comment,
            };
        };

        let comma_starts_line = self.comment_breaks_line_before(item_end, comma);
        let mut before_separator = self.drain_before_token(item_end, comma);
        if comma_starts_line {
            before_separator = Doc::concat(vec![before_separator, Doc::HardLine]);
        }

        let comma_line = self.line_index.line(comma as u32);
        let next_on_comma_line =
            next_item_start.is_some_and(|start| self.line_index.line(start as u32) == comma_line);
        let trailing = if next_on_comma_line {
            Doc::Empty
        } else {
            self.drain_trailing_doc_bounded(comma + 1, next_start)
        };

        ListItem {
            content,
            before_separator,
            trailing,
            trailing_is_line_comment: trailing_is_line_comment || comma_starts_line,
        }
    }

    /// Effective start of `span` — the position of its first leading comment
    /// (if one exists just before the span) or `span.start` itself.
    fn effective_start(&self, span: Span) -> usize {
        self.comment_cursor
            .borrow()
            .peek_before(span.start)
            .map(|c| c.span.start)
            .unwrap_or(span.start)
    }

    /// Effective end after draining trailing comments — max of `span.end` and
    /// the end of the last drained comment.
    fn effective_end(&self, span: Span) -> usize {
        self.comment_cursor.borrow().last_end().max(span.end)
    }

    /// Render a comment as a [`Doc`].
    fn comment_to_doc(&self, c: &CommentStmt) -> Doc {
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

    fn ast_to_doc(&self, ast: &Ast) -> Doc {
        match ast {
            Ast::Document(nodes, span) => self.decls_to_doc(nodes, *span),
            Ast::Import(decl) => {
                let mut parts = vec![Doc::text("import "), self.at(&decl.name)];
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Using(decl) => {
                let mut parts = vec![Doc::text("using "), self.at(&decl.name)];
                if let Some(alias) = &decl.alias {
                    parts.push(Doc::text(" "));
                    parts.push(self.drain_leading_doc(decl.as_kw_start.unwrap_or(decl.span.end)));
                    parts.push(Doc::text("as "));
                    parts.push(self.at(alias));
                }
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Typedef(decl) => {
                let mut parts = vec![Doc::text("typedef ")];
                parts.push(self.at(&decl.name));
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(decl.as_kw_start));
                parts.push(Doc::text("as "));
                parts.push(self.type_to_doc(&decl.type_));
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Module(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "module");
                header_parts.push(self.at(&decl.name));
                header_parts.push(Doc::text(" "));
                let header = Doc::concat(header_parts);
                let before_brace = self.drain_leading_doc(decl.brace_start);
                let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
                let inner = self.decls_to_doc(&decl.body, decl.span);

                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![
                        header,
                        before_brace,
                        Doc::text("{"),
                        after_open,
                        Doc::text("}"),
                    ]);
                }

                Doc::concat(vec![
                    header,
                    before_brace,
                    Doc::text("{"),
                    after_open,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Class(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "class");
                header_parts.push(self.at(&decl.name));
                if let Some(extends) = &decl.extends {
                    header_parts.push(Doc::text(" "));
                    header_parts.push(
                        self.drain_leading_doc(decl.extends_kw_start.unwrap_or(decl.brace_start)),
                    );
                    header_parts.push(Doc::text("extends "));
                    header_parts.push(self.at(extends));
                }
                header_parts.push(Doc::text(" "));
                let header = Doc::Concat(header_parts);
                let before_brace = self.drain_leading_doc(decl.brace_start);
                let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
                let inner = self.decls_to_doc(&decl.body, decl.span);

                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![
                        header,
                        before_brace,
                        Doc::text("{"),
                        after_open,
                        Doc::text("}"),
                    ]);
                }

                Doc::concat(vec![
                    header,
                    before_brace,
                    Doc::text("{"),
                    after_open,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Function(decl) => self.function_to_doc(decl),
            Ast::Enum(decl) => self.enum_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Annotation(entries, span) => self.annotation_to_doc(entries, *span),
            Ast::Eof => Doc::Empty,
        }
    }

    /// A `//` comment runs to the end of the line, so an annotation group holding one is broken
    /// with each entry on its own line. Otherwise the entry after it would be commented out.
    fn annotation_to_doc(&self, entries: &[AnnotationEntry], span: Span) -> Doc {
        let inner_start = span.start + 1;
        let inner_end = span.end.saturating_sub(1);
        let multiline = self
            .comment_cursor
            .borrow()
            .has_line_comment_in(inner_start, inner_end);
        let separator = if multiline {
            Doc::HardLine
        } else {
            Doc::text(" ")
        };

        if entries.is_empty() {
            let comments = self.comment_cursor.borrow_mut().drain_before(inner_end);
            if comments.is_empty() {
                return Doc::text("()");
            }

            let mut inner = Vec::new();
            for (i, comment) in comments.iter().enumerate() {
                if i > 0 {
                    inner.push(separator.clone());
                }

                inner.push(self.comment_to_doc(comment));
            }

            return wrap_annotation(inner, multiline);
        }

        let mut inner = Vec::new();
        for (i, entry) in entries.iter().enumerate() {
            let next_entry = entries.get(i + 1);

            if i > 0 {
                inner.push(separator.clone());
            }

            inner.push(self.drain_leading_doc(entry.span.start));
            inner.push(Doc::text(format!(":{}", entry.name)));

            if !entry.args.is_empty() {
                inner.push(Doc::text("("));
                for (j, arg) in entry.args.iter().enumerate() {
                    if j > 0 {
                        inner.push(Doc::text(", "));
                    }

                    inner.push(self.expr_with_leading(arg));
                }

                inner.push(Doc::text(")"));
            }

            if next_entry.is_some_and(|next| next.preceded_by_comma) {
                inner.push(Doc::text(","));
            }

            // Bounded so comments belonging to the next entry, or to the annotated declaration
            // after the closing `)`, are not pulled in here.
            let max_pos = next_entry.map_or(span.end, |next| next.span.start);
            inner.push(self.drain_trailing_doc_bounded(entry.span.end, max_pos));
        }

        wrap_annotation(inner, multiline)
    }

    /// Drain any comments that appear between the last expression/token and
    /// the closing `;`, then push the semicolon. Preserves block comments like
    /// `expr /* c */ ;` while keeping the common `expr;` case minimal.
    fn push_before_semi(&self, parts: &mut Vec<Doc>, semi_pos: usize) {
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

    /// Render a sequence of declarations interleaved with standalone comments,
    /// preserving blank lines between adjacent items.
    fn decls_to_doc(&self, decls: &[Ast], container: Span) -> Doc {
        let has_content = !decls.is_empty()
            || self
                .comment_cursor
                .borrow()
                .has_comment_in(container.start, container.end);
        if !has_content {
            return Doc::Empty;
        }

        let mut docs = Vec::new();
        let mut prev_end: Option<usize> = None;
        let mut prev_is_block_decl = false;

        for (idx, decl) in decls.iter().enumerate() {
            let Some(decl_span) = decl.span() else {
                continue;
            };
            let decl_span = *decl_span;

            let eff_start = self.effective_start(decl_span);
            let is_block_decl = matches!(decl, Ast::Function(_) | Ast::Class(_) | Ast::Module(_));

            if let Some(pe) = prev_end {
                let gap = self.gap_between_positions(pe, eff_start);
                docs.push(if prev_is_block_decl && matches!(gap, Doc::HardLine) {
                    Doc::BlankLine
                } else {
                    gap
                });
            }

            // Bound the trailing drain at the next decl's start so that same-line
            // comments following a decl (e.g. after an annotation or var with `;`)
            // are not stolen by the current decl's trailing drain.
            let next_decl_start = decls[idx + 1..]
                .iter()
                .find_map(|d| d.span().map(|s| s.start))
                .unwrap_or(container.end);

            docs.push(self.drain_leading_doc(decl_span.start));
            docs.push(self.ast_to_doc(decl));
            docs.push(self.drain_trailing_doc_bounded(decl_span.end, next_decl_start));

            prev_end = Some(self.effective_end(decl_span));
            prev_is_block_decl = is_block_decl;
        }

        // Drain any remaining comments inside the container (after last decl).
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(container.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            if let Some(pe) = prev_end {
                docs.push(self.gap_between_positions(pe, start));
            }

            docs.push(self.drain_dangling_comments_doc(container.end));
        }

        Doc::Concat(docs)
    }

    fn enum_to_doc(&self, decl: &EnumDecl) -> Doc {
        let mut prefix_parts = self.decl_keyword(&decl.modifiers, "enum");
        if let Some(name) = &decl.name {
            prefix_parts.push(self.at(name));
            prefix_parts.push(Doc::text(" "));
        }

        let prefix = Doc::concat(prefix_parts);

        if decl.variants.is_empty() {
            let before_brace = self.drain_leading_doc(decl.brace_start);
            let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
            let remaining_start = self
                .comment_cursor
                .borrow()
                .peek_before(decl.span.end)
                .map(|c| c.span.start);

            if remaining_start.is_none()
                && matches!(before_brace, Doc::Empty)
                && matches!(after_open, Doc::Empty)
            {
                return Doc::concat(vec![prefix.clone(), Doc::text("{"), Doc::text("}")]);
            }

            let mut inner = Vec::new();
            if remaining_start.is_some() {
                inner.push(self.drain_dangling_comments_doc(decl.span.end));
            }

            if inner.is_empty()
                && matches!(before_brace, Doc::Empty)
                && matches!(after_open, Doc::Empty)
            {
                return Doc::concat(vec![prefix.clone(), Doc::text("{"), Doc::text("}")]);
            }

            return Doc::concat(vec![
                prefix.clone(),
                before_brace,
                Doc::text("{"),
                after_open,
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text("}"),
            ]);
        }

        // Bounded by the first variant so comments after it on a one-line enum stay with it.
        let before_brace = self.drain_leading_doc(decl.brace_start);
        let after_open = self.drain_after_open_brace(decl.brace_start, decl.variants[0].span.start);
        let header = Doc::concat(vec![
            prefix.clone(),
            before_brace,
            Doc::text("{"),
            after_open,
        ]);

        let last_idx = decl.variants.len() - 1;
        let mut inner = Vec::new();
        let mut prev_end: Option<usize> = None;

        let push_gap = |inner: &mut Vec<Doc>, prev_end: Option<usize>, next_start: usize| {
            let Some(pe) = prev_end else { return };
            inner.push(self.gap_between_positions(pe, next_start));
        };

        let name_pads = if self.align_pairs {
            enum_variant_name_pads(&decl.variants)
        } else {
            vec![0; decl.variants.len()]
        };

        for (i, v) in decl.variants.iter().enumerate() {
            let eff_start = self.effective_start(v.span);
            push_gap(&mut inner, prev_end, eff_start);

            let mut parts = Vec::new();
            parts.push(self.drain_leading_doc(v.span.start));
            parts.push(Doc::text(&v.name));

            if let Some(value) = &v.value {
                let pad = name_pads[i].saturating_sub(display_width(&v.name));
                if pad > 0 {
                    parts.push(Doc::text(" ".repeat(pad)));
                }
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(v.assign_kw_start.unwrap_or(value.span().start)));
                parts.push(Doc::text("= "));
                parts.push(self.expr_with_leading(value));
            }

            // Bounded by the closing `}` so a comment after it on a one-line enum stays there.
            let item = self.list_item(
                Doc::Concat(parts),
                v.span.end,
                decl.variants.comma_after(i),
                decl.variants.get(i + 1).map(|next| next.span.start),
                decl.span.end,
            );
            let mut parts = vec![item.content, item.before_separator];
            if i != last_idx || decl.variants.has_trailing_comma() {
                parts.push(Doc::text(","));
            }

            parts.push(item.trailing);
            inner.push(Doc::Concat(parts));
            prev_end = Some(self.effective_end(v.span));
        }

        // Drain comments after last variant before `}`.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(decl.span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            push_gap(&mut inner, prev_end, start);
            inner.push(self.drain_dangling_comments_doc(decl.span.end));
        }

        Doc::concat(vec![
            header,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn function_to_doc(&self, decl: &FunctionDecl) -> Doc {
        let mut parts = self.decl_keyword(&decl.modifiers, "function");
        parts.push(self.at(&decl.name));

        parts.push(self.parameter_list_to_doc(&decl.parameters));

        if let Some(ret) = &decl.returns {
            parts.push(Doc::text(" "));
            parts.push(self.drain_leading_doc(decl.as_kw_start.unwrap_or(ret.span.start)));
            parts.push(Doc::text("as "));
            parts.push(self.type_to_doc(ret));
        }

        match &decl.body {
            None => parts.push(Doc::text(";")),
            Some(body) => {
                parts.push(Doc::text(" "));
                parts.push(self.block_body_to_doc(body));
            }
        }

        Doc::Concat(parts)
    }

    /// The `(…)` of a function, method type or interface method.
    fn parameter_list_to_doc(&self, parameters: &Parens<Separated<Parameter>>) -> Doc {
        let items = parameters
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let mut arg_parts = Vec::new();
                arg_parts.push(self.drain_leading_doc(arg.span.start));
                arg_parts.push(Doc::text(&arg.name.node));
                if let Some(ty) = &arg.type_ {
                    arg_parts.push(Doc::text(" "));
                    arg_parts
                        .push(self.drain_leading_doc(arg.as_kw_start.unwrap_or(ty.span.start)));
                    arg_parts.push(Doc::text("as "));
                    arg_parts.push(self.type_to_doc(ty));
                }

                // arg.span.end points to the start of the following `,` or `)`
                // token (the parser sets it to current_token_start after
                // parse_type, skipping past any same-line comment). Use the
                // type's own span end so trailing comments are captured on the
                // correct source line.
                let parameter_end = arg.type_.as_ref().map_or(arg.span.end, |t| t.span.end);

                self.list_item(
                    Doc::Concat(arg_parts),
                    parameter_end,
                    parameters.comma_after(i),
                    parameters.get(i + 1).map(|next| next.span.start),
                    parameters.close,
                )
            })
            .collect();

        self.format_list(
            "(",
            ")",
            items,
            self.drain_dangling_comments_doc(parameters.close),
            parameters.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }

    fn var_stmt_to_doc(&self, var_decl: &VarDecl) -> Doc {
        if var_decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                &var_decl.modifiers,
                "var",
                &var_decl.bindings,
                var_decl.semi_pos,
            );
        }

        let mut parts = vec![self.var_decl_to_doc(var_decl)];
        self.push_before_semi(&mut parts, var_decl.semi_pos);

        Doc::Concat(parts)
    }

    fn const_decl_to_doc(&self, decl: &ConstDecl) -> Doc {
        if decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                &decl.modifiers,
                "const",
                &decl.bindings,
                decl.semi_pos,
            );
        }

        let mut parts = self.decl_keyword(&decl.modifiers, "const");
        self.push_bindings(&mut parts, &decl.bindings);
        self.push_before_semi(&mut parts, decl.semi_pos);

        Doc::Concat(parts)
    }

    fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts = self.decl_keyword(&var.modifiers, "var");
        self.push_bindings(&mut parts, &var.bindings);

        Doc::Concat(parts)
    }

    fn wrapped_bindings_decl(
        &self,
        modifiers: &Modifiers,
        keyword: &str,
        bindings: &Separated<Binding>,
        semi_pos: usize,
    ) -> Doc {
        let mut parts = self.modifiers_to_doc(modifiers);

        let mut indented = vec![Doc::Line];
        for (i, binding) in bindings.iter().enumerate() {
            let content = self.binding_to_doc(binding);
            let next_start = bindings.get(i + 1).map(|next| next.span.start);
            // The last binding leaves its comments to the `;`.
            let item = self.list_item(
                content,
                binding.span.end,
                bindings.comma_after(i),
                next_start,
                binding.span.end,
            );
            indented.push(item.content);
            indented.push(item.before_separator);

            if next_start.is_none() {
                indented.push(item.trailing);
                continue;
            }

            indented.push(Doc::text(","));
            indented.push(item.trailing);
            indented.push(if item.trailing_is_line_comment {
                Doc::HardLine
            } else {
                Doc::Line
            });
        }

        // The `;` sits inside the group so a declaration that only overflows
        // by its terminator still breaks.
        let mut group = vec![Doc::text(keyword.to_string()), Doc::Indent(indented)];
        self.push_before_semi(&mut group, semi_pos);
        parts.push(Doc::group(group));

        Doc::Concat(parts)
    }

    fn decl_keyword(&self, modifiers: &Modifiers, keyword: &str) -> Vec<Doc> {
        let mut parts = self.modifiers_to_doc(modifiers);
        parts.push(Doc::text(format!("{keyword} ")));

        parts
    }

    fn push_bindings(&self, parts: &mut Vec<Doc>, bindings: &Separated<Binding>) {
        for (i, binding) in bindings.iter().enumerate() {
            let content = self.binding_to_doc(binding);
            let next_start = bindings.get(i + 1).map(|next| next.span.start);

            // The last binding leaves its comments to what follows it, such as the `;`.
            let item = self.list_item(
                content,
                binding.span.end,
                bindings.comma_after(i),
                next_start,
                binding.span.end,
            );
            parts.push(item.content);
            parts.push(item.before_separator);

            if next_start.is_none() {
                continue;
            }

            parts.push(Doc::text(","));
            parts.push(item.trailing);
            parts.push(if item.trailing_is_line_comment {
                Doc::HardLine
            } else {
                Doc::text(" ")
            });
        }
    }

    fn binding_to_doc(&self, b: &Binding) -> Doc {
        let mut parts = vec![
            self.drain_leading_doc(b.name.start()),
            Doc::text(&b.name.node),
        ];

        if let Some(ty) = &b.type_ {
            parts.push(Doc::text(" "));
            parts.push(self.drain_leading_doc(b.as_kw_start.unwrap_or(ty.span.start)));
            parts.push(Doc::text("as "));
            parts.push(self.type_to_doc(ty));
        }

        if let Some(init) = &b.initializer {
            let before_assign = b.type_.as_ref().map_or(b.name.span.end, |ty| ty.span.end);
            let assign_pos = b.assign_kw_start.unwrap_or(init.span().start);
            parts.push(self.token_then_expr(before_assign, "=", assign_pos, init));
        }

        Doc::Concat(parts)
    }

    fn modifiers_to_doc(&self, modifiers: &Modifiers) -> Vec<Doc> {
        let mut keywords = Vec::new();
        if let Some(visibility) = &modifiers.visibility {
            let keyword = match visibility.node {
                Visibility::Private => "private ",
                Visibility::Protected => "protected ",
                Visibility::Hidden => "hidden ",
                Visibility::Public => "public ",
            };
            keywords.push((visibility.start(), keyword));
        }

        if let Some(static_kw_start) = modifiers.static_kw_start {
            keywords.push((static_kw_start, "static "));
        }

        keywords.sort_by_key(|(position, _)| *position);

        keywords
            .into_iter()
            .flat_map(|(position, keyword)| [self.drain_leading_doc(position), Doc::text(keyword)])
            .collect()
    }

    fn type_to_doc(&self, ty: &Type) -> Doc {
        let leading = self.drain_leading_doc(ty.span.start);
        let inner = self.type_inner_to_doc(ty);

        if matches!(leading, Doc::Empty) {
            inner
        } else {
            Doc::concat(vec![leading, inner])
        }
    }

    fn type_inner_to_doc(&self, ty: &Type) -> Doc {
        let suffix = if ty.optional { "?" } else { "" };
        let base = match &ty.kind {
            TypeKind::Named {
                ident,
                generic_params,
            } => {
                if generic_params.is_empty() {
                    Doc::text(format!("{ident}{suffix}"))
                } else {
                    let params: Vec<Doc> = generic_params
                        .iter()
                        .enumerate()
                        .flat_map(|(i, p)| {
                            if i > 0 {
                                vec![Doc::text(", "), self.type_to_doc(p)]
                            } else {
                                vec![self.type_to_doc(p)]
                            }
                        })
                        .collect();

                    Doc::concat(vec![
                        Doc::text(format!("{ident}<")),
                        Doc::Concat(params),
                        Doc::text(format!(">{suffix}")),
                    ])
                }
            }
            TypeKind::Dict { entries, body_span } => {
                self.inline_dict_type_to_doc(entries, *body_span, suffix)
            }
            TypeKind::Interface { members, body_span } => {
                self.interface_type_to_doc(members, *body_span, suffix)
            }
            TypeKind::Tuple { elements } => {
                let mut parts = vec![Doc::text("[")];
                for (i, el) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }

                    parts.push(self.type_to_doc(el));
                }

                parts.push(Doc::text(format!("]{suffix}")));

                Doc::Concat(parts)
            }
            TypeKind::Method {
                name,
                args,
                returns,
            } => {
                let mut parts = vec![Doc::text(name), self.parameter_list_to_doc(args)];

                if let Some(ret) = returns {
                    parts.push(Doc::text(" as "));
                    parts.push(self.type_to_doc(ret));
                }

                if !suffix.is_empty() {
                    parts.push(Doc::text(suffix.to_string()));
                }

                Doc::Concat(parts)
            }
            TypeKind::Group(group) => Doc::concat(vec![
                Doc::text("("),
                self.type_to_doc(&group.inner),
                Doc::text(format!("){suffix}")),
            ]),
        };

        if ty.alternatives.is_empty() {
            return base;
        }

        let mut parts = vec![base];
        for alternative in &ty.alternatives {
            parts.push(Doc::text(format!(" {} ", alternative.separator.as_str())));
            parts.push(self.type_to_doc(&alternative.type_));
        }

        Doc::Concat(parts)
    }

    fn inline_dict_type_to_doc(
        &self,
        entries: &Separated<DictTypeEntry>,
        body_span: Span,
        suffix: &str,
    ) -> Doc {
        let close = body_span.end - 1;
        let items = entries
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let leading = self.drain_leading_doc(entry.span.start);
                let key = match &entry.key {
                    DictTypeKey::Symbol(s) => format!(":{s}"),
                    DictTypeKey::String(s) => format!("\"{s}\""),
                };
                let content = Doc::concat(vec![
                    leading,
                    Doc::text(key),
                    Doc::text(" as "),
                    self.type_to_doc(&entry.value_type),
                ]);
                let next_entry_start = entries.get(i + 1).map(|next| next.span.start);

                self.list_item(
                    content,
                    entry.span.end,
                    entries.comma_after(i),
                    next_entry_start,
                    close,
                )
            })
            .collect();

        self.format_list(
            "{",
            &format!("}}{suffix}"),
            items,
            self.drain_dangling_comments_doc(close),
            entries.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }

    fn interface_type_to_doc(
        &self,
        members: &[InterfaceMember],
        body_span: Span,
        suffix: &str,
    ) -> Doc {
        let has_content = !members.is_empty()
            || self
                .comment_cursor
                .borrow()
                .has_comment_in(body_span.start + 1, body_span.end);

        if !has_content {
            return Doc::text(format!("interface {{}}{suffix}"));
        }

        // Place same-line comment directly after `{`, not inside the indent.
        let after_open = self.drain_after_open_brace(body_span.start, body_span.end);
        let mut inner = Vec::new();

        let mut prev_end: Option<usize> = None;

        for member in members {
            let member_span = match member {
                InterfaceMember::Function(m) => m.span,
                InterfaceMember::Variable(v) => v.span,
            };

            let eff_start = self.effective_start(member_span);
            if let Some(pe) = prev_end {
                inner.push(self.gap_between_positions(pe, eff_start));
            }
            // When prev_end is None (first member) and after_open is empty, the outer
            // Indent([HardLine, ...]) already provides the newline — no extra HardLine.

            inner.push(self.interface_member_to_doc(member));
            prev_end = Some(self.effective_end(member_span));
        }

        // Drain trailing comments inside the interface body.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(body_span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            if let Some(pe) = prev_end {
                inner.push(self.gap_between_positions(pe, start));
            }
            inner.push(self.drain_dangling_comments_doc(body_span.end));
        }

        Doc::concat(vec![
            Doc::text("interface {"),
            after_open,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text(format!("}}{suffix}")),
        ])
    }

    fn interface_member_to_doc(&self, member: &InterfaceMember) -> Doc {
        let span = match member {
            InterfaceMember::Function(m) => m.span,
            InterfaceMember::Variable(v) => v.span,
        };

        // Drain leading before building body — body construction calls type_to_doc
        // which also drains, so leading must come first.
        let leading = self.drain_leading_doc(span.start);

        let body = match member {
            InterfaceMember::Function(m) => {
                let mut parts = vec![
                    Doc::text("function "),
                    self.at(&m.name),
                    self.parameter_list_to_doc(&m.args),
                ];
                if let Some(ret) = &m.returns {
                    parts.push(Doc::text(" "));
                    parts.push(self.drain_leading_doc(m.as_kw_start.unwrap_or(ret.span.start)));
                    parts.push(Doc::text("as "));
                    parts.push(self.type_to_doc(ret));
                }

                parts.push(Doc::text(";"));
                Doc::Concat(parts)
            }
            InterfaceMember::Variable(v) => {
                let mut parts = vec![Doc::text("var "), self.at(&v.name)];
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(v.as_kw_start));
                parts.push(Doc::text("as "));
                parts.push(self.type_to_doc(&v.type_));
                parts.push(Doc::text(";"));
                Doc::Concat(parts)
            }
        };

        let trailing = self.drain_trailing_doc(span.end);

        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => body,
            _ => Doc::concat(vec![leading, body, trailing]),
        }
    }

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
        if self.comment_ends_line_before(condition.open + 1, cond_start) {
            let after_open = self.drain_trailing_doc_bounded(condition.open + 1, cond_start);
            let leading = self.drain_leading_doc(cond_start);
            let cond_doc = Doc::group(vec![self.condition_to_doc(cond)]);
            let cond_trailing = self.drain_trailing_doc_bounded(cond_end, paren_close);
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
            let has_line_inside = self.has_trailing_line_comment_bounded(cond_end, paren_close);
            let cond_trailing = self.drain_trailing_doc_bounded(cond_end, paren_close);
            // Standalone comments between the condition end and `)` (e.g. `// c3`).
            let dangling = self.drain_dangling_comments_doc(paren_close);
            let has_inside = has_line_inside || !matches!(dangling, Doc::Empty);

            // Comments between `)` and the body `{`, on the same line as `)`.
            // Bounded by `brace_start` so we don't steal comments that belong
            // to the body (e.g. `// trailing` after a same-line `{ ... }`).
            let has_line_outside = self.has_trailing_line_comment_bounded(paren_close, brace_start);
            let after_paren = self.drain_trailing_doc_bounded(paren_close, brace_start);

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
        let breaks = self.has_trailing_line_comment_bounded(cond_end, paren_close);
        let mut parts = vec![self.drain_trailing_doc_bounded(cond_end, paren_close)];
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
        let mut parts = vec![header, self.block_body_to_doc(&s.then_branch)];

        // Bound the same-line trailing drain to the `else` keyword so that
        // comments on the `else {` / `else if` line are not captured here.
        let max_pos = s.else_kw_start.unwrap_or(usize::MAX);

        let has_trailing = self.has_trailing_line_comment_bounded(s.then_branch.span.end, max_pos);
        let trailing = self.drain_trailing_doc_bounded(s.then_branch.span.end, max_pos);

        // Drain standalone comments that sit between `}` and the `else` keyword
        // on their own lines (e.g. `}\n// note\nelse {`).  They must be consumed
        // before `block_to_doc` is called, otherwise `drain_leading_doc` inside
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

                parts.push(self.block_to_doc(b));
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

    fn binary_chain_parts(
        &self,
        operands: &[&Expr],
        ops: &[(&BinaryOperator, usize)],
        outer_max_pos: usize,
    ) -> Vec<Doc> {
        let mut parts: Vec<Doc> = Vec::new();

        for (i, operand) in operands.iter().enumerate() {
            let span = *operand.span();
            let leading = self.drain_leading_doc(span.start);

            // Bound the inner expression at the operator position so it cannot
            // steal comments that sit between the operand and the operator.
            // For the last operand fall back to `outer_max_pos`.
            let inner_max = ops
                .get(i)
                .map(|(_, op_pos)| *op_pos)
                .unwrap_or(outer_max_pos);

            let inner = match operand {
                Expr::Binary(e) => self.binary_chain_to_doc(operand, &e.operator, false, inner_max),
                _ => self.expr_inner_to_doc_ctx(operand, inner_max),
            };

            parts.push(leading);
            parts.push(inner);

            if i + 1 < operands.len() {
                let (op, op_pos) = ops[i];
                let op_text = operators::binary_op(op);
                let op_end = op_pos + op_text.len();
                let op_line = self.line_index.line(op_pos as u32);
                let next_start = operands[i + 1].span().start;

                // A comment after the operator that ends the line keeps the operator at the end of
                // the line in front of it, as in `left || // c`, rather than moving past it.
                let comment_after_op = self.comment_ends_line_before(op_end, next_start);
                let breaks = self.comment_breaks_line_before(span.end, op_pos);
                parts.push(self.drain_before_token(span.end, op_pos));

                if comment_after_op {
                    let op_on_operand_line = op_line == self.line_index.line(span.end as u32 - 1);
                    parts.push(if breaks {
                        Doc::HardLine
                    } else if op_on_operand_line {
                        Doc::text(" ")
                    } else {
                        Doc::Line
                    });
                    parts.push(Doc::text(op_text));
                    parts.push(self.drain_trailing_doc_bounded(op_end, next_start));
                    parts.push(Doc::HardLine);
                } else {
                    parts.push(if breaks { Doc::HardLine } else { Doc::Line });
                    parts.push(Doc::text(format!("{op_text} ")));
                }
            } else {
                parts.push(self.drain_trailing_doc_bounded(span.end, outer_max_pos));
            }
        }

        parts
    }

    fn binary_chain_to_doc(
        &self,
        expr: &Expr,
        op: &BinaryOperator,
        outermost: bool,
        outer_max_pos: usize,
    ) -> Doc {
        let mut operands: Vec<&Expr> = Vec::new();
        let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
        collect_binary_chain(expr, op, &mut operands, &mut ops);

        let inner = self.binary_chain_parts(&operands, &ops, outer_max_pos);

        if outermost {
            Doc::Group(vec![Doc::Indent(inner)])
        } else {
            Doc::Group(inner)
        }
    }

    fn condition_to_doc(&self, expr: &Expr) -> Doc {
        if let Expr::Binary(e) = expr {
            let mut operands: Vec<&Expr> = Vec::new();
            let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
            collect_binary_chain(expr, &e.operator, &mut operands, &mut ops);

            // Cap at cond.span.end so the last operand cannot drain trailing
            // comments that sit outside the condition (i.e. after the `)`
            // in `if (…)`). paren_condition_header drains those via has_line.
            let cond_end = expr.span().end;

            return Doc::Concat(self.binary_chain_parts(&operands, &ops, cond_end));
        }

        self.expr_with_leading(expr)
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

        for (i, case) in s.cases.iter().enumerate() {
            if i > 0 {
                body.push(Doc::HardLine);
            }

            body.push(self.drain_leading_doc(case.span.start));

            let mut header = vec![Doc::text("case ")];
            match &case.label {
                CaseLabel::Value(e) => {
                    header.push(self.expr_with_leading(e));
                    // Drain inline block comments between the value and ':',
                    // e.g. `case 1 /*NAME*/ :`.
                    header.push(self.drain_trailing_doc_bounded(e.span().end, case.label_span.end));
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
            let body_span = Span {
                start: case.label_span.end,
                end: case.span.end,
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
            // For fall-through cases (empty stmts), span.end points to the
            // start of the next `case` keyword. Anchor trailing drain at the
            // label's `:` so we don't steal comments from the following case.
            let trailing_anchor = case
                .stmts
                .last()
                .map(|s| s.span().end)
                .unwrap_or(case.label_span.end);
            // Several labels can share a line, e.g. `case 1: case 2: // c`, so a comment after a
            // later label belongs to that label and not this one.
            let next_case_start = s
                .cases
                .get(i + 1)
                .map(|next| next.span.start)
                .unwrap_or(usize::MAX);
            let case_trailing = self.drain_trailing_doc_bounded(trailing_anchor, next_case_start);
            let has_content = !case.stmts.is_empty() || !matches!(case_inner, Doc::Empty);
            if has_content || !matches!(case_trailing, Doc::Empty) {
                body.push(Doc::Indent(vec![Doc::HardLine, case_inner, case_trailing]));
            }
        }

        // Drain any comments after the last case.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(switch_body_span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            let prev_end = s.cases.last().map(|c| c.span.end).unwrap_or(s.brace_start);
            body.push(self.gap_between_positions(prev_end, start));
            body.push(self.drain_dangling_comments_doc(switch_body_span.end));
        }

        Doc::concat(vec![
            header,
            before_brace,
            Doc::text("{"),
            after_open,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(body)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn try_stmt_to_doc(&self, s: &TryStmt) -> Doc {
        let mut parts = vec![Doc::text("try "), self.block_body_to_doc(&s.body)];

        for catch in &s.catches {
            let mut header = vec![Doc::text(" catch ("), Doc::text(&catch.binding)];
            if let Some(ty) = &catch.type_filter {
                header.push(Doc::text(" instanceof "));
                header.push(self.type_to_doc(ty));
            }

            header.push(Doc::text(")"));
            parts.push(Doc::Concat(header));
            parts.push(self.block_to_doc(&catch.body));
        }

        if let Some(f) = &s.finally {
            parts.push(Doc::text(" finally"));
            parts.push(self.block_to_doc(f));
        }

        Doc::Concat(parts)
    }

    /// Render a block body `{ … }` where the caller has already emitted the
    /// preceding space or token. Drains comments that appear between the last
    /// caller token and `{` (before-bracket zone) and after `{` on the same
    /// line (after-open-brace zone).
    fn block_body_to_doc(&self, block: &BlockStmt) -> Doc {
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

        let inner = self.stmts_to_doc(&block.stmts, block.span);

        if block.stmts.is_empty() && matches!(inner, Doc::Empty) {
            return Doc::concat(vec![
                before_brace,
                Doc::text("{"),
                after_open,
                Doc::text("}"),
            ]);
        }

        Doc::concat(vec![
            before_brace,
            Doc::text("{"),
            after_open,
            Doc::Indent(vec![Doc::HardLine, inner]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    /// Render ` { … }` (with leading space) — used after `else`, `catch`,
    /// `finally`, and similar keywords that precede a block.
    fn block_to_doc(&self, block: &BlockStmt) -> Doc {
        let comment_on_own_line = self
            .comment_cursor
            .borrow()
            .peek_before(block.span.start)
            .is_some_and(|c| self.line_index.starts_line(c.span.start as u32));
        let before_brace = self.drain_leading_doc(block.span.start);

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

        let open = if matches!(before_brace, Doc::Empty) {
            Doc::text(" {")
        } else if comment_on_own_line {
            Doc::concat(vec![Doc::HardLine, before_brace, Doc::text("{")])
        } else {
            Doc::concat(vec![Doc::text(" "), before_brace, Doc::text("{")])
        };

        let inner = self.stmts_to_doc(&block.stmts, block.span);

        if block.stmts.is_empty() && matches!(inner, Doc::Empty) {
            return Doc::concat(vec![open, after_open, Doc::text("}")]);
        }

        Doc::concat(vec![
            open,
            after_open,
            Doc::Indent(vec![Doc::HardLine, inner]),
            Doc::HardLine,
            Doc::text("}"),
        ])
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

        // Drain any remaining comments inside the container (standalone after last stmt).
        // Use drain_dangling_comments_doc so we don't add a trailing HardLine after the
        // last comment — the enclosing block structure provides the newline before `}`.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(container.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            if let Some(pe) = prev_end {
                docs.push(self.gap_between_positions(pe, start));
            }

            docs.push(self.drain_dangling_comments_doc(container.end));
        }

        if docs.is_empty() {
            return Doc::Empty;
        }

        Doc::Concat(docs)
    }

    /// Render a statement with its comments, capping the trailing comment drain at `max_pos`.
    /// Pass `container.end` when the stmt is inside a block so that comments
    /// after the closing `}` are not accidentally captured by the last stmt.
    fn stmt_to_doc_bounded(&self, stmt: &Stmt, max_pos: usize) -> Doc {
        let span = *stmt.span();
        let leading = self.drain_leading_doc(span.start);
        let inner = self.stmt_inner_to_doc(stmt);
        let trailing = self.drain_trailing_doc_bounded(span.end, max_pos);

        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => inner,
            _ => Doc::concat(vec![leading, inner, trailing]),
        }
    }

    fn stmt_inner_to_doc(&self, stmt: &Stmt) -> Doc {
        match stmt {
            Stmt::Block(block) => self.block_body_to_doc(block),
            Stmt::If(s) => self.if_stmt_to_doc(s),
            Stmt::While(s) => Doc::concat(vec![
                self.paren_condition_header("while", &s.condition, s.body.span.start),
                self.block_body_to_doc(&s.body),
            ]),
            Stmt::DoWhile(s) => {
                let condition = &s.condition;
                let mut parts = vec![
                    Doc::text("do "),
                    self.block_body_to_doc(&s.body),
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
                self.push_before_semi(&mut parts, s.header.inner.first_semi);

                parts.push(Doc::text(" "));
                if let Some(condition) = &s.header.inner.condition {
                    parts.push(self.expr_with_leading_bounded(condition, s.header.close));
                }

                self.push_before_semi(&mut parts, s.header.inner.second_semi);
                parts.push(Doc::text(" "));
                if let Some(update) = &s.header.inner.update {
                    parts.push(self.expr_list_to_doc(update));
                }

                let before_close = self.drain_leading_doc(s.header.close);
                if !matches!(before_close, Doc::Empty) {
                    parts.push(Doc::text(" "));
                    parts.push(before_close);
                }
                parts.push(Doc::text(") "));
                parts.push(self.block_body_to_doc(&s.body));

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

    fn expr_list_to_doc(&self, exprs: &[Expr]) -> Doc {
        Doc::concat(
            exprs
                .iter()
                .enumerate()
                .flat_map(|(i, e)| {
                    let sep = if i > 0 { vec![Doc::text(", ")] } else { vec![] };
                    sep.into_iter()
                        .chain(std::iter::once(self.expr_with_leading(e)))
                })
                .collect(),
        )
    }

    fn expr_with_leading(&self, expr: &Expr) -> Doc {
        let span = *expr.span();
        let leading = self.drain_leading_doc(span.start);
        let inner = self.expr_inner_to_doc(expr);

        if matches!(&leading, Doc::Empty) {
            inner
        } else {
            Doc::concat(vec![leading, inner])
        }
    }

    /// Like [`expr_with_leading`] but caps the trailing drain of any nested
    /// binary chain at `outer_max_pos`. Used when the expression sits inside a
    /// delimiter (e.g. paren) whose closing token must not be swallowed by a
    /// `//` comment that belongs to an outer chain.
    fn expr_with_leading_bounded(&self, expr: &Expr, outer_max_pos: usize) -> Doc {
        let leading = self.drain_leading_doc(expr.span().start);
        let inner = self.expr_inner_to_doc_ctx(expr, outer_max_pos);

        if matches!(&leading, Doc::Empty) {
            inner
        } else {
            Doc::concat(vec![leading, inner])
        }
    }

    fn expr_inner_to_doc_ctx(&self, expr: &Expr, outer_max_pos: usize) -> Doc {
        match expr {
            // Bound binary chains at the expression's own span so they cannot
            // drain trailing comments that belong to the surrounding delimiter
            // (e.g. a trailing `//` after the last operand of an array entry
            // must stay after the `,`, not inside the binary expression doc).
            Expr::Binary(e) => self.binary_chain_to_doc(
                expr,
                &e.operator,
                true,
                expr.span().end.min(outer_max_pos),
            ),
            Expr::Paren(e) => {
                let inner_end = e.inner.span().end;
                let paren_end = e.span.end.min(outer_max_pos);

                Doc::concat(vec![
                    Doc::text("("),
                    self.expr_with_leading_bounded(&e.inner, paren_end),
                    self.drain_trailing_doc_bounded(inner_end, paren_end),
                    Doc::text(")"),
                ])
            }
            _ => self.expr_inner_to_doc(expr),
        }
    }

    fn expr_inner_to_doc(&self, expr: &Expr) -> Doc {
        match expr {
            // Bound binary chains at the expression's own span so they cannot
            // steal trailing comments that appear after a following `;` or `,`.
            Expr::Binary(e) => self.binary_chain_to_doc(expr, &e.operator, true, expr.span().end),
            Expr::Unary(e) => match e.operator {
                UnaryOperator::PostInc => {
                    Doc::concat(vec![self.expr_with_leading(&e.operand), Doc::text("++")])
                }
                UnaryOperator::PostDec => {
                    Doc::concat(vec![self.expr_with_leading(&e.operand), Doc::text("--")])
                }
                _ => Doc::concat(vec![
                    Doc::text(operators::unary_prefix_op(&e.operator)),
                    self.expr_with_leading(&e.operand),
                ]),
            },
            Expr::Ternary(e) => {
                let condition = self.expr_with_leading(&e.condition);
                let condition_end = e.condition.span().end;
                let question_break = self.line_before_token(condition_end, e.question_pos);
                let before_question = self.drain_before_token(condition_end, e.question_pos);
                let then_expr =
                    self.after_token(e.question_pos + 1, e.then_expr.span().start, || {
                        self.expr_with_leading(&e.then_expr)
                    });
                let then_end = e.then_expr.span().end;
                let colon_break = self.line_before_token(then_end, e.colon_pos);
                let before_colon = self.drain_before_token(then_end, e.colon_pos);
                let else_expr = self.after_token(e.colon_pos + 1, e.else_expr.span().start, || {
                    self.expr_with_leading(&e.else_expr)
                });

                Doc::Group(vec![
                    condition,
                    before_question,
                    Doc::Indent(vec![
                        question_break,
                        Doc::text("?"),
                        then_expr,
                        before_colon,
                        colon_break,
                        Doc::text(":"),
                        else_expr,
                    ]),
                ])
            }
            Expr::Assign(e) => Doc::concat(vec![
                self.expr_with_leading(&e.target),
                self.token_then_expr(
                    e.target.span().end,
                    operators::assign_op(&e.operator),
                    e.op_pos,
                    &e.value,
                ),
            ]),
            Expr::Call(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                let callee = self.expr_with_leading(&e.callee);

                Doc::concat(vec![callee, self.call_arguments_to_doc(e)])
            }
            Expr::Member(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                Doc::concat(vec![
                    self.expr_with_leading(&e.object),
                    Doc::text(format!(".{}", e.property)),
                ])
            }
            Expr::Index(e) => {
                let mut parts = vec![
                    self.expr_with_leading(&e.object),
                    Doc::text("["),
                    self.expr_with_leading(&e.index),
                ];
                let before_close = self.drain_leading_doc(e.span.end - 1);
                if !matches!(before_close, Doc::Empty) {
                    parts.push(Doc::text(" "));
                    parts.push(before_close);
                }
                parts.push(Doc::text("]"));
                Doc::Concat(parts)
            }
            Expr::New(e) => {
                // Drained here or the first argument would claim them and move them inside `(`.
                let class = Doc::concat(vec![
                    Doc::text("new "),
                    Doc::Indent(vec![
                        self.drain_leading_doc(e.class_span.start),
                        Doc::text(&e.class),
                    ]),
                ]);

                let Some(args_open) = e.args_open else {
                    return Doc::concat(vec![class, Doc::text("()")]);
                };

                let before_open = self.drain_before_open_paren(args_open);
                let first_arg_start = e
                    .args
                    .first()
                    .map(|a| a.value.span().start)
                    .unwrap_or(e.span.end);
                let hugged = self.hugged_sole_collection(
                    &e.args,
                    e.args.has_trailing_comma(),
                    args_open,
                    e.span.end,
                );

                if let Some(args) = hugged {
                    return Doc::concat(vec![class, before_open, args]);
                }

                let after_open_force_newline =
                    self.after_open_has_line_comment(args_open, first_arg_start);
                let after_open = self.drain_after_open_paren(args_open, first_arg_start);

                let items = self.call_args_to_items(&e.args, e.span.end);
                let before_close = self.drain_dangling_comments_doc(e.span.end);

                Doc::concat(vec![
                    class,
                    before_open,
                    self.format_list(
                        "(",
                        ")",
                        items,
                        before_close,
                        e.args.has_trailing_comma(),
                        after_open,
                        after_open_force_newline,
                    ),
                ])
            }
            Expr::NewArray(e) => {
                let mut parts = vec![Doc::text("new")];
                if let Some(ty) = &e.element_type {
                    parts.push(Doc::text(" "));
                    parts.push(self.type_to_doc(ty));
                } else {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::text("["));
                parts.push(self.expr_with_leading(&e.size));
                let close_pos = e.span.end - if e.is_byte_array { 2 } else { 1 };
                let before_close = self.drain_leading_doc(close_pos);
                if !matches!(before_close, Doc::Empty) {
                    parts.push(Doc::text(" "));
                    parts.push(before_close);
                }
                parts.push(Doc::text(if e.is_byte_array { "]b" } else { "]" }));

                Doc::Concat(parts)
            }
            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_with_leading(&e.expr),
                Doc::text(" as "),
                self.type_to_doc(&e.target_type),
            ]),
            Expr::Array(e) => self.format_array(e),
            Expr::Dict(e) => self.format_dict(e),
            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Number(v)
                | LiteralValue::Long(v)
                | LiteralValue::Hex(v)
                | LiteralValue::HexLong(v) => v.clone(),
                LiteralValue::Float(lit) => format_float_lit(lit),
                LiteralValue::Double(lit) => format_double_lit(lit),
                LiteralValue::String(v) => format!("\"{v}\""),
                LiteralValue::Char(v) => format!("'{v}'"),
                LiteralValue::Boolean(v) => v.to_string(),
                LiteralValue::Symbol(v) => format!(":{v}"),
                LiteralValue::Null => "null".to_string(),
                LiteralValue::NaN => "NaN".to_string(),
            }),
            Expr::Ident(e) => Doc::text(&e.name),
            Expr::Paren(e) => {
                let inner_end = e.inner.span().end;

                Doc::concat(vec![
                    Doc::text("("),
                    self.expr_with_leading_bounded(&e.inner, e.span.end),
                    self.drain_trailing_doc_bounded(inner_end, e.span.end),
                    Doc::text(")"),
                ])
            }
            Expr::Me(_) => Doc::text("me"),
            Expr::Self_(_) => Doc::text("self"),
            Expr::Bling(_) => Doc::text("$"),
        }
    }

    /// The `(…)` of a call whose only argument is an array or dict literal, rendered as
    /// `([` … `])` or `({` … `})` so the collection alone decides whether to break. Hugging
    /// saves a level of indentation for the entries. `None` when it does not apply, including when
    /// a comment sits between the parentheses and the brackets, since it would have nowhere to go.
    fn hugged_sole_collection(
        &self,
        args: &[CallArg],
        args_trailing_comma: bool,
        args_open: usize,
        call_end: usize,
    ) -> Option<Doc> {
        if args_trailing_comma {
            return None;
        }

        let [argument] = args else {
            return None;
        };

        if !matches!(argument.value, Expr::Array(_) | Expr::Dict(_)) {
            return None;
        }

        let array_span = *argument.value.span();
        let before_array = Span {
            start: args_open + 1,
            end: array_span.start,
        };
        let after_array = Span {
            start: array_span.end,
            end: call_end,
        };

        if self.has_comments_in(before_array) || self.has_comments_in(after_array) {
            return None;
        }

        Some(Doc::concat(vec![
            Doc::text("("),
            self.expr_inner_to_doc(&argument.value),
            Doc::text(")"),
        ]))
    }

    /// The `(…)` part of a call.
    fn call_arguments_to_doc(&self, e: &CallExpr) -> Doc {
        let before_open = self.drain_before_open_paren(e.args_open);
        if let Some(args) = self.hugged_sole_collection(
            &e.args,
            e.args.has_trailing_comma(),
            e.args_open,
            e.span.end,
        ) {
            return Doc::concat(vec![before_open, args]);
        }

        // Only capture comments between `(` and the first argument as after-open.
        // Comments inside an argument expression (e.g. `x - /* C */ 1`) must not
        // be stolen here — they belong to the sub-expression's drain_leading_doc.
        let first_arg_start = e
            .args
            .first()
            .map(|a| a.value.span().start)
            .unwrap_or(e.span.end);
        let after_open_force_newline =
            self.after_open_has_line_comment(e.args_open, first_arg_start);
        let after_open = self.drain_after_open_paren(e.args_open, first_arg_start);

        let items = self.call_args_to_items(&e.args, e.span.end);
        let before_close = self.drain_dangling_comments_doc(e.span.end);

        Doc::concat(vec![
            before_open,
            self.format_list(
                "(",
                ")",
                items,
                before_close,
                e.args.has_trailing_comma(),
                after_open,
                after_open_force_newline,
            ),
        ])
    }

    fn call_args_to_items(&self, args: &Separated<CallArg>, args_close: usize) -> Vec<ListItem> {
        args.iter()
            .enumerate()
            .map(|(i, a)| {
                let next_arg_start = args.get(i + 1).map(|next| next.value.span().start);
                let content =
                    self.expr_with_leading_bounded(&a.value, next_arg_start.unwrap_or(args_close));

                self.list_item(
                    content,
                    a.value.span().end,
                    args.comma_after(i),
                    next_arg_start,
                    args_close,
                )
            })
            .collect()
    }

    fn format_array(&self, e: &ArrayExpr) -> Doc {
        let body = self.format_array_body(e);

        if e.is_byte_array {
            Doc::concat(vec![body, Doc::text("b")])
        } else {
            body
        }
    }

    fn format_array_body(&self, e: &ArrayExpr) -> Doc {
        if e.entries.is_empty() {
            if self
                .comment_cursor
                .borrow()
                .has_comment_in(e.span.start + 1, e.span.end)
            {
                let comments = self.comment_cursor.borrow_mut().drain_before(e.span.end);
                let mut inner = Vec::new();
                for (i, c) in comments.iter().enumerate() {
                    if i > 0 {
                        inner.push(Doc::HardLine);
                    }
                    inner.push(self.comment_to_doc(c));
                }

                return Doc::concat(vec![
                    Doc::text("["),
                    Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                    Doc::HardLine,
                    Doc::text("]"),
                ]);
            }

            return Doc::text("[]");
        }

        let must_break = e.entries.has_trailing_comma() || self.has_comments_in(e.span);

        if must_break {
            return self.format_array_multiline(e);
        }

        let items: Vec<ListItem> = e
            .entries
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let value_span = *entry.value.span();
                let next_start = e
                    .entries
                    .get(i + 1)
                    .map(|ne| ne.value.span().start)
                    .unwrap_or(e.span.end);
                ListItem {
                    content: self.expr_with_leading_bounded(&entry.value, next_start),
                    before_separator: Doc::Empty,
                    trailing: self.drain_trailing_doc_bounded(value_span.end, next_start),
                    trailing_is_line_comment: false,
                }
            })
            .collect();

        self.format_list("[", "]", items, Doc::Empty, false, Doc::Empty, false)
    }

    fn format_array_multiline(&self, e: &ArrayExpr) -> Doc {
        self.format_collection_multiline(
            e.span,
            &e.entries,
            "[",
            "]",
            |entry| entry.value.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                self.expr_with_leading_bounded(&entry.value, next_entry_start)
            },
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn format_list(
        &self,
        open: &str,
        close: &str,
        items: Vec<ListItem>,
        before_close: Doc,
        trailing_comma: bool,
        after_open: Doc,
        after_open_force_newline: bool,
    ) -> Doc {
        let has_after_open = !matches!(&after_open, Doc::Empty);
        let has_before_close = !matches!(&before_close, Doc::Empty);

        if items.is_empty() && !has_before_close && !has_after_open {
            return Doc::text(format!("{open}{close}"));
        }

        if items.is_empty() && !has_after_open {
            return Doc::concat(vec![
                Doc::text(open),
                Doc::Indent(vec![Doc::HardLine, before_close]),
                Doc::HardLine,
                Doc::text(close),
            ]);
        }

        // Line comments (`// …`) consume the rest of the source line, so any
        // list containing one must be forced multi-line regardless of width.
        // Block comments (`/* … */`) can stay inline — let the pretty-printer
        // decide based on the available line width.
        let force_multiline = trailing_comma
            || after_open_force_newline
            || has_before_close
            || items.iter().any(|i| i.trailing_is_line_comment);

        let last_idx = items.len().saturating_sub(1);
        let mut inner = Vec::new();
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                inner.push(if force_multiline {
                    Doc::HardLine
                } else {
                    Doc::Line
                });
            }

            inner.push(item.content);
            inner.push(item.before_separator);

            if i != last_idx || (force_multiline && trailing_comma) {
                inner.push(Doc::text(","));
            }

            inner.push(item.trailing);
        }

        if force_multiline {
            if has_before_close {
                inner.push(Doc::HardLine);
                inner.push(before_close);
            }

            return Doc::concat(vec![
                Doc::text(open),
                after_open,
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text(close),
            ]);
        }

        // When there's a block comment after the opening delimiter, separate
        // it from the content with `Doc::Line` (" " flat, newline+indent
        // expanded) so flat mode gives `(/* c */ x)` not `(/* c */x)`.
        let indent_start = if has_after_open && inner.is_empty() {
            vec![after_open]
        } else if has_after_open {
            vec![after_open, Doc::Line]
        } else {
            vec![Doc::SoftLine]
        };

        Doc::Group(vec![
            Doc::text(open),
            Doc::Indent([indent_start, vec![Doc::Concat(inner)]].concat()),
            Doc::SoftLine,
            Doc::text(close),
        ])
    }

    fn format_dict(&self, e: &DictExpr) -> Doc {
        if e.entries.is_empty() {
            if self
                .comment_cursor
                .borrow()
                .has_comment_in(e.span.start + 1, e.span.end)
            {
                let comments = self.comment_cursor.borrow_mut().drain_before(e.span.end);
                let mut inner = Vec::new();
                for (i, c) in comments.iter().enumerate() {
                    if i > 0 {
                        inner.push(Doc::HardLine);
                    }
                    inner.push(self.comment_to_doc(c));
                }

                return Doc::concat(vec![
                    Doc::text("{"),
                    Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                    Doc::HardLine,
                    Doc::text("}"),
                ]);
            }

            return Doc::text("{}");
        }

        let must_break = e.entries.has_trailing_comma() || self.has_comments_in(e.span);

        if must_break {
            return self.format_dict_multiline(e);
        }

        if self.align_pairs {
            return self.format_dict_aligned_or_inline(e);
        }

        self.format_dict_inline_or_break(e)
    }

    fn format_dict_aligned_or_inline(&self, e: &DictExpr) -> Doc {
        let mut flat_inner = Vec::new();
        for (i, entry) in e.entries.iter().enumerate() {
            if i > 0 {
                flat_inner.push(Doc::text(", "));
            }

            // format_dict guarantees no comments inside dict when this path is
            // taken, so expr_inner_to_doc is safe and avoids accidentally
            // draining trailing comments that belong to the outer context (e.g.
            // a `// comment` after `}` in an enclosing array entry).
            flat_inner.push(Doc::concat(vec![
                self.expr_inner_to_doc(&entry.key),
                Doc::text(" => "),
                self.expr_inner_to_doc(&entry.value),
            ]));
        }
        let flat_doc = Doc::concat(vec![
            Doc::text("{"),
            Doc::Concat(flat_inner),
            Doc::text("}"),
        ]);
        let break_doc = self.format_dict_multiline(e);

        Doc::Group(vec![Doc::flat_or_break(flat_doc, break_doc)])
    }

    fn format_dict_multiline(&self, e: &DictExpr) -> Doc {
        let aligned = self.align_pairs && !e.entries.is_empty();
        let max_key_width = if aligned {
            e.entries
                .iter()
                .filter_map(|entry| expr_key_width(&entry.key))
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        self.format_collection_multiline(
            e.span,
            &e.entries,
            "{",
            "}",
            |entry| entry.key.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                let key = self.expr_with_leading(&entry.key);
                let key_end = entry.key.span().end;
                let arrow_breaks = self.comment_breaks_line_before(key_end, entry.arrow_pos);
                let before_arrow = self.drain_before_token(key_end, entry.arrow_pos);
                let value = self.after_token(
                    entry.arrow_pos + "=>".len(),
                    entry.value.span().start,
                    || self.expr_with_leading_bounded(&entry.value, next_entry_start),
                );

                if arrow_breaks {
                    return Doc::concat(vec![
                        key,
                        before_arrow,
                        Doc::Indent(vec![Doc::HardLine, Doc::text("=>"), value]),
                    ]);
                }

                let padding = if aligned {
                    expr_key_width(&entry.key)
                        .map(|w| " ".repeat(max_key_width.saturating_sub(w)))
                        .unwrap_or_default()
                } else {
                    String::new()
                };

                Doc::concat(vec![
                    key,
                    before_arrow,
                    Doc::Text(padding),
                    Doc::text(" =>"),
                    value,
                ])
            },
        )
    }

    /// Render `entries` one per line. `content_of` renders an entry given where the next one
    /// starts, which bounds the comments it may take.
    #[allow(clippy::too_many_arguments)]
    fn format_collection_multiline<E>(
        &self,
        span: Span,
        entries: &Separated<E>,
        open: &str,
        close: &str,
        start_of: impl Fn(&E) -> usize,
        end_of: impl Fn(&E) -> usize,
        content_of: impl Fn(&E, usize) -> Doc,
    ) -> Doc {
        let trailing_comma = entries.has_trailing_comma();
        let last_idx = entries.len().saturating_sub(1);

        // A comment on the line of the opening bracket stays there, unless the first entry shares
        // that line too and the comment is in front of it.
        let open_line = self.line_index.line(span.start as u32);
        let after_open = match entries.first().map(&start_of) {
            Some(first_start) if self.line_index.line(first_start as u32) != open_line => {
                self.drain_after_open_brace(span.start, first_start)
            }
            _ => Doc::Empty,
        };

        let mut inner: Vec<Doc> = Vec::new();
        let mut prev_end: Option<usize> = None;

        for (i, entry) in entries.iter().enumerate() {
            let entry_start = start_of(entry);

            // Drain any standalone comments before this entry — but only those on a
            // different line. A block comment on the same line as the entry is an inline
            // prefix (e.g. `/* INFO */ :key => val`) and is handled by `content_of`.
            let entry_line = self.line_index.line(entry_start as u32);
            let comment_start = self
                .comment_cursor
                .borrow()
                .peek_before(entry_start)
                .filter(|c| self.line_index.line(c.span.start as u32) != entry_line)
                .map(|c| c.span.start);

            if let Some(start) = comment_start {
                self.push_gap(&mut inner, prev_end, start, trailing_comma);
                let comment_docs = self.drain_leading_doc(entry_start);
                inner.push(comment_docs);
            } else {
                self.push_gap(&mut inner, prev_end, entry_start, trailing_comma);
            }

            // Only capture trailing comments up to the next entry's start (or
            // the collection close). This prevents stealing a comment from a
            // later entry or from outside the collection.
            let next_entry_start = entries.get(i + 1).map(&start_of);
            let content = content_of(entry, next_entry_start.unwrap_or(span.end));
            let entry_end = end_of(entry);
            let item = self.list_item(
                content,
                entry_end,
                entries.comma_after(i),
                next_entry_start,
                span.end,
            );

            let mut parts = vec![item.content, item.before_separator];
            if i != last_idx || trailing_comma {
                parts.push(Doc::text(","));
            }

            parts.push(item.trailing);
            inner.push(Doc::Concat(parts));
            prev_end = Some(self.comment_cursor.borrow().last_end().max(entry_end));
        }

        // Drain any standalone comments after the last entry.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            self.push_gap(&mut inner, prev_end, start, trailing_comma);
            inner.push(self.drain_dangling_comments_doc(span.end));
        }

        Doc::concat(vec![
            Doc::text(open.to_string()),
            after_open,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text(close.to_string()),
        ])
    }

    fn push_gap(
        &self,
        inner: &mut Vec<Doc>,
        prev_end: Option<usize>,
        next_start: usize,
        preserve_blanks: bool,
    ) {
        let Some(prev) = prev_end else { return };

        if preserve_blanks {
            inner.push(self.gap_between_positions(prev, next_start));
        } else {
            inner.push(Doc::HardLine);
        }
    }

    fn format_dict_inline_or_break(&self, e: &DictExpr) -> Doc {
        let items: Vec<ListItem> = e
            .entries
            .iter()
            .map(|entry| ListItem {
                // format_dict guarantees no comments inside dict when this
                // path is taken — use expr_inner_to_doc to avoid draining
                // trailing comments that belong to the outer context.
                content: Doc::concat(vec![
                    self.expr_inner_to_doc(&entry.key),
                    Doc::text(" => "),
                    self.expr_inner_to_doc(&entry.value),
                ]),
                before_separator: Doc::Empty,
                trailing: Doc::Empty,
                trailing_is_line_comment: false,
            })
            .collect();

        self.format_list(
            "{",
            "}",
            items,
            Doc::Empty,
            e.entries.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }

    fn gap_between_positions(&self, prev_end: usize, next_start: usize) -> Doc {
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

/// Compute the flat display width of a dict key expression without touching
/// the comment cursor. Used for alignment padding in multiline dicts.
///
/// Returns `None` when the expression cannot be represented as a single flat
/// token sequence (e.g. a call or binary expression as a key).
fn expr_key_width(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Ident(e) => Some(display_width(&e.name)),
        Expr::Me(_) => Some("me".len()),
        Expr::Self_(_) => Some("self".len()),
        Expr::Bling(_) => Some(1),
        Expr::Member(e) => Some(expr_key_width(&e.object)? + 1 + display_width(&e.property)),
        Expr::Lit(e) => Some(match &e.value {
            LiteralValue::Number(v)
            | LiteralValue::Long(v)
            | LiteralValue::Hex(v)
            | LiteralValue::HexLong(v) => v.len(),
            LiteralValue::Float(lit) => format_float_lit(lit).len(),
            LiteralValue::Double(lit) => format_double_lit(lit).len(),
            LiteralValue::String(v) => 2 + display_width(v),
            LiteralValue::Char(v) => 3 + display_width(v),
            LiteralValue::Boolean(v) => v.to_string().len(),
            LiteralValue::Symbol(v) => 1 + display_width(v),
            LiteralValue::Null => "null".len(),
            LiteralValue::NaN => "NaN".len(),
        }),
        _ => None,
    }
}

fn wrap_annotation(inner: Vec<Doc>, multiline: bool) -> Doc {
    if !multiline {
        return Doc::concat(vec![Doc::text("("), Doc::Concat(inner), Doc::text(")")]);
    }

    Doc::concat(vec![
        Doc::text("("),
        Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
        Doc::HardLine,
        Doc::text(")"),
    ])
}

/// Re-emit a [`FloatLit`] in the exact source form recorded by the lexer.
fn format_float_lit(lit: &FloatLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    match lit.suffix {
        Some(suffix) => format!("{body}{suffix}"),
        None => body,
    }
}

/// Re-emit a [`DoubleLit`] preserving its source style.
fn format_double_lit(lit: &DoubleLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    format!("{body}{}", lit.suffix)
}

fn collect_binary_chain<'a>(
    expr: &'a Expr,
    chain_op: &BinaryOperator,
    operands: &mut Vec<&'a Expr>,
    ops: &mut Vec<(&'a BinaryOperator, usize)>,
) {
    if let Expr::Binary(be) = expr
        && operators::precedence_group(&be.operator) == operators::precedence_group(chain_op)
    {
        collect_binary_chain(&be.left, chain_op, operands, ops);
        ops.push((&be.operator, be.op_pos));
        collect_binary_chain(&be.right, chain_op, operands, ops);

        return;
    }

    operands.push(expr);
}

fn enum_variant_name_pads(variants: &[EnumVariant]) -> Vec<usize> {
    let mut out = vec![0; variants.len()];
    let mut i = 0;
    while i < variants.len() {
        if variants[i].value.is_none() {
            i += 1;
            continue;
        }

        let run_start = i;
        let mut max_name = 0;
        while i < variants.len() && variants[i].value.is_some() {
            max_name = max_name.max(display_width(&variants[i].name));
            i += 1;
        }

        if i - run_start >= 2 {
            for slot in out.iter_mut().take(i).skip(run_start) {
                *slot = max_name;
            }
        }
    }
    out
}

/// Column-align trailing `//` and `/*` comments across consecutive lines.
fn align_trailing_comments(text: &str) -> String {
    let lines: Vec<&str> = text.split('\n').collect();

    // Track block-comment spans so interior lines are not misidentified as
    // having trailing `//` comments (e.g. `https://` in a doc-comment URL).
    let mut in_block = false;
    let analyzed: Vec<Option<(usize, usize, usize)>> = lines
        .iter()
        .map(|l| {
            if in_block {
                if l.contains("*/") {
                    in_block = false;
                }
                return None;
            }
            let result = analyze_trailing(l);
            // Case 1: trailing `/* ... */` opener on a code line (analyze_trailing
            // returned Some with a `/*` comment that doesn't close on this line).
            if let Some((_, _, cs)) = result {
                let slice = &l.as_bytes()[cs..];
                if slice.len() >= 2 && slice[1] == b'*' && !l[cs..].contains("*/") {
                    in_block = true;
                    return None;
                }
            }

            // Case 2: `/*` occupies the whole line with no code before it.
            // analyze_trailing returns None in this case (code_end <= indent),
            // so we detect it by inspecting the trimmed line directly.  This
            // is the common `/** doc comment */` pattern.
            let trimmed = l.trim_start();
            if trimmed.starts_with("/*") && !trimmed.contains("*/") {
                in_block = true;
            }

            result
        })
        .collect();

    let mut out: Vec<String> = lines.iter().map(|l| (*l).to_string()).collect();

    let mut i = 0;
    while i < analyzed.len() {
        let Some((indent, _, _)) = analyzed[i] else {
            i += 1;
            continue;
        };

        let mut j = i;
        while j < analyzed.len()
            && matches!(analyzed[j], Some((ind, _, _)) if ind == indent
                || (ind > indent && is_binary_chain_continuation(lines[j])))
        {
            j += 1;
        }

        if j - i >= 2 {
            let max_code = (i..j)
                .filter_map(|k| {
                    analyzed[k].map(|(_, code_end, _)| display_width(&lines[k][..code_end]))
                })
                .max()
                .unwrap_or(0);

            for k in i..j {
                let Some((_, code_end, comment_start)) = analyzed[k] else {
                    continue;
                };
                let line = lines[k];
                let code = &line[..code_end];
                let comment = &line[comment_start..];
                let pad = max_code - display_width(code);
                out[k] = format!("{code}{} {comment}", " ".repeat(pad));
            }
        }

        i = j;
    }

    out.join("\n")
}

fn is_binary_chain_continuation(line: &str) -> bool {
    const OPS: &[&str] = &[
        "==",
        "!=",
        "<=",
        ">=",
        "<<",
        ">>",
        "&&",
        "||",
        "and",
        "or",
        "instanceof",
        "has",
        "+",
        "-",
        "*",
        "/",
        "%",
        "<",
        ">",
        "&",
        "|",
        "^",
    ];

    let trimmed = line.trim_start();
    OPS.iter()
        .any(|op| trimmed.starts_with(op) && trimmed[op.len()..].starts_with(' '))
}

fn analyze_trailing(line: &str) -> Option<(usize, usize, usize)> {
    let bytes = line.as_bytes();
    let indent = bytes.iter().take_while(|b| **b == b' ').count();

    let mut i = indent;
    let mut in_string = false;
    let mut in_char = false;
    let mut comment_start: Option<usize> = None;

    while i < bytes.len() {
        let c = bytes[i];
        if in_string {
            if c == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }

            if c == b'"' {
                in_string = false;
            }
        } else if in_char {
            if c == b'\\' && i + 1 < bytes.len() {
                i += 2;
                continue;
            }

            if c == b'\'' {
                in_char = false;
            }
        } else if c == b'"' {
            in_string = true;
        } else if c == b'\'' {
            in_char = true;
        } else if c == b'/' && i + 1 < bytes.len() && matches!(bytes[i + 1], b'/' | b'*') {
            // A block comment with code after it on the same line is inline, not trailing.
            if bytes[i + 1] == b'*'
                && let Some(close) = line[i + 2..].find("*/")
            {
                let after = i + 2 + close + 2;
                if !line[after..].trim().is_empty() {
                    i = after;
                    continue;
                }
            }

            comment_start = Some(i);
            break;
        }

        i += 1;
    }

    let comment_start = comment_start?;

    let code_end = line[..comment_start].trim_end().len();
    if code_end <= indent {
        return None;
    }

    Some((indent, code_end, comment_start))
}
