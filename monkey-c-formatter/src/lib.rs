pub mod doc;
mod operators;

use doc::{Doc, render};
use monkey_c_parser::ast::{
    ArrayExpr, Ast, BinaryOperator, Binding, BlockStmt, CallArg, CaseLabel, CommentStmt, ConstDecl,
    DictExpr, DictTypeEntry, DictTypeKey, DoubleLit, ElseBranch, EnumDecl, EnumVariant, Expr,
    FloatLit, ForInit, FunctionDecl, IfStmt, InterfaceMember, LiteralValue, Span, Spanned, Stmt,
    SwitchStmt, TryStmt, Type, TypeKind, UnaryOperator, VarDecl, Visibility,
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
    /// operators column-aligned.
    align_pairs: bool,
    /// When `true`, multi-binding `var`/`const` declarations break each
    /// binding onto its own indented line.
    wrap_multi_bindings: bool,
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
            wrap_multi_bindings: false,
            comment_cursor: RefCell::new(CommentCursor::default()),
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    /// Enable column-aligned separators across related entries (opt-in).
    pub fn with_alignment(mut self, align_pairs: bool) -> Self {
        self.align_pairs = align_pairs;
        self
    }

    /// Enable per-binding wrapping for multi-binding `var`/`const` declarations.
    pub fn with_decl_wrap(mut self, decl_wrap: bool) -> Self {
        self.wrap_multi_bindings = decl_wrap;
        self
    }

    /// Format a parsed file and return the result as a `String`.
    pub fn format(&self, output: &ParseOutput) -> String {
        *self.comment_cursor.borrow_mut() = CommentCursor::new(&output.comments);

        let doc = self.ast_to_doc(&output.ast);
        let rendered = render(&doc, self.line_width);
        let mut aligned = align_trailing_comments(&rendered);

        if !aligned.ends_with('\n') {
            aligned.push('\n');
        }

        aligned
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
                parts.push(Doc::text(" "));
            } else {
                parts.push(Doc::HardLine);
            }
            parts.push(self.comment_to_doc(c));
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
            parts.push(Doc::text(" "));
            parts.push(self.comment_to_doc(c));
        }

        Doc::Concat(parts)
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
            self.block_comment_to_doc(&c.text)
        } else {
            Doc::text(format!("//{}", c.text))
        }
    }

    /// Render a `/* … */` comment, placing the closing `*/` on its own line
    /// for multi-line bodies.
    fn block_comment_to_doc(&self, text: &str) -> Doc {
        if !text.contains('\n') {
            return Doc::text(format!("/*{text}*/"));
        }

        let trimmed = text.trim_end();

        Doc::concat(vec![
            Doc::text(format!("/*{trimmed}")),
            Doc::HardLine,
            Doc::text("*/"),
        ])
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
                let header = Doc::concat(vec![
                    Doc::text("module "),
                    self.at(&decl.name),
                    Doc::text(" "),
                ]);
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
                let mut header_parts = vec![Doc::text("class "), self.at(&decl.name)];
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
            Ast::Annotation(entries, span) => {
                if entries.is_empty() {
                    let inner_start = span.start + 1;
                    let inner_end = span.end.saturating_sub(1);
                    if self
                        .comment_cursor
                        .borrow()
                        .has_comment_in(inner_start, inner_end)
                    {
                        let comments = self.comment_cursor.borrow_mut().drain_before(inner_end);
                        let mut inner = Vec::new();
                        for (i, c) in comments.iter().enumerate() {
                            if i > 0 {
                                inner.push(Doc::text(" "));
                            }
                            inner.push(self.comment_to_doc(c));
                        }

                        return Doc::concat(vec![
                            Doc::text("("),
                            Doc::Concat(inner),
                            Doc::text(")"),
                        ]);
                    }

                    return Doc::text("()");
                }

                let mut parts: Vec<Doc> = Vec::new();
                for (i, entry) in entries.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }

                    parts.push(self.drain_leading_doc(entry.span.start));
                    parts.push(Doc::text(format!(":{}", entry.name)));
                    if !entry.args.is_empty() {
                        parts.push(Doc::text("("));
                        for (j, arg) in entry.args.iter().enumerate() {
                            if j > 0 {
                                parts.push(Doc::text(", "));
                            }

                            parts.push(self.expr_with_leading(arg));
                        }

                        parts.push(Doc::text(")"));
                    }

                    // Bound to annotation's closing `)` so that trailing
                    // comments after the annotated declaration are not captured
                    // inside the annotation parens.
                    parts.push(self.drain_trailing_doc_bounded(entry.span.end, span.end));
                }

                Doc::concat(vec![Doc::text("("), Doc::Concat(parts), Doc::text(")")])
            }
            Ast::Eof => Doc::Empty,
        }
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
        let prefix = match &decl.name {
            Some(name) => Doc::concat(vec![Doc::text("enum "), self.at(name), Doc::text(" ")]),
            None => Doc::text("enum "),
        };

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
                inner.push(self.drain_leading_doc(decl.span.end));
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

        let before_brace = self.drain_leading_doc(decl.brace_start);
        let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
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
                let pad = name_pads[i].saturating_sub(v.name.len());
                if pad > 0 {
                    parts.push(Doc::text(" ".repeat(pad)));
                }
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(v.assign_kw_start.unwrap_or(value.span().start)));
                parts.push(Doc::text("= "));
                parts.push(self.expr_with_leading(value));
            }

            if i != last_idx || decl.trailing_comma {
                parts.push(Doc::text(","));
            }

            let trailing = self.drain_trailing_doc(v.span.end);
            parts.push(trailing);

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
            inner.push(self.drain_leading_doc(decl.span.end));
        }

        Doc::concat(vec![
            header,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn function_to_doc(&self, decl: &FunctionDecl) -> Doc {
        let mut parts: Vec<Doc> = Vec::new();

        if let Some(vis) = &decl.visibility {
            parts.push(self.visibility_to_doc(vis));
        }

        if decl.is_static {
            parts.push(Doc::text("static "));
        }

        parts.push(Doc::text("function "));
        parts.push(self.at(&decl.name));

        let items = decl
            .parameters
            .iter()
            .map(|arg| {
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
                let trailing_pos = arg.type_.as_ref().map_or(arg.span.end, |t| t.span.end);
                let trailing_is_line_comment =
                    self.has_trailing_line_comment_bounded(trailing_pos, usize::MAX);

                ListItem {
                    content: Doc::Concat(arg_parts),
                    trailing: self.drain_trailing_doc(trailing_pos),
                    trailing_is_line_comment,
                }
            })
            .collect();

        parts.push(self.format_list(
            "(",
            ")",
            items,
            &[],
            decl.parameters_trailing_comma,
            Doc::Empty,
            false,
        ));

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

    fn var_stmt_to_doc(&self, var_decl: &VarDecl) -> Doc {
        if self.wrap_multi_bindings && var_decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                var_decl.visibility.as_ref(),
                var_decl.is_static,
                "var",
                &var_decl.bindings,
            );
        }

        let mut parts = vec![self.var_decl_to_doc(var_decl)];
        self.push_before_semi(&mut parts, var_decl.semi_pos);

        Doc::Concat(parts)
    }

    fn const_decl_to_doc(&self, decl: &ConstDecl) -> Doc {
        if self.wrap_multi_bindings && decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                decl.visibility.as_ref(),
                decl.is_static,
                "const",
                &decl.bindings,
            );
        }

        let mut parts = self.decl_keyword(decl.visibility.as_ref(), decl.is_static, "const");
        self.push_bindings(&mut parts, &decl.bindings);
        self.push_before_semi(&mut parts, decl.semi_pos);

        Doc::Concat(parts)
    }

    fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts = self.decl_keyword(var.visibility.as_ref(), var.is_static, "var");
        self.push_bindings(&mut parts, &var.bindings);

        Doc::Concat(parts)
    }

    fn wrapped_bindings_decl(
        &self,
        visibility: Option<&Visibility>,
        is_static: bool,
        keyword: &str,
        bindings: &[Binding],
    ) -> Doc {
        let mut parts: Vec<Doc> = Vec::new();
        if let Some(vis) = visibility {
            parts.push(self.visibility_to_doc(vis));
        }

        if is_static {
            parts.push(Doc::text("static "));
        }

        parts.push(Doc::text(keyword.to_string()));

        let mut indented = vec![Doc::HardLine];
        for (i, b) in bindings.iter().enumerate() {
            if i > 0 {
                indented.push(Doc::text(","));
                indented.push(Doc::HardLine);
            }

            indented.push(self.binding_to_doc(b));
        }

        parts.push(Doc::Indent(indented));
        parts.push(Doc::text(";"));

        Doc::Concat(parts)
    }

    fn decl_keyword(
        &self,
        visibility: Option<&Visibility>,
        is_static: bool,
        keyword: &str,
    ) -> Vec<Doc> {
        let mut parts: Vec<Doc> = Vec::new();
        if let Some(vis) = visibility {
            parts.push(self.visibility_to_doc(vis));
        }

        if is_static {
            parts.push(Doc::text("static "));
        }

        parts.push(Doc::text(format!("{keyword} ")));
        parts
    }

    fn push_bindings(&self, parts: &mut Vec<Doc>, bindings: &[Binding]) {
        for (i, b) in bindings.iter().enumerate() {
            if i > 0 {
                parts.push(Doc::text(", "));
            }

            parts.push(self.binding_to_doc(b));
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
            parts.push(Doc::text(" "));
            parts.push(self.drain_leading_doc(b.assign_kw_start.unwrap_or(init.span().start)));
            parts.push(Doc::text("= "));
            parts.push(self.expr_with_leading(init));
        }

        Doc::Concat(parts)
    }

    fn visibility_to_doc(&self, vis: &Visibility) -> Doc {
        Doc::text(match vis {
            Visibility::Private => "private ",
            Visibility::Protected => "protected ",
            Visibility::Hidden => "hidden ",
            Visibility::Public => "public ",
        })
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
            TypeKind::Dict {
                entries,
                trailing_comma,
            } => self.inline_dict_type_to_doc(entries, *trailing_comma, suffix),
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
                let mut parts = vec![Doc::text(format!("{name}("))];
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }

                    parts.push(Doc::text(&arg.name.node));

                    if let Some(t) = &arg.type_ {
                        parts.push(Doc::text(" as "));
                        parts.push(self.type_to_doc(t));
                    }
                }

                parts.push(Doc::text(")"));

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
        for alt in &ty.alternatives {
            parts.push(Doc::text(" or "));
            parts.push(self.type_to_doc(alt));
        }

        Doc::Concat(parts)
    }

    fn inline_dict_type_to_doc(
        &self,
        entries: &[DictTypeEntry],
        trailing_comma: bool,
        suffix: &str,
    ) -> Doc {
        if entries.is_empty() {
            return Doc::text(format!("{{}}{suffix}"));
        }

        let entry_doc = |entry: &DictTypeEntry| {
            let leading = self.drain_leading_doc(entry.span.start);
            let key = match &entry.key {
                DictTypeKey::Symbol(s) => format!(":{s}"),
                DictTypeKey::String(s) => format!("\"{s}\""),
            };
            let type_doc = self.type_to_doc(&entry.value_type);
            let inner = Doc::concat(vec![Doc::text(key), Doc::text(" as "), type_doc]);

            if matches!(leading, Doc::Empty) {
                inner
            } else {
                Doc::concat(vec![leading, inner])
            }
        };

        if trailing_comma {
            let mut inner = Vec::new();
            for (i, entry) in entries.iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::text(","));
                    inner.push(Doc::HardLine);
                }
                inner.push(entry_doc(entry));
            }
            inner.push(Doc::text(","));

            return Doc::concat(vec![
                Doc::text("{"),
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text(format!("}}{suffix}")),
            ]);
        }

        let mut inner = Vec::new();
        for (i, entry) in entries.iter().enumerate() {
            if i > 0 {
                inner.push(Doc::text(","));
                inner.push(Doc::Line);
            }
            inner.push(entry_doc(entry));
        }

        Doc::Group(vec![
            Doc::text("{"),
            Doc::Indent(vec![Doc::SoftLine, Doc::Concat(inner)]),
            Doc::SoftLine,
            Doc::text(format!("}}{suffix}")),
        ])
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
                let mut parts = vec![Doc::text("function "), self.at(&m.name), Doc::text("(")];

                for (i, arg) in m.args.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }
                    parts.push(self.drain_leading_doc(arg.span.start));
                    parts.push(Doc::text(&arg.name.node));
                    if let Some(ty) = &arg.type_ {
                        parts.push(Doc::text(" "));
                        parts
                            .push(self.drain_leading_doc(arg.as_kw_start.unwrap_or(ty.span.start)));
                        parts.push(Doc::text("as "));
                        parts.push(self.type_to_doc(ty));
                    }
                }

                parts.push(Doc::text(")"));
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
        cond: &Expr,
        paren_close: usize,
        brace_start: usize,
    ) -> Doc {
        let wrappable = matches!(cond, Expr::Binary(_));
        let cond_doc = self.condition_to_doc(cond);

        let cond_end = cond.span().end;

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

            let mut group_parts = vec![
                Doc::text(format!("{keyword} (")),
                Doc::Indent(vec![cond_doc]),
                cond_trailing,
            ];

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
            // Non-binary condition: only drain inside-paren trailing (e.g. `if (x /* INSIDE */)`).
            // Comments after `)` are handled by the body or statement-level trailing drain.
            let cond_trailing = self.drain_trailing_doc_bounded(cond_end, paren_close);

            Doc::concat(vec![
                Doc::text(format!("{keyword} (")),
                cond_doc,
                cond_trailing,
                Doc::text(") "),
            ])
        }
    }

    fn if_stmt_to_doc(&self, s: &IfStmt) -> Doc {
        let header = self.paren_condition_header(
            "if",
            &s.condition.inner,
            s.condition.close,
            s.then_branch.span.start,
        );
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
                let next_start = operands[i + 1].span().start;

                // When a `//` line comment appears anywhere between this operand and
                // the next (before OR after the operator), keep the classic "glue the
                // operator to the comment" layout: `left op // comment\n right`.
                // This preserves the comment's position relative to the operator, which
                // matches rustfmt/gofmt behaviour for trailing line comments.
                //
                // When only block comments are present, split at `op_pos`:
                // comments before the operator stay with the left operand, and the
                // pretty-printer decides where to wrap (before the operator).
                let has_line_anywhere =
                    self.has_trailing_line_comment_bounded(span.end, next_start);

                if has_line_anywhere {
                    let trailing = self.drain_trailing_doc_bounded(span.end, next_start);
                    parts.push(Doc::text(format!(" {}", operators::binary_op(op))));
                    parts.push(trailing);
                    parts.push(Doc::HardLine);
                } else {
                    let trailing = self.drain_trailing_doc_bounded(span.end, op_pos);
                    parts.push(trailing);
                    parts.push(Doc::Line);
                    parts.push(Doc::text(format!("{} ", operators::binary_op(op))));
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

        // Drain before-brace comments before building body so they aren't
        // stolen by drain_leading_doc inside the case-rendering loop.
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
            header.push(self.drain_after_open_brace(case.label_span.end, case.span.end));
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
            let case_trailing = self.drain_trailing_doc(trailing_anchor);
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
            self.paren_condition_header(
                "switch",
                &s.discriminant.inner,
                s.discriminant.close,
                s.brace_start,
            ),
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

    fn stmt_to_doc(&self, stmt: &Stmt) -> Doc {
        self.stmt_to_doc_bounded(stmt, usize::MAX)
    }

    /// Like [`stmt_to_doc`] but caps the trailing comment drain at `max_pos`.
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
            Stmt::Block(block) => Doc::concat(vec![Doc::text("{"), self.block_inner_to_doc(block)]),
            Stmt::If(s) => self.if_stmt_to_doc(s),
            Stmt::While(s) => Doc::concat(vec![
                self.paren_condition_header(
                    "while",
                    &s.condition.inner,
                    s.condition.close,
                    s.body.span.start,
                ),
                self.block_body_to_doc(&s.body),
            ]),
            Stmt::DoWhile(s) => Doc::concat(vec![
                Doc::text("do "),
                self.block_body_to_doc(&s.body),
                Doc::text(" while ("),
                self.expr_with_leading(&s.condition),
                Doc::text(");"),
            ]),
            Stmt::For(s) => {
                let init_doc = match &s.header.inner.init {
                    None => Doc::Empty,
                    Some(ForInit::Var(v)) => Doc::concat(vec![
                        self.drain_leading_doc(v.span.start),
                        self.var_decl_to_doc(v),
                    ]),
                    Some(ForInit::Expr(exprs)) => self.expr_list_to_doc(exprs),
                };
                let for_close = s.header.close;
                let cond_doc = s
                    .header
                    .inner
                    .condition
                    .as_ref()
                    .map(|e| self.expr_with_leading_bounded(e, for_close))
                    .unwrap_or(Doc::Empty);
                let update_doc = s
                    .header
                    .inner
                    .update
                    .as_ref()
                    .map(|exprs| self.expr_list_to_doc(exprs))
                    .unwrap_or(Doc::Empty);

                let mut parts = vec![Doc::text("for ("), init_doc];
                self.push_before_semi(&mut parts, s.header.inner.first_semi);
                parts.push(Doc::text(" "));
                parts.push(cond_doc);
                self.push_before_semi(&mut parts, s.header.inner.second_semi);
                parts.push(Doc::text(" "));
                parts.push(update_doc);

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
                    let mut parts = vec![Doc::text("return "), self.expr_with_leading(v)];
                    self.push_before_semi(&mut parts, s.semi_pos);

                    Doc::Concat(parts)
                }
            },
            Stmt::Break(_) => Doc::text("break;"),
            Stmt::Continue(_) => Doc::text("continue;"),
            Stmt::Throw(s) => {
                let mut parts = vec![Doc::text("throw "), self.expr_with_leading(&s.value)];
                self.push_before_semi(&mut parts, s.semi_pos);

                Doc::Concat(parts)
            }
            Stmt::Switch(s) => self.switch_stmt_to_doc(s),
            Stmt::Try(s) => self.try_stmt_to_doc(s),
            Stmt::Var(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Stmt::Expr(e) => Doc::concat(vec![self.expr_inner_to_doc(e), Doc::text(";")]),
        }
    }

    fn block_inner_to_doc(&self, block: &BlockStmt) -> Doc {
        let inner = self.stmts_to_doc(&block.stmts, block.span);
        if block.stmts.is_empty() && matches!(inner, Doc::Empty) {
            return Doc::text("}");
        }

        Doc::concat(vec![
            Doc::Indent(vec![Doc::HardLine, inner]),
            Doc::HardLine,
            Doc::text("}"),
        ])
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
            Expr::Ternary(e) => Doc::Group(vec![
                self.expr_with_leading(&e.condition),
                Doc::Indent(vec![
                    Doc::Line,
                    Doc::text("? "),
                    self.expr_with_leading(&e.then_expr),
                    Doc::Line,
                    Doc::text(": "),
                    self.expr_with_leading(&e.else_expr),
                ]),
            ]),
            Expr::Assign(e) => Doc::concat(vec![
                self.expr_with_leading(&e.target),
                Doc::text(format!(" {} ", operators::assign_op(&e.operator))),
                self.expr_with_leading(&e.value),
            ]),
            Expr::Call(e) => {
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
                let after_open = self.drain_after_open_brace(e.args_open, first_arg_start);
                Doc::concat(vec![
                    self.expr_with_leading(&e.callee),
                    self.format_list(
                        "(",
                        ")",
                        self.call_args_to_items(&e.args, e.span.end),
                        &[],
                        e.args_trailing_comma,
                        after_open,
                        after_open_force_newline,
                    ),
                ])
            }
            Expr::Member(e) => Doc::concat(vec![
                self.expr_with_leading(&e.object),
                Doc::text(format!(".{}", e.property)),
            ]),
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
                let first_arg_start = e
                    .args
                    .first()
                    .map(|a| a.value.span().start)
                    .unwrap_or(e.span.end);
                let after_open_force_newline = e
                    .args_open
                    .is_some_and(|start| self.after_open_has_line_comment(start, first_arg_start));
                let after_open = e
                    .args_open
                    .map(|start| self.drain_after_open_brace(start, first_arg_start))
                    .unwrap_or(Doc::Empty);

                Doc::concat(vec![
                    Doc::text(format!("new {}", e.class)),
                    self.format_list(
                        "(",
                        ")",
                        self.call_args_to_items(&e.args, e.span.end),
                        &[],
                        e.args_trailing_comma,
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
                LiteralValue::Number(v) => v.clone(),
                LiteralValue::Long(v) => format!("{v}l"),
                LiteralValue::Hex(s) => format!("0x{s}"),
                LiteralValue::HexLong(s) => format!("0x{s}l"),
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

    fn call_args_to_items(&self, args: &[CallArg], args_close: usize) -> Vec<ListItem> {
        args.iter()
            .enumerate()
            .map(|(i, a)| {
                let value_span = *a.value.span();
                let next_arg_start = args
                    .get(i + 1)
                    .map(|next| next.value.span().start)
                    .unwrap_or(args_close);
                let trailing_is_line_comment =
                    self.has_trailing_line_comment_bounded(value_span.end, next_arg_start);
                ListItem {
                    content: self.expr_with_leading_bounded(&a.value, next_arg_start),
                    trailing: self.drain_trailing_doc_bounded(value_span.end, next_arg_start),
                    trailing_is_line_comment,
                }
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

        let must_break = e.trailing_comma || self.has_comments_in(e.span);

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
                    trailing: self.drain_trailing_doc_bounded(value_span.end, next_start),
                    trailing_is_line_comment: false,
                }
            })
            .collect();

        self.format_list("[", "]", items, &[], false, Doc::Empty, false)
    }

    fn format_array_multiline(&self, e: &ArrayExpr) -> Doc {
        self.format_collection_multiline(
            e.span,
            &e.entries,
            e.trailing_comma,
            "[",
            "]",
            |entry| entry.value.span().start,
            |entry| &entry.value,
            |_| Doc::Empty,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn format_list(
        &self,
        open: &str,
        close: &str,
        items: Vec<ListItem>,
        tail_comments: &[Stmt],
        trailing_comma: bool,
        after_open: Doc,
        after_open_force_newline: bool,
    ) -> Doc {
        let has_after_open = !matches!(&after_open, Doc::Empty);

        if items.is_empty() && tail_comments.is_empty() && !has_after_open {
            return Doc::text(format!("{open}{close}"));
        }

        if items.is_empty() && !has_after_open {
            let comment_docs = tail_comments
                .iter()
                .enumerate()
                .flat_map(|(i, c)| {
                    let prefix = if i == 0 { Doc::Empty } else { Doc::HardLine };
                    vec![prefix, self.stmt_to_doc(c)]
                })
                .collect();

            return Doc::concat(vec![
                Doc::text(open),
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(comment_docs)]),
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
            || !tail_comments.is_empty()
            || items.iter().any(|i| i.trailing_is_line_comment);

        if force_multiline {
            let mut inner = Vec::new();
            let last_idx = items.len().saturating_sub(1);
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::HardLine);
                }

                inner.push(item.content);

                if i != last_idx || trailing_comma {
                    inner.push(Doc::text(","));
                }

                if !matches!(item.trailing, Doc::Empty) {
                    inner.push(item.trailing);
                }
            }

            for c in tail_comments {
                inner.push(Doc::HardLine);
                inner.push(self.stmt_to_doc(c));
            }

            return Doc::concat(vec![
                Doc::text(open),
                after_open,
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text(close),
            ]);
        }

        // Block-comments-only (or no comments): use a group so the
        // pretty-printer keeps everything flat when it fits.
        // Trailing goes AFTER the comma (same order as force_multiline) so
        // `a, /* c */ b` rather than `a /* c */, b`.
        let mut inner = Vec::new();
        let last_idx = items.len().saturating_sub(1);
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                inner.push(Doc::Line);
            }

            inner.push(item.content);

            if i != last_idx {
                inner.push(Doc::text(","));
            }

            if !matches!(item.trailing, Doc::Empty) {
                inner.push(item.trailing);
            }
        }

        // When there's a block comment after the opening delimiter, separate
        // it from the content with `Doc::Line` (" " flat, newline+indent
        // expanded) so flat mode gives `(/* c */ x)` not `(/* c */x)`.
        let indent_start = if has_after_open {
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

        let must_break = e.trailing_comma || self.has_comments_in(e.span);

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
            e.trailing_comma,
            "{",
            "}",
            |entry| entry.key.span().start,
            |entry| &entry.value,
            |entry| {
                let key_doc = self.expr_with_leading(&entry.key);
                let padding = if aligned {
                    expr_key_width(&entry.key)
                        .map(|w| " ".repeat(max_key_width.saturating_sub(w)))
                        .unwrap_or_default()
                } else {
                    String::new()
                };

                Doc::concat(vec![key_doc, Doc::Text(padding), Doc::text(" => ")])
            },
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn format_collection_multiline<E>(
        &self,
        span: Span,
        entries: &[E],
        trailing_comma: bool,
        open: &str,
        close: &str,
        start_of: impl Fn(&E) -> usize,
        value_of: impl Fn(&E) -> &Expr,
        head_of: impl Fn(&E) -> Doc,
    ) -> Doc {
        let last_idx = entries.len().saturating_sub(1);
        let mut inner: Vec<Doc> = Vec::new();
        let mut prev_end: Option<usize> = None;

        for (i, entry) in entries.iter().enumerate() {
            let entry_start = start_of(entry);

            // Drain any standalone comments before this entry — but only those on a
            // different line. A block comment on the same line as the entry is an inline
            // prefix (e.g. `/* INFO */ :key => val`) and is handled by head_of / expr_with_leading.
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

            let value = value_of(entry);
            let value_span = *value.span();

            // Only capture trailing comments up to the next entry's start (or
            // the collection close). This prevents stealing a comment from a
            // later entry or from outside the collection.
            let next_entry_start = entries.get(i + 1).map(&start_of).unwrap_or(span.end);

            let mut parts = vec![
                head_of(entry),
                self.expr_with_leading_bounded(value, next_entry_start),
            ];
            if i != last_idx || trailing_comma {
                parts.push(Doc::text(","));
            }
            let has_trailing_line =
                self.has_trailing_line_comment_bounded(value_span.end, next_entry_start);
            let value_trailing = self.drain_trailing_doc_bounded(value_span.end, next_entry_start);
            if !matches!(value_trailing, Doc::Empty) {
                parts.push(value_trailing);
            }

            let eff_end = self.comment_cursor.borrow().last_end().max(value_span.end);

            inner.push(Doc::Concat(parts));
            prev_end = Some(eff_end);

            let _ = has_trailing_line;
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
                trailing: Doc::Empty,
                trailing_is_line_comment: false,
            })
            .collect();

        self.format_list("{", "}", items, &[], e.trailing_comma, Doc::Empty, false)
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
        Expr::Ident(e) => Some(e.name.len()),
        Expr::Me(_) => Some("me".len()),
        Expr::Self_(_) => Some("self".len()),
        Expr::Bling(_) => Some(1),
        Expr::Member(e) => Some(expr_key_width(&e.object)? + 1 + e.property.len()),
        Expr::Lit(e) => Some(match &e.value {
            LiteralValue::Number(v) => v.len(),
            LiteralValue::Long(v) => v.to_string().len() + 1,
            LiteralValue::Hex(s) => 2 + s.len(),
            LiteralValue::HexLong(s) => 2 + s.len() + 1,
            LiteralValue::Float(lit) => format_float_lit(lit).len(),
            LiteralValue::Double(lit) => format_double_lit(lit).len(),
            LiteralValue::String(v) => 2 + v.len(),
            LiteralValue::Char(v) => 3 + v.len(),
            LiteralValue::Boolean(v) => v.to_string().len(),
            LiteralValue::Symbol(v) => 1 + v.len(),
            LiteralValue::Null => "null".len(),
            LiteralValue::NaN => "NaN".len(),
        }),
        _ => None,
    }
}

/// Re-emit a [`FloatLit`] in the exact source form recorded by the lexer.
fn format_float_lit(lit: &FloatLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    if lit.has_suffix {
        format!("{body}f")
    } else {
        body
    }
}

/// Re-emit a [`DoubleLit`] preserving its source style.
fn format_double_lit(lit: &DoubleLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    format!("{body}d")
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
            max_name = max_name.max(variants[i].name.len());
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
                .filter_map(|k| analyzed[k].map(|(_, code_end, _)| code_end))
                .max()
                .unwrap_or(0);

            for k in i..j {
                let Some((_, code_end, comment_start)) = analyzed[k] else {
                    continue;
                };
                let line = lines[k];
                let code = &line[..code_end];
                let comment = &line[comment_start..];
                let pad = max_code - code_end;
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
