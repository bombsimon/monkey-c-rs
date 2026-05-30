pub mod doc;
mod operators;

use doc::{Doc, render};
use monkey_c_parser::ast::{
    ArrayExpr, Ast, BinaryOperator, Binding, BlockStmt, CallArg, CaseLabel, CommentStmt, ConstDecl,
    DictExpr, DictTypeEntry, DictTypeKey, DoubleLit, ElseBranch, EnumDecl, EnumVariant, Expr,
    FloatLit, ForInit, FunctionDecl, IfStmt, InterfaceMember, LiteralValue, ParseOutput, Span,
    Stmt, SwitchStmt, TryStmt, Type, TypeKind, UnaryOperator, VarDecl, Visibility,
};
use monkey_c_parser::comments::{CommentsMap, DanglingPlacement, attach_comments};
use monkey_c_parser::line_index::LineIndex;
use std::cell::RefCell;

/// An item in a delimited list (array entry, dict pair, call arg, etc.) along
/// with any comments that trail it inside the bracketed list. Used by
/// [`Formatter::format_list`].
struct ListItem {
    content: Doc,
    trailing_comments: Vec<Doc>,
}

/// Sortable item used by [`Formatter::stmts_to_doc`] /
/// [`Formatter::decls_to_doc`] to interleave block-level standalone comments
/// with statements / declarations in source order.
enum BodyItem<'a> {
    Decl(&'a Ast, Span),
    Stmt(&'a Stmt, Span),
    Comment(CommentStmt),
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
    /// operators column-aligned. Currently covers:
    /// - dict literals — `=>` between key and value
    /// - enum variants — `=` between name and explicit value
    align_pairs: bool,
    /// When `true`, multi-binding `var`/`const` declarations break each
    /// binding onto its own indented line. See [`Self::with_decl_wrap`].
    wrap_multi_bindings: bool,
    /// Built once at the start of [`Self::format`] from the [`ParseOutput`]'s
    /// comment table. Render methods query it via
    /// [`Self::leading_doc`] / [`Self::trailing_doc`] / [`Self::dangling`].
    comments: RefCell<CommentsMap>,
}

impl Formatter {
    /// Create a formatter for `source`.
    ///
    /// The source string is used to build a [`LineIndex`] for blank-line
    /// preservation — the formatter does not re-emit source text verbatim.
    pub fn new(source: impl AsRef<str>) -> Self {
        let source = source.as_ref();

        Self {
            line_index: LineIndex::new(source),
            line_width: 100,
            align_pairs: false,
            wrap_multi_bindings: false,
            comments: RefCell::new(CommentsMap::new()),
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    /// Enable column-aligned separators across related entries (opt-in).
    ///
    /// Currently covers:
    /// - dict literals — `=>` between key and value (`{ :a => 1, :bb => 2 }`)
    /// - enum variants — `=` between name and explicit value
    ///
    /// When set, the affected constructs are rendered multi-line with their
    /// names padded so the separators form a vertical column.
    pub fn with_alignment(mut self) -> Self {
        self.align_pairs = true;
        self
    }

    /// Enable per-binding wrapping for multi-binding `var`/`const` declarations
    /// (opt-in). When set, a declaration with two or more bindings is broken
    /// so the keyword sits alone and each binding lives on its own indented
    /// line. For-loop init clauses keep their flat form regardless.
    pub fn with_decl_wrap(mut self) -> Self {
        self.wrap_multi_bindings = true;
        self
    }

    /// Format a parsed file and return the result as a `String`.
    pub fn format(&self, output: &ParseOutput) -> String {
        *self.comments.borrow_mut() =
            attach_comments(&output.ast, &output.comments, &self.line_index);

        let doc = self.ast_to_doc(&output.ast);
        let rendered = render(&doc, self.line_width);
        let mut aligned = align_trailing_comments(&rendered);

        // Always end with `\n` according to POSIX convention.
        if !aligned.ends_with('\n') {
            aligned.push('\n');
        }

        aligned
    }

    /// True if any attached comment — line or block, in any role — has a span
    /// strictly inside `span`. When the source spans multiple lines and has
    /// any comment, the array/dict renderer refuses to collapse to one line.
    fn has_comments_in(&self, span: Span) -> bool {
        self.comments
            .borrow()
            .iter()
            .any(|c| c.span.start >= span.start && c.span.end <= span.end)
    }

    /// True if `span` covers more than one source line.
    fn spans_multiple_lines(&self, span: Span) -> bool {
        self.line_index.line(span.start as u32) != self.line_index.line(span.end as u32)
    }

    /// Comments attached to render just *before* the node at `span`. Returns
    /// owned `CommentStmt`s so the caller can format them as line or block
    /// comments without holding a borrow on `self.comments`.
    fn leading(&self, span: Span) -> Vec<CommentStmt> {
        self.comments.borrow().leading(span).cloned().collect()
    }

    /// Comments attached to render just *after* the node at `span`.
    fn trailing(&self, span: Span) -> Vec<CommentStmt> {
        self.comments.borrow().trailing(span).cloned().collect()
    }

    /// Comments dangling in the "between the header and `{`" slot of `span`.
    /// Returns `Doc::Empty` when there are no such comments. When non-empty
    /// the output ends with a space, so it slots into `header + bb + block_body`
    /// without double-spacing (`if (x) ` + `/* C */ ` + `{body}`).
    fn before_bracket_doc(&self, span: Span) -> Doc {
        let comments = self.dangling(span, DanglingPlacement::BeforeBracket);
        if comments.is_empty() {
            return Doc::Empty;
        }

        let mut parts = Vec::new();
        for c in &comments {
            parts.push(self.comment_to_doc(c));
            parts.push(Doc::text(" "));
        }

        Doc::Concat(parts)
    }

    /// Comments inside the node at `span` but not adjacent to any direct
    /// child — emitted at a kind-specific position by the render method.
    fn dangling(&self, span: Span, placement: DanglingPlacement) -> Vec<CommentStmt> {
        self.comments
            .borrow()
            .dangling(span, placement)
            .cloned()
            .collect()
    }

    /// Render a comment as a [`Doc`]. Line comments become `// text`; block
    /// comments are routed through [`Self::block_comment_to_doc`] so
    /// multi-line `/* … */` blocks align their closing delimiter.
    fn comment_to_doc(&self, c: &CommentStmt) -> Doc {
        if c.is_block {
            self.block_comment_to_doc(&c.text)
        } else {
            Doc::text(format!("//{}", c.text))
        }
    }

    /// Render leading comments as a prefix. For inline block comments adjacent
    /// to the node on the same source line we emit `<comment> <node>`; for
    /// every other case (line comments, block comments on a separate line) we
    /// emit `<comment>\n<node>`.
    fn leading_doc(&self, span: Span) -> Doc {
        let comments = self.leading(span);
        if comments.is_empty() {
            return Doc::Empty;
        }

        let node_line = self.line_index.line_col(span.start as u32).line;
        let mut parts = Vec::new();
        for c in &comments {
            let comment_end_line = self
                .line_index
                .line_col(c.span.end.saturating_sub(1) as u32)
                .line;

            parts.push(self.comment_to_doc(c));

            if c.is_block && comment_end_line == node_line {
                parts.push(Doc::text(" "));
            } else {
                parts.push(Doc::HardLine);
            }
        }

        Doc::Concat(parts)
    }

    /// Render trailing comments as a suffix. Inline block comments on the
    /// same source line emit as ` <comment>`; everything else emits on a new
    /// line.
    fn trailing_doc(&self, span: Span) -> Doc {
        let comments = self.trailing(span);
        if comments.is_empty() {
            return Doc::Empty;
        }

        let node_end_line = self
            .line_index
            .line_col(span.end.saturating_sub(1) as u32)
            .line;

        let mut parts = Vec::new();
        let mut last_line = node_end_line;
        for c in &comments {
            let comment_start_line = self.line_index.line_col(c.span.start as u32).line;
            if comment_start_line == last_line {
                parts.push(Doc::text(" "));
            } else {
                parts.push(Doc::HardLine);
            }

            parts.push(self.comment_to_doc(c));
            last_line = self
                .line_index
                .line_col(c.span.end.saturating_sub(1) as u32)
                .line;
        }

        Doc::Concat(parts)
    }

    fn ast_to_doc(&self, ast: &Ast) -> Doc {
        match ast {
            Ast::Document(nodes, span) => self.decls_to_doc(nodes, *span),
            Ast::Import(decl) => Doc::concat(vec![
                Doc::text("import "),
                Doc::text(&decl.name),
                Doc::text(";"),
            ]),
            Ast::Using(decl) => Doc::concat(vec![
                Doc::text("using "),
                Doc::text(&decl.name),
                decl.alias
                    .as_ref()
                    .map(|a| Doc::concat(vec![Doc::text(" as "), Doc::text(a)]))
                    .unwrap_or(Doc::Empty),
                Doc::text(";"),
            ]),
            Ast::Typedef(decl) => Doc::concat(vec![
                Doc::text(format!("typedef {} as ", decl.name)),
                self.type_to_doc(&decl.type_),
                Doc::text(";"),
            ]),
            Ast::Module(decl) => {
                let before_bracket = self.before_bracket_doc(decl.span);
                let header = Doc::concat(vec![
                    Doc::text(format!("module {} ", decl.name)),
                    before_bracket,
                    Doc::text("{"),
                ]);

                let inner = self.decls_to_doc(&decl.body, decl.span);
                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![header, Doc::text("}")]);
                }

                Doc::concat(vec![
                    header,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Class(decl) => {
                let before_bracket = self.before_bracket_doc(decl.span);
                let header = Doc::concat(vec![
                    Doc::text(format!("class {}", decl.name)),
                    decl.extends
                        .as_ref()
                        .map(|e| Doc::text(format!(" extends {e}")))
                        .unwrap_or(Doc::Empty),
                    Doc::text(" "),
                    before_bracket,
                    Doc::text("{"),
                ]);

                let inner = self.decls_to_doc(&decl.body, decl.span);
                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![header, Doc::text("}")]);
                }

                Doc::concat(vec![
                    header,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Function(decl) => self.function_to_doc(decl),
            Ast::Enum(decl) => self.enum_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Annotation(entries, _) => {
                let parts: Vec<Doc> = entries
                    .iter()
                    .enumerate()
                    .flat_map(|(i, entry)| {
                        let mut bits = Vec::new();
                        if i > 0 {
                            bits.push(Doc::text(", "));
                        }

                        bits.push(Doc::text(format!(":{}", entry.name)));
                        if !entry.args.is_empty() {
                            bits.push(Doc::text("("));
                            for (j, arg) in entry.args.iter().enumerate() {
                                if j > 0 {
                                    bits.push(Doc::text(", "));
                                }

                                bits.push(self.expr_to_doc(arg));
                            }

                            bits.push(Doc::text(")"));
                        }

                        bits
                    })
                    .collect();
                Doc::concat(vec![Doc::text("("), Doc::Concat(parts), Doc::text(")")])
            }
            Ast::Eof => Doc::Empty,
        }
    }

    /// Render a sequence of declarations interleaved with any standalone
    /// comments dangling on the surrounding `container` (top-level document,
    /// module body, class body). Blank lines between adjacent items are
    /// preserved via [`Self::gap_between_spans`]. Returns [`Doc::Empty`] when
    /// the body has neither declarations nor dangling comments so callers can
    /// short-circuit to `{}` rendering.
    fn decls_to_doc(&self, decls: &[Ast], container: Span) -> Doc {
        let dangling_comments = self.all_dangling(container);
        if decls.is_empty() && dangling_comments.is_empty() {
            return Doc::Empty;
        }

        let mut items: Vec<BodyItem> = decls
            .iter()
            .filter_map(|d| d.span().map(|s| BodyItem::Decl(d, *s)))
            .collect();
        for c in dangling_comments {
            items.push(BodyItem::Comment(c));
        }

        // Sort by the leading-comment-extended start so an item's leading
        // comments slot ahead of a previous item's trailing-end in the gap math.
        items.sort_by_key(|i| self.effective_start(i));

        let mut docs = Vec::new();
        let mut prev_end: Option<usize> = None;
        for item in &items {
            let start = self.effective_start(item);
            if let Some(end) = prev_end {
                let prev_span = Span { start: 0, end };
                let next_span = Span { start, end: start };
                docs.push(self.gap_between_spans(Some(&prev_span), Some(&next_span)));
            }

            match item {
                BodyItem::Decl(decl, decl_span) => {
                    docs.push(self.leading_doc(*decl_span));
                    docs.push(self.ast_to_doc(decl));
                    docs.push(self.trailing_doc(*decl_span));
                }
                BodyItem::Comment(c) => {
                    docs.push(self.comment_to_doc(c));
                }
                BodyItem::Stmt(_, _) => unreachable!("decls_to_doc only contains Decl items"),
            }

            prev_end = Some(self.effective_end(item));
        }

        Doc::Concat(docs)
    }

    /// Render an `enum { … }`. Always multi-line — enums are declarations
    /// and never collapse to a single line. A trailing comma in source is
    /// preserved on the last variant.
    fn enum_to_doc(&self, decl: &EnumDecl) -> Doc {
        let body_before_comments = self.dangling(decl.span, DanglingPlacement::BeforeFirstChild);
        let after_last_comments = self.dangling(decl.span, DanglingPlacement::AfterLastChild);
        let before_bracket = self.before_bracket_doc(decl.span);

        let header_with = |close: &str| {
            let prefix = match &decl.name {
                Some(name) => format!("enum {name} "),
                None => "enum ".to_string(),
            };
            Doc::concat(vec![
                Doc::text(prefix),
                before_bracket.clone(),
                Doc::text(close.to_string()),
            ])
        };

        let header = header_with("{");

        if decl.variants.is_empty() {
            let body_comments: Vec<_> = body_before_comments
                .into_iter()
                .chain(after_last_comments.clone())
                .collect();

            if body_comments.is_empty() {
                return header_with("{}");
            }

            let mut inner = Vec::new();
            for (i, c) in body_comments.iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::HardLine);
                }

                inner.push(self.comment_to_doc(c));
            }

            return Doc::concat(vec![
                header,
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text("}"),
            ]);
        }

        let last_idx = decl.variants.len() - 1;
        let mut inner = Vec::new();
        let mut prev_end: Option<usize> = None;

        let push_gap = |inner: &mut Vec<Doc>, prev_end: Option<usize>, next_start: usize| {
            let Some(pe) = prev_end else { return };
            let prev_span = Span { start: 0, end: pe };
            let next_span = Span {
                start: next_start,
                end: next_start,
            };
            inner.push(self.gap_between_spans(Some(&prev_span), Some(&next_span)));
        };

        for c in &body_before_comments {
            push_gap(&mut inner, prev_end, c.span.start);
            inner.push(self.comment_to_doc(c));
            prev_end = Some(c.span.end);
        }

        let name_pads = if self.align_pairs {
            enum_variant_name_pads(&decl.variants)
        } else {
            vec![0; decl.variants.len()]
        };

        for (i, v) in decl.variants.iter().enumerate() {
            let leading_comments = self.leading(v.span);
            let v_start = leading_comments
                .first()
                .map(|c| c.span.start)
                .unwrap_or(v.span.start);
            push_gap(&mut inner, prev_end, v_start);

            let mut parts = Vec::new();

            // Leading comments on the variant (`/* X */ Variant,`).
            let leading = self.leading_doc(v.span);
            if !matches!(leading, Doc::Empty) {
                parts.push(leading);
            }

            parts.push(Doc::text(&v.name));
            if let Some(value) = &v.value {
                let pad = name_pads[i].saturating_sub(v.name.len());
                if pad > 0 {
                    parts.push(Doc::text(" ".repeat(pad)));
                }
                parts.push(Doc::text(" = "));
                parts.push(self.expr_to_doc(value));
            }

            if i != last_idx || decl.trailing_comma {
                parts.push(Doc::text(","));
            }

            // Trailing comments on the variant. A comment written on the same
            // source line stays inline (`Variant, // X`); a comment on a
            // later line (e.g. commented-out code below the variant) keeps
            // its own line.
            let variant_end_line = self
                .line_index
                .line_col(v.span.end.saturating_sub(1) as u32)
                .line;
            let mut last_line = variant_end_line;
            let trailing_comments = self.trailing(v.span);
            for c in &trailing_comments {
                let comment_start_line = self.line_index.line_col(c.span.start as u32).line;
                if comment_start_line == last_line {
                    parts.push(Doc::text(" "));
                } else {
                    parts.push(Doc::HardLine);
                }
                parts.push(self.comment_to_doc(c));
                last_line = self
                    .line_index
                    .line_col(c.span.end.saturating_sub(1) as u32)
                    .line;
            }

            inner.push(Doc::Concat(parts));
            prev_end = Some(
                trailing_comments
                    .last()
                    .map(|c| c.span.end)
                    .unwrap_or(v.span.end),
            );
        }

        // Standalone comments between the last variant and `}` — they don't
        // belong to any variant, so render them on their own lines at the
        // variant indent.
        for c in &after_last_comments {
            push_gap(&mut inner, prev_end, c.span.start);
            inner.push(self.comment_to_doc(c));
            prev_end = Some(c.span.end);
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

        parts.push(Doc::text(format!("function {}", decl.name)));

        let items = decl
            .args
            .iter()
            .map(|arg| {
                let mut arg_parts = Vec::new();
                let leading = self.leading_doc(arg.span);
                if !matches!(leading, Doc::Empty) {
                    arg_parts.push(leading);
                }

                arg_parts.push(Doc::text(&arg.name));
                if let Some(ty) = &arg.type_ {
                    arg_parts.push(Doc::text(" as "));
                    arg_parts.push(self.type_to_doc(ty));
                }

                ListItem {
                    content: Doc::Concat(arg_parts),
                    trailing_comments: self
                        .trailing(arg.span)
                        .into_iter()
                        .map(|c| self.comment_to_doc(&c))
                        .collect(),
                }
            })
            .collect();

        parts.push(self.format_list("(", ")", items, &[], decl.args_trailing_comma));

        if let Some(ret) = &decl.returns {
            parts.push(Doc::text(" as "));
            parts.push(self.type_to_doc(ret));
        }

        parts.push(Doc::text(" "));
        parts.push(self.before_bracket_doc(decl.span));
        parts.push(self.block_body_to_doc(&decl.body));

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

        Doc::concat(vec![self.var_decl_to_doc(var_decl), Doc::text(";")])
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
        parts.push(Doc::text(";"));

        Doc::Concat(parts)
    }

    /// Render a `var` declaration without a trailing semicolon.
    ///
    /// Used for both `var` statements and `for`-loop init clauses. The
    /// statement form ([`Self::var_stmt_to_doc`]) substitutes the multi-binding
    /// wrapping path when [`Self::with_decl_wrap`] is on; for-loop inits keep
    /// the flat form regardless so the loop header stays on one line.
    fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts = self.decl_keyword(var.visibility.as_ref(), var.is_static, "var");
        self.push_bindings(&mut parts, &var.bindings);

        Doc::Concat(parts)
    }

    /// Render a `var`/`const` declaration where each binding lives on its own
    /// indented line, with the keyword left dangling on the line above. Used
    /// only when there are two or more bindings.
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

    /// `[visibility ][static ]<keyword> ` prefix shared by `var` and `const`.
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

    /// Append comma-separated `name [as Type] [= init]` bindings to `parts`.
    fn push_bindings(&self, parts: &mut Vec<Doc>, bindings: &[Binding]) {
        for (i, b) in bindings.iter().enumerate() {
            if i > 0 {
                parts.push(Doc::text(", "));
            }

            parts.push(self.binding_to_doc(b));
        }
    }

    /// Render a single `name [as Type] [= init]` binding.
    fn binding_to_doc(&self, b: &Binding) -> Doc {
        let mut parts = vec![Doc::text(&b.name)];
        if let Some(ty) = &b.type_ {
            parts.push(Doc::text(" as "));
            parts.push(self.type_to_doc(ty));
        }

        if let Some(init) = &b.initializer {
            parts.push(Doc::text(" = "));
            parts.push(self.expr_to_doc(init));
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

                    parts.push(Doc::text(arg.name.clone()));

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

    /// Render an inline dict type `{ :k as T, "k" as U }`. A source-level
    /// trailing comma forces multi-line; otherwise fits on one line when it
    /// can.
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
            let key = match &entry.key {
                DictTypeKey::Symbol(s) => format!(":{s}"),
                DictTypeKey::String(s) => format!("\"{}\"", escape_string(s)),
            };
            Doc::concat(vec![
                Doc::text(key),
                Doc::text(" as "),
                self.type_to_doc(&entry.value_type),
            ])
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

    /// Render an inline `interface { … }` type. Always multi-line; each
    /// member is followed by `;` and may carry leading/trailing comments
    /// anchored on its span. Comments between the opening `{` and the first
    /// member (or between the last member and `}`) attach as dangling
    /// comments on `body_span` and are interleaved here.
    fn interface_type_to_doc(
        &self,
        members: &[InterfaceMember],
        body_span: Span,
        suffix: &str,
    ) -> Doc {
        let before = self.dangling(body_span, DanglingPlacement::BeforeFirstChild);
        let after = self.dangling(body_span, DanglingPlacement::AfterLastChild);

        if members.is_empty() && before.is_empty() && after.is_empty() {
            return Doc::text(format!("interface {{}}{suffix}"));
        }

        let mut inner = Vec::new();
        for (i, c) in before.iter().enumerate() {
            if i > 0 {
                inner.push(Doc::HardLine);
            }
            inner.push(self.comment_to_doc(c));
        }

        for (i, member) in members.iter().enumerate() {
            if i > 0 || !before.is_empty() {
                inner.push(Doc::HardLine);
            }

            inner.push(self.interface_member_to_doc(member));
        }

        for c in &after {
            inner.push(Doc::HardLine);
            inner.push(self.comment_to_doc(c));
        }

        Doc::concat(vec![
            Doc::text("interface {"),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text(format!("}}{suffix}")),
        ])
    }

    fn interface_member_to_doc(&self, member: &InterfaceMember) -> Doc {
        let (span, body) = match member {
            InterfaceMember::Function(m) => {
                let mut parts = vec![
                    Doc::text("function "),
                    Doc::text(m.name.clone()),
                    Doc::text("("),
                ];

                for (i, arg) in m.args.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }
                    parts.push(Doc::text(arg.name.clone()));
                    if let Some(ty) = &arg.type_ {
                        parts.push(Doc::text(" as "));
                        parts.push(self.type_to_doc(ty));
                    }
                }

                parts.push(Doc::text(")"));
                if let Some(ret) = &m.returns {
                    parts.push(Doc::text(" as "));
                    parts.push(self.type_to_doc(ret));
                }

                parts.push(Doc::text(";"));
                (m.span, Doc::Concat(parts))
            }
            InterfaceMember::Variable(v) => (
                v.span,
                Doc::concat(vec![
                    Doc::text(format!("var {} as ", v.name)),
                    self.type_to_doc(&v.type_),
                    Doc::text(";"),
                ]),
            ),
        };

        let leading = self.leading_doc(span);
        let trailing = self.trailing_doc(span);
        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => body,
            _ => Doc::concat(vec![leading, body, trailing]),
        }
    }

    /// Render `<keyword> (cond)` followed by either a space (when `cond` fits
    /// flat) or a hard line break (when it wraps), so the trailing `{` lands
    /// on its own line in the wrapped case. Used by `if`, `while`, `switch`.
    ///
    /// For non-binary conditions there's nothing to wrap at, so a plain
    /// `<keyword> (cond) ` is emitted regardless of width.
    fn paren_condition_header(&self, keyword: &str, cond: &Expr) -> Doc {
        let wrappable = matches!(cond, Expr::Binary(_));
        let cond_doc = self.condition_to_doc(cond);
        if wrappable {
            Doc::Group(vec![
                Doc::text(format!("{keyword} (")),
                Doc::Indent(vec![cond_doc]),
                Doc::text(")"),
                Doc::flat_or_break(Doc::text(" "), Doc::HardLine),
            ])
        } else {
            Doc::concat(vec![
                Doc::text(format!("{keyword} (")),
                cond_doc,
                Doc::text(") "),
            ])
        }
    }

    fn if_stmt_to_doc(&self, s: &IfStmt) -> Doc {
        let header = self.paren_condition_header("if", &s.condition.inner);
        let mut parts = vec![
            header,
            self.before_bracket_doc(s.span),
            self.block_body_to_doc(&s.then_branch),
        ];

        // block_body_to_doc already emits the trailing on then_branch.span.
        // We only need to know whether a trailing was present so the `else`
        // clause moves to its own line.
        let has_trailing = !self.trailing(s.then_branch.span).is_empty();

        match &s.else_branch {
            None => {}
            Some(ElseBranch::Block(b)) => {
                if !has_trailing {
                    parts.push(Doc::text(" else"));
                } else {
                    parts.push(Doc::HardLine);
                    parts.push(Doc::text("else"));
                }

                parts.push(self.block_to_doc(b));
            }
            Some(ElseBranch::If(inner)) => {
                if !has_trailing {
                    parts.push(Doc::text(" else "));
                } else {
                    parts.push(Doc::HardLine);
                    parts.push(Doc::text("else "));
                }

                parts.push(self.if_stmt_to_doc(inner));
            }
        }

        Doc::Concat(parts)
    }

    /// Render an expression intended as a control-flow condition.
    ///
    /// For any top-level binary expression, the operands are joined by a
    /// breakable [`Doc::Line`] so an enclosing [`Doc::Group`] can wrap the
    /// chain at line width — operator leads the next line (Rust-style):
    /// `a + b\n    <= c`. Same-operator chains are flattened; mixed operators
    /// keep their precedence-determined nesting on the broken side.
    fn condition_to_doc(&self, expr: &Expr) -> Doc {
        if let Expr::Binary(e) = expr {
            let op_str = format!("{} ", operators::binary_op(&e.operator));
            let mut operands: Vec<&Expr> = Vec::new();
            collect_logical_chain(expr, &e.operator, &mut operands);

            let mut parts: Vec<Doc> = Vec::new();
            for (i, operand) in operands.iter().enumerate() {
                if i > 0 {
                    parts.push(Doc::Line);
                    parts.push(Doc::text(op_str.clone()));
                }

                parts.push(self.expr_to_doc(operand));
            }

            // The chain-flattening loop above emits each operand individually
            // and bypasses the outer Binary's `expr_to_doc` wrap — so any
            // trailing comment attached to the outer Binary's span needs to
            // be appended here. (`if (a == 0 /* T */)`: comment attaches to
            // `a == 0`, not `0`.)
            let trailing = self.trailing_doc(*expr.span());
            if !matches!(trailing, Doc::Empty) {
                parts.push(trailing);
            }

            return Doc::Concat(parts);
        }

        self.expr_to_doc(expr)
    }

    /// Like [`Formatter::block_to_doc`] but without a leading space — used when
    /// the caller has already chosen how to separate `{` from the preceding
    /// token (e.g. via [`Doc::FlatOrBreak`]).
    /// Render a `/* … */` comment. When the body spans multiple lines, the
    /// closing `*/` is placed on its own line at the current indent so it
    /// aligns with the opening `/*`. Single-line block comments are emitted
    /// inline unchanged.
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

    /// Render a `switch (…) { … }`. Each `case`/`default` arm lives at one
    /// indent inside the switch body; the statements that follow are
    /// indented one further. Leading comments are emitted before the arm's
    /// `case`/`default` keyword.
    fn switch_stmt_to_doc(&self, s: &SwitchStmt) -> Doc {
        let mut body = Vec::new();
        for (i, case) in s.cases.iter().enumerate() {
            if i > 0 {
                body.push(Doc::HardLine);
            }

            // Leading comments on the case via the CommentsMap.
            for c in self.leading(case.span) {
                body.push(self.comment_to_doc(&c));
                body.push(Doc::HardLine);
            }

            let mut header = vec![Doc::text("case ")];
            match &case.label {
                CaseLabel::Value(e) => {
                    header.push(self.expr_to_doc(e));
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
            // Trailing comments on the `:` boundary — same-line stays
            // inline (`case 1: // tag`); own-line indents to the case body.
            header.push(self.trailing_doc(case.label_span));
            body.push(Doc::Concat(header));

            let case_inner = self.stmts_to_doc(&case.stmts, case.span);
            if !case.stmts.is_empty() || !matches!(case_inner, Doc::Empty) {
                body.push(Doc::Indent(vec![Doc::HardLine, case_inner]));
            }
        }

        // Dangling comments after the last case but before `}` of the switch.
        for c in self.dangling(s.span, DanglingPlacement::AfterLastChild) {
            body.push(Doc::HardLine);
            body.push(self.comment_to_doc(&c));
        }

        Doc::concat(vec![
            self.paren_condition_header("switch", &s.discriminant.inner),
            self.before_bracket_doc(s.span),
            Doc::text("{"),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(body)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn try_stmt_to_doc(&self, s: &TryStmt) -> Doc {
        let mut parts = vec![
            Doc::text("try "),
            self.before_bracket_doc(s.span),
            self.block_body_to_doc(&s.body),
        ];

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

    fn block_body_to_doc(&self, block: &BlockStmt) -> Doc {
        let trailing = self.trailing_doc(block.span);
        let after_open = self.after_open_brace_doc(block.span);
        let inner = self.stmts_to_doc(&block.stmts, block.span);
        if block.stmts.is_empty() && matches!(inner, Doc::Empty) {
            return Doc::concat(vec![Doc::text("{"), after_open, Doc::text("}"), trailing]);
        }

        Doc::concat(vec![
            Doc::text("{"),
            after_open,
            Doc::Indent(vec![Doc::HardLine, inner]),
            Doc::HardLine,
            Doc::text("}"),
            trailing,
        ])
    }

    /// Render a block as ` {\n    …\n}` with the opening brace on the same line.
    fn block_to_doc(&self, block: &BlockStmt) -> Doc {
        let trailing = self.trailing_doc(block.span);
        let after_open = self.after_open_brace_doc(block.span);
        let inner = self.stmts_to_doc(&block.stmts, block.span);
        if block.stmts.is_empty() && matches!(inner, Doc::Empty) {
            return Doc::concat(vec![Doc::text(" {"), after_open, Doc::text("}"), trailing]);
        }

        Doc::concat(vec![
            Doc::text(" {"),
            after_open,
            Doc::Indent(vec![Doc::HardLine, inner]),
            Doc::HardLine,
            Doc::text("}"),
            trailing,
        ])
    }

    /// Render any `AfterOpenBrace` dangling comments on `block_span` as
    /// ` // C` (or ` /* C */`) — emitted immediately after the `{`, on the
    /// same source line.
    fn after_open_brace_doc(&self, block_span: Span) -> Doc {
        let comments = self.dangling(block_span, DanglingPlacement::AfterOpenBrace);
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

    /// Render a sequence of statements interleaved with any standalone
    /// comments dangling on the surrounding `container` block. Blank lines
    /// between adjacent items are preserved via [`Self::gap_between_spans`].
    /// Returns [`Doc::Empty`] when both lists are empty.
    fn stmts_to_doc(&self, stmts: &[Stmt], container: Span) -> Doc {
        let dangling_comments = self.all_dangling(container);
        if stmts.is_empty() && dangling_comments.is_empty() {
            return Doc::Empty;
        }

        let mut items: Vec<BodyItem> = stmts.iter().map(|s| BodyItem::Stmt(s, *s.span())).collect();
        for c in dangling_comments {
            items.push(BodyItem::Comment(c));
        }

        items.sort_by_key(|i| self.effective_start(i));

        let mut docs = Vec::new();
        let mut prev_end: Option<usize> = None;
        for item in &items {
            let start = self.effective_start(item);
            if let Some(end) = prev_end {
                let prev_span = Span { start: 0, end };
                let next_span = Span { start, end: start };
                docs.push(self.gap_between_spans(Some(&prev_span), Some(&next_span)));
            }

            match item {
                BodyItem::Stmt(stmt, _) => {
                    docs.push(self.stmt_to_doc(stmt));
                }
                BodyItem::Comment(c) => {
                    docs.push(self.comment_to_doc(c));
                }
                BodyItem::Decl(_, _) => unreachable!("stmts_to_doc only contains Stmt items"),
            }

            prev_end = Some(self.effective_end(item));
        }

        Doc::Concat(docs)
    }

    /// All standalone comments attached to `container` regardless of dangling
    /// placement. Used by `stmts_to_doc` / `decls_to_doc` to interleave
    /// orphan comments with their sibling nodes by source position.
    fn all_dangling(&self, container: Span) -> Vec<CommentStmt> {
        let mut out = Vec::new();
        out.extend(self.dangling(container, DanglingPlacement::BeforeFirstChild));
        out.extend(self.dangling(container, DanglingPlacement::Inside));
        out.extend(self.dangling(container, DanglingPlacement::AfterLastChild));

        out
    }

    /// Start offset of `item` including any leading comments attached to it
    /// — used for ordering and for blank-line gap calculation so a leading
    /// comment shifts the "next item" earlier in source.
    fn effective_start(&self, item: &BodyItem) -> usize {
        match item {
            BodyItem::Decl(_, s) | BodyItem::Stmt(_, s) => self
                .leading(*s)
                .first()
                .map(|c| c.span.start)
                .unwrap_or(s.start),
            BodyItem::Comment(c) => c.span.start,
        }
    }

    /// End offset of `item` including any trailing comments attached to it.
    fn effective_end(&self, item: &BodyItem) -> usize {
        match item {
            BodyItem::Decl(_, s) | BodyItem::Stmt(_, s) => self
                .trailing(*s)
                .last()
                .map(|c| c.span.end)
                .unwrap_or(s.end),
            BodyItem::Comment(c) => c.span.end,
        }
    }

    fn stmt_to_doc(&self, stmt: &Stmt) -> Doc {
        let span = *stmt.span();
        let leading = self.leading_doc(span);
        let inner = self.stmt_inner_to_doc(stmt);
        let trailing = self.trailing_doc(span);

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
                self.paren_condition_header("while", &s.condition.inner),
                self.before_bracket_doc(s.span),
                self.block_body_to_doc(&s.body),
            ]),
            Stmt::DoWhile(s) => Doc::concat(vec![
                Doc::text("do "),
                self.before_bracket_doc(s.span),
                self.block_body_to_doc(&s.body),
                Doc::text(" while ("),
                self.expr_to_doc(&s.condition),
                Doc::text(");"),
            ]),
            Stmt::For(s) => {
                let init_doc = match &s.header.inner.init {
                    None => Doc::Empty,
                    Some(ForInit::Var(v)) => self.var_decl_to_doc(v),
                    Some(ForInit::Expr(e)) => self.expr_to_doc(e),
                };
                let cond_doc = s
                    .header
                    .inner
                    .condition
                    .as_ref()
                    .map(|e| self.expr_to_doc(e))
                    .unwrap_or(Doc::Empty);
                let update_doc = s
                    .header
                    .inner
                    .update
                    .as_ref()
                    .map(|e| self.expr_to_doc(e))
                    .unwrap_or(Doc::Empty);

                Doc::concat(vec![
                    Doc::text("for ("),
                    init_doc,
                    Doc::text("; "),
                    cond_doc,
                    Doc::text("; "),
                    update_doc,
                    Doc::text(") "),
                    self.before_bracket_doc(s.span),
                    self.block_body_to_doc(&s.body),
                ])
            }
            Stmt::Return(s) => match &s.value {
                None => Doc::text("return;"),
                Some(v) => Doc::concat(vec![
                    Doc::text("return "),
                    self.expr_to_doc(v),
                    Doc::text(";"),
                ]),
            },
            Stmt::Break(_) => Doc::text("break;"),
            Stmt::Continue(_) => Doc::text("continue;"),
            Stmt::Throw(s) => Doc::concat(vec![
                Doc::text("throw "),
                self.expr_to_doc(&s.value),
                Doc::text(";"),
            ]),
            Stmt::Switch(s) => self.switch_stmt_to_doc(s),
            Stmt::Try(s) => self.try_stmt_to_doc(s),
            Stmt::Var(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Stmt::Expr(e) => Doc::concat(vec![self.expr_inner_to_doc(e), Doc::text(";")]),
        }
    }

    /// Render the interior of a standalone `{ … }` block statement.
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

    /// Render an expression and prepend/append any leading or trailing
    /// comments attached to it via [`CommentsMap`]. Empty when there are no
    /// attached comments, so the common case has no overhead in the output.
    fn expr_to_doc(&self, expr: &Expr) -> Doc {
        let span = *expr.span();
        let leading = self.leading_doc(span);
        let inner = self.expr_inner_to_doc(expr);
        let trailing = self.trailing_doc(span);

        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => inner,
            _ => Doc::concat(vec![leading, inner, trailing]),
        }
    }

    /// Render an expression with leading comments but *without* trailing
    /// comments. Used by container renderers (dicts, arrays, call args) that
    /// need to emit a comma between the value and its trailing comments —
    /// otherwise an own-line trailing comment leaves an orphan comma on the
    /// next line.
    fn expr_with_leading(&self, expr: &Expr) -> Doc {
        let span = *expr.span();
        let leading = self.leading_doc(span);
        let inner = self.expr_inner_to_doc(expr);

        if matches!(&leading, Doc::Empty) {
            inner
        } else {
            Doc::concat(vec![leading, inner])
        }
    }

    fn expr_inner_to_doc(&self, expr: &Expr) -> Doc {
        match expr {
            Expr::Binary(e) => Doc::concat(vec![
                self.expr_to_doc(&e.left),
                Doc::text(format!(" {} ", operators::binary_op(&e.operator))),
                self.expr_to_doc(&e.right),
            ]),
            Expr::Unary(e) => match e.operator {
                UnaryOperator::PostInc => {
                    Doc::concat(vec![self.expr_to_doc(&e.operand), Doc::text("++")])
                }
                UnaryOperator::PostDec => {
                    Doc::concat(vec![self.expr_to_doc(&e.operand), Doc::text("--")])
                }
                _ => Doc::concat(vec![
                    Doc::text(operators::unary_prefix_op(&e.operator)),
                    self.expr_to_doc(&e.operand),
                ]),
            },
            Expr::Ternary(e) => Doc::Group(vec![
                self.expr_to_doc(&e.cond),
                Doc::Indent(vec![
                    Doc::Line,
                    Doc::text("? "),
                    self.expr_to_doc(&e.then_expr),
                    Doc::Line,
                    Doc::text(": "),
                    self.expr_to_doc(&e.else_expr),
                ]),
            ]),
            Expr::Assign(e) => Doc::concat(vec![
                self.expr_to_doc(&e.target),
                Doc::text(format!(" {} ", operators::assign_op(&e.operator))),
                self.expr_to_doc(&e.value),
            ]),
            Expr::Call(e) => Doc::concat(vec![
                self.expr_to_doc(&e.callee),
                self.format_list(
                    "(",
                    ")",
                    self.call_args_to_items(&e.args),
                    &[],
                    e.args_trailing_comma,
                ),
            ]),
            Expr::Member(e) => Doc::concat(vec![
                self.expr_to_doc(&e.object),
                Doc::text(format!(".{}", e.property)),
            ]),
            Expr::Index(e) => Doc::concat(vec![
                self.expr_to_doc(&e.object),
                Doc::text("["),
                self.expr_to_doc(&e.index),
                Doc::text("]"),
            ]),
            Expr::New(e) => Doc::concat(vec![
                Doc::text(format!("new {}", e.class)),
                self.format_list(
                    "(",
                    ")",
                    self.call_args_to_items(&e.args),
                    &[],
                    e.args_trailing_comma,
                ),
            ]),
            Expr::NewArray(e) => {
                let mut parts = vec![Doc::text("new")];
                if let Some(ty) = &e.element_type {
                    parts.push(Doc::text(" "));
                    parts.push(self.type_to_doc(ty));
                } else {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::text("["));
                parts.push(self.expr_to_doc(&e.size));
                parts.push(Doc::text(if e.is_byte_array { "]b" } else { "]" }));

                Doc::Concat(parts)
            }
            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_to_doc(&e.expr),
                Doc::text(" as "),
                self.type_to_doc(&e.target_type),
            ]),
            Expr::Array(e) => self.format_array(e),
            Expr::Dict(e) => self.format_dict(e),
            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Number(v) => v.to_string(),
                LiteralValue::Long(v) => format!("{v}l"),
                LiteralValue::Hex(s) => format!("0x{s}"),
                LiteralValue::HexLong(s) => format!("0x{s}l"),
                LiteralValue::Float(lit) => format_float_lit(lit),
                LiteralValue::Double(lit) => format_double_lit(lit),
                LiteralValue::String(v) => format!("\"{}\"", escape_string(v)),
                LiteralValue::Char(v) => format!("'{}'", escape_char(v)),
                LiteralValue::Boolean(v) => v.to_string(),
                LiteralValue::Symbol(v) => format!(":{v}"),
                LiteralValue::Null => "null".to_string(),
                LiteralValue::NaN => "NaN".to_string(),
            }),
            Expr::Ident(e) => Doc::text(&e.name),
            Expr::Paren(e) => Doc::concat(vec![
                Doc::text("("),
                self.expr_to_doc(&e.inner),
                Doc::text(")"),
            ]),
            Expr::Me(_) => Doc::text("me"),
            Expr::Self_(_) => Doc::text("self"),
            Expr::Bling(_) => Doc::text("$"),
        }
    }

    fn call_args_to_items(&self, args: &[CallArg]) -> Vec<ListItem> {
        args.iter()
            .map(|a| ListItem {
                content: self.expr_with_leading(&a.value),
                // Trailing comments go in the slot rendered after the `,`,
                // so `f(x, // tail\n  y)` doesn't put `// tail` before the
                // comma (which would swallow it).
                trailing_comments: self
                    .trailing(*a.value.span())
                    .into_iter()
                    .map(|c| self.comment_to_doc(&c))
                    .collect(),
            })
            .collect()
    }

    /// Format an array literal.
    /// - source has trailing comma → multi-line, blank lines preserved
    /// - source spans multiple lines and contains any comment → multi-line
    ///   (because `//` can't be inlined into `[…]`, and standalone block
    ///   comments between entries have no meaningful single-line position)
    /// - otherwise → try one-line, fall back to break-at-width
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
            let dangling_comments = self.all_dangling(e.span);
            if dangling_comments.is_empty() {
                return Doc::text("[]");
            }

            let mut inner = Vec::new();
            for (i, c) in dangling_comments.iter().enumerate() {
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

        let must_break =
            e.trailing_comma || (self.spans_multiple_lines(e.span) && self.has_comments_in(e.span));

        if must_break {
            return self.format_array_multiline(e);
        }

        let items: Vec<ListItem> = e
            .entries
            .iter()
            .map(|entry| ListItem {
                content: self.expr_to_doc(&entry.value),
                trailing_comments: Vec::new(),
            })
            .collect();

        self.format_list("[", "]", items, &[], false)
    }

    /// Multi-line array body. See [`Self::format_collection_multiline`].
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

    /// Format a bracketed list of items.
    /// - trailing comma → always multi-line, comma preserved after last item
    /// - no trailing comma → try flat; break only if the line would exceed [`self.line_width`]
    ///
    /// Each entry may carry trailing comments (rendered after its comma on the
    /// same line). Tail comments are emitted inside the brackets when there
    /// are no entries to attach them to. Presence of any comments forces
    /// multi-line rendering.
    fn format_list(
        &self,
        open: &str,
        close: &str,
        items: Vec<ListItem>,
        tail_comments: &[Stmt],
        trailing_comma: bool,
    ) -> Doc {
        if items.is_empty() && tail_comments.is_empty() {
            return Doc::text(format!("{open}{close}"));
        }

        let has_any_comments =
            !tail_comments.is_empty() || items.iter().any(|i| !i.trailing_comments.is_empty());

        if items.is_empty() {
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

        if trailing_comma || has_any_comments {
            let mut inner = Vec::new();
            let last_idx = items.len() - 1;
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::HardLine);
                }

                inner.push(item.content);

                if i != last_idx || trailing_comma {
                    inner.push(Doc::text(","));
                }

                for c in &item.trailing_comments {
                    inner.push(Doc::text(" "));
                    inner.push(c.clone());
                }
            }

            for c in tail_comments {
                inner.push(Doc::HardLine);
                inner.push(self.stmt_to_doc(c));
            }

            return Doc::concat(vec![
                Doc::text(open),
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text(close),
            ]);
        }

        let mut inner = Vec::new();
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                inner.push(Doc::text(","));
                inner.push(Doc::Line);
            }

            inner.push(item.content);
        }

        Doc::Group(vec![
            Doc::text(open),
            Doc::Indent(vec![Doc::SoftLine, Doc::Concat(inner)]),
            Doc::SoftLine,
            Doc::text(close),
        ])
    }

    /// Format a dict literal. Walks `members` so free-floating comments and
    /// blank lines between entries are preserved in source order.
    ///
    /// When [`align_pairs`](Self::align_pairs) is on, key columns
    /// are padded so `=>` operators line up across all entries.
    fn format_dict(&self, e: &DictExpr) -> Doc {
        // Empty dict — single token unless there are dangling comments
        // inside the braces.
        if e.entries.is_empty() {
            let dangling_comments = self.all_dangling(e.span);
            if dangling_comments.is_empty() {
                return Doc::text("{}");
            }

            let mut inner = Vec::new();
            for (i, c) in dangling_comments.iter().enumerate() {
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

        let must_break =
            e.trailing_comma || (self.spans_multiple_lines(e.span) && self.has_comments_in(e.span));

        if must_break {
            return self.format_dict_multiline(e);
        }

        if self.align_pairs {
            return self.format_dict_aligned_or_inline(e);
        }

        self.format_dict_inline_or_break(e)
    }

    /// Aligned-mode dict with no comments / trailing comma. Renders inline
    /// if it fits, otherwise breaks multi-line with `=>` columns aligned.
    fn format_dict_aligned_or_inline(&self, e: &DictExpr) -> Doc {
        let mut flat_inner = Vec::new();
        for (i, entry) in e.entries.iter().enumerate() {
            if i > 0 {
                flat_inner.push(Doc::text(", "));
            }

            flat_inner.push(Doc::concat(vec![
                self.expr_to_doc(&entry.key),
                Doc::text(" => "),
                self.expr_to_doc(&entry.value),
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

    /// Multi-line dict body. See [`Self::format_collection_multiline`].
    fn format_dict_multiline(&self, e: &DictExpr) -> Doc {
        let aligned = self.align_pairs && !e.entries.is_empty();
        let max_key_width = if aligned {
            e.entries
                .iter()
                .filter_map(|entry| doc::flat_width(&self.expr_inner_to_doc(&entry.key)))
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
                let key_doc = self.expr_to_doc(&entry.key);
                let padding = if aligned {
                    doc::flat_width(&self.expr_inner_to_doc(&entry.key))
                        .map(|w| " ".repeat(max_key_width.saturating_sub(w)))
                        .unwrap_or_default()
                } else {
                    String::new()
                };

                Doc::concat(vec![key_doc, Doc::Text(padding), Doc::text(" => ")])
            },
        )
    }

    /// Render an array- or dict-style multi-line collection body. Merges
    /// `entries` with `self.all_dangling(span)` in source order, emits
    /// blank-line-preserving gaps when `trailing_comma` is `true`, and wraps
    /// the result in `open` … `close`.
    ///
    /// `start_of` returns the entry's source-start (used for ordering against
    /// standalone comments). `value_of` returns the expression that carries
    /// trailing comments and is wrapped with leading comments. `head_of`
    /// returns the prefix emitted before the value (empty for arrays, the
    /// `key padding => ` text for dicts).
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
        let mut inside_comments = self.all_dangling(span);
        inside_comments.sort_by_key(|c| c.span.start);
        let mut comments_iter = inside_comments.iter().peekable();

        let last_idx = entries.len().saturating_sub(1);
        let mut inner: Vec<Doc> = Vec::new();
        let mut prev_end: Option<usize> = None;

        for (i, entry) in entries.iter().enumerate() {
            let entry_start = start_of(entry);

            // Emit any standalone comments that come before this entry.
            while comments_iter
                .peek()
                .is_some_and(|c| c.span.start < entry_start)
            {
                let c = comments_iter.next().unwrap();
                self.push_gap(&mut inner, prev_end, c.span.start, trailing_comma);
                inner.push(self.comment_to_doc(c));
                prev_end = Some(c.span.end);
            }

            self.push_gap(&mut inner, prev_end, entry_start, trailing_comma);

            let value = value_of(entry);
            let value_span = *value.span();
            let mut parts = vec![head_of(entry), self.expr_with_leading(value)];
            if i != last_idx || trailing_comma {
                parts.push(Doc::text(","));
            }

            let value_trailing = self.trailing_doc(value_span);
            if !matches!(value_trailing, Doc::Empty) {
                parts.push(value_trailing);
            }

            // Effective end includes a same-line trailing comment so the next
            // gap is measured from past it.
            let eff_end = self
                .trailing(value_span)
                .last()
                .map(|c| c.span.end)
                .unwrap_or(value_span.end);

            inner.push(Doc::Concat(parts));
            prev_end = Some(eff_end);
        }

        // Drain any standalone comments after the last entry.
        for c in comments_iter {
            self.push_gap(&mut inner, prev_end, c.span.start, trailing_comma);
            inner.push(self.comment_to_doc(c));
            prev_end = Some(c.span.end);
        }

        Doc::concat(vec![
            Doc::text(open.to_string()),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text(close.to_string()),
        ])
    }

    /// Push the inter-item gap to `inner`. When `preserve_blanks` is `true`,
    /// the source's blank-line count between `prev_end` and `next_start`
    /// drives whether to emit [`Doc::BlankLine`] or [`Doc::HardLine`];
    /// otherwise always [`Doc::HardLine`].
    fn push_gap(
        &self,
        inner: &mut Vec<Doc>,
        prev_end: Option<usize>,
        next_start: usize,
        preserve_blanks: bool,
    ) {
        let Some(prev) = prev_end else { return };

        if preserve_blanks {
            let prev_span = Span {
                start: 0,
                end: prev,
            };
            let next_span = Span {
                start: next_start,
                end: next_start,
            };

            inner.push(self.gap_between_spans(Some(&prev_span), Some(&next_span)));
        } else {
            inner.push(Doc::HardLine);
        }
    }

    /// Render an all-entries, no-comment dict either inline or broken via
    /// Wadler-Lindig group decision.
    fn format_dict_inline_or_break(&self, e: &DictExpr) -> Doc {
        let items: Vec<ListItem> = e
            .entries
            .iter()
            .map(|entry| ListItem {
                content: Doc::concat(vec![
                    self.expr_to_doc(&entry.key),
                    Doc::text(" => "),
                    self.expr_to_doc(&entry.value),
                ]),
                trailing_comments: Vec::new(),
            })
            .collect();

        self.format_list("{", "}", items, &[], e.trailing_comma)
    }

    /// Return a [`Doc::BlankLine`] if the original source had at least one blank
    /// line between these two spans, otherwise [`Doc::HardLine`].
    fn gap_between_spans(&self, prev_span: Option<&Span>, next_span: Option<&Span>) -> Doc {
        let blanks = match (prev_span, next_span) {
            (Some(prev), Some(next)) if prev.end > 0 && next.start > prev.end => self
                .line_index
                .blank_lines_between(prev.end as u32, next.start as u32),
            _ => 0,
        };

        if blanks > 0 {
            Doc::BlankLine
        } else {
            Doc::HardLine
        }
    }
}

/// Whether `expr` is a binary expression — any top-level binary operator
/// gives the formatter a natural wrap-point.
/// Rust's `f32`/`f64` `to_string` strips trailing `.0`, but Monkey C source
/// distinguishes integer and float literals by the decimal point — re-emit it
/// when it's missing so `1.0` doesn't round-trip to `1`.
fn with_decimal_point(s: &str) -> String {
    if s.contains('.') {
        s.to_string()
    } else {
        format!("{s}.0")
    }
}

/// Turn the canonical `0.978` form into the leading-dot `.978` form. The
/// lexer only flags `leading_dot` on positive literals (a leading `-` is
/// parsed as a unary expression), so stripping `0.` is sufficient.
fn strip_leading_zero(s: &str) -> String {
    s.strip_prefix("0.")
        .map(|rest| format!(".{rest}"))
        .unwrap_or_else(|| s.to_string())
}

/// Re-emit a [`FloatLit`] in the exact source form recorded by the lexer.
/// Combines `has_dot`/`leading_dot`/`has_suffix` to reconstruct `0f`, `0.5`,
/// `0.5f`, `.978`, `.5f`, etc.
fn format_float_lit(lit: &FloatLit) -> String {
    let body = if lit.has_dot {
        let with_dot = with_decimal_point(&lit.value.to_string());
        if lit.leading_dot {
            strip_leading_zero(&with_dot)
        } else {
            with_dot
        }
    } else {
        // Source had no `.`, so the literal is integer-valued — drop the
        // trailing `.0` that `f32::to_string()` includes for whole numbers.
        (lit.value as i64).to_string()
    };

    if lit.has_suffix {
        format!("{body}f")
    } else {
        body
    }
}

/// Re-emit a [`DoubleLit`] preserving its source style. Mirrors
/// [`format_float_lit`] but the `d` suffix is always present.
fn format_double_lit(lit: &DoubleLit) -> String {
    let body = if lit.has_dot {
        let with_dot = with_decimal_point(&lit.value.to_string());
        if lit.leading_dot {
            strip_leading_zero(&with_dot)
        } else {
            with_dot
        }
    } else {
        (lit.value as i64).to_string()
    };

    format!("{body}d")
}

/// The lexer decodes escape sequences in string literals (`\"` → `"`, `\n` →
/// newline, etc.), so emitting the stored value verbatim would produce invalid
/// source. Re-encode the same set the lexer recognises.
fn escape_string(s: &str) -> String {
    escape_quoted(s, '"')
}

/// Same as [`escape_string`] but escapes single quotes — used for character
/// literals (`'a'`).
fn escape_char(s: &str) -> String {
    escape_quoted(s, '\'')
}

fn escape_quoted(s: &str, quote: char) -> String {
    let mut out = String::with_capacity(s.len() + 2);

    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            _ if c == quote => {
                out.push('\\');
                out.push(c);
            }
            _ => out.push(c),
        }
    }

    out
}

/// Walk a left-associative binary tree and collect all operands joined by `op`,
/// at the same precedence level.
///
/// `a || b || c` parses as `Or(Or(a, b), c)` — this flattens it to
/// `[a, b, c]` so the formatter can render the chain with breakable
/// separators between siblings.
fn collect_logical_chain<'a>(expr: &'a Expr, op: &BinaryOperator, out: &mut Vec<&'a Expr>) {
    if let Expr::Binary(be) = expr
        && be.operator == *op
    {
        collect_logical_chain(&be.left, op, out);
        collect_logical_chain(&be.right, op, out);

        return;
    }

    out.push(expr);
}

/// Per-variant padding target for column-aligning the `=` between names and
/// explicit values. Only contiguous runs of two or more variants that all
/// have a value get aligned; everything else gets `0` (no padding).
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

/// Column-align trailing `//` and `/*` comments across consecutive lines. A
/// "run" is a sequence of lines that share the same leading-whitespace indent
/// and each end with a trailing comment. Within a run, the code portion is
/// padded with spaces so every comment starts at the same column.
///
/// Strings (`"…"`) and char literals (`'…'`) are tracked so a `//` inside a
/// string doesn't get mistaken for a trailing comment.
fn align_trailing_comments(text: &str) -> String {
    let lines: Vec<&str> = text.split('\n').collect();

    // For each line: `Some((indent, code_end, comment_start))` when the line
    // has a trailing comment (code before, comment after), else `None`.
    let analyzed: Vec<Option<(usize, usize, usize)>> =
        lines.iter().map(|l| analyze_trailing(l)).collect();

    let mut out: Vec<String> = lines.iter().map(|l| (*l).to_string()).collect();

    let mut i = 0;
    while i < analyzed.len() {
        let Some((indent, _, _)) = analyzed[i] else {
            i += 1;
            continue;
        };

        let mut j = i;
        while j < analyzed.len() && matches!(analyzed[j], Some((ind, _, _)) if ind == indent) {
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

/// Return `(indent, code_end, comment_start)` when `line` contains a trailing
/// comment — non-whitespace code followed by `//` or `/*` outside any string
/// or char literal. `code_end` is the byte offset just past the last
/// non-whitespace character of the code portion; `comment_start` is the byte
/// offset of the opening `/`.
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

    // Require non-whitespace code before the comment — otherwise it's a
    // standalone comment, not a trailing one.
    let code_end = line[..comment_start].trim_end().len();
    if code_end <= indent {
        return None;
    }

    Some((indent, code_end, comment_start))
}
