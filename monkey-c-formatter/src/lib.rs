pub mod doc;
mod operators;

use doc::{render, Doc};
use monkey_c_parser::ast::{
    ArrayEntry, Ast, BinaryOperator, BlockStmt, CallArg, ConstDecl, DictEntry, ElseBranch, Expr,
    ForInit, FunctionDecl, IfStmt, LiteralValue, Stmt, TryStmt, Type, VarDecl, Visibility,
};
use monkey_c_parser::line_index::LineIndex;

/// An item in a delimited list (array entry, dict pair, call arg, etc.) along
/// with any comments that trail it inside the bracketed list. Used by
/// [`Formatter::format_list`].
struct ListItem {
    content: Doc,
    trailing_comments: Vec<Doc>,
}

/// Walk a left-associative binary tree and collect all operands joined by `op`,
/// at the same precedence level.
///
/// `a || b || c` parses as `Or(Or(a, b), c)` — this flattens it to
/// `[a, b, c]` so the formatter can render the chain with breakable
/// separators between siblings.
fn collect_logical_chain<'a>(expr: &'a Expr, op: &BinaryOperator, out: &mut Vec<&'a Expr>) {
    if let Expr::Binary(be) = expr {
        if be.operator == *op {
            collect_logical_chain(&be.left, op, out);
            collect_logical_chain(&be.right, op, out);

            return;
        }
    }

    out.push(expr);
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
    /// When `true`, dict key-value pairs are rendered multi-line with their
    /// `=>` operators column-aligned.
    align_dict_pairs: bool,
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
            align_dict_pairs: false,
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    /// Enable column-aligned `=>` in dict literals (opt-in).
    ///
    /// When set, all non-empty dicts are rendered multi-line with their keys
    /// padded so the `=>` operators form a vertical column.
    pub fn with_aligned_dict_pairs(mut self) -> Self {
        self.align_dict_pairs = true;
        self
    }

    /// Format `ast` and return the result as a `String`.
    pub fn format(&self, ast: &Ast) -> String {
        let doc = self.ast_to_doc(ast);

        render(&doc, self.line_width)
    }

    fn ast_to_doc(&self, ast: &Ast) -> Doc {
        match ast {
            Ast::Document(nodes) => self.decls_to_doc(nodes),
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
                Self::type_to_doc(&decl.type_),
                Doc::text(";"),
            ]),
            Ast::Module(decl) => {
                let header = Doc::text(format!("module {} {{", decl.name));
                if decl.body.is_empty() {
                    return Doc::concat(vec![header, Doc::text("}")]);
                }
                Doc::concat(vec![
                    header,
                    Doc::Indent(vec![Doc::HardLine, self.decls_to_doc(&decl.body)]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Class(decl) => {
                let header = Doc::concat(vec![
                    Doc::text(format!("class {}", decl.name)),
                    decl.extends
                        .as_ref()
                        .map(|e| Doc::text(format!(" extends {}", e)))
                        .unwrap_or(Doc::Empty),
                ]);
                if decl.body.is_empty() {
                    return Doc::concat(vec![header, Doc::text(" {}")]);
                }
                Doc::concat(vec![
                    header,
                    Doc::text(" {"),
                    Doc::Indent(vec![Doc::HardLine, self.decls_to_doc(&decl.body)]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Function(decl) => self.function_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Comment(text, _) => Doc::text(format!("//{}", text)),
            Ast::BlockComment(text, _) => self.block_comment_to_doc(text),
            Ast::Annotation(name, _) => Doc::text(format!("(:{name})")),
            Ast::Eof => Doc::Empty,
        }
    }

    /// Render a sequence of declarations, inserting a [`Doc::BlankLine`] wherever
    /// the original source had one.
    fn decls_to_doc(&self, decls: &[Ast]) -> Doc {
        let mut docs = Vec::new();

        for (i, decl) in decls.iter().enumerate() {
            if i > 0 {
                let prev = &decls[i - 1];
                docs.push(self.gap_between_spans(prev.span(), decl.span()));
            }
            docs.push(self.ast_to_doc(decl));
        }

        Doc::Concat(docs)
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
                let mut arg_parts = vec![Doc::text(&arg.name)];
                if let Some(ty) = &arg.type_ {
                    arg_parts.push(Doc::text(" as "));
                    arg_parts.push(Self::type_to_doc(ty));
                }
                ListItem {
                    content: Doc::Concat(arg_parts),
                    trailing_comments: arg
                        .trailing_comments
                        .iter()
                        .map(|c| self.stmt_to_doc(c))
                        .collect(),
                }
            })
            .collect();

        parts.push(self.format_list("(", ")", items, &decl.args_tail_comments, false));

        if let Some(ret) = &decl.returns {
            parts.push(Doc::text(" as "));
            parts.push(Self::type_to_doc(ret));
        }

        parts.push(self.block_to_doc(&decl.body));

        Doc::Concat(parts)
    }

    fn var_stmt_to_doc(&self, var_decl: &VarDecl) -> Doc {
        Doc::concat(vec![self.var_decl_to_doc(var_decl), Doc::text(";")])
    }

    fn const_decl_to_doc(&self, decl: &ConstDecl) -> Doc {
        let mut parts: Vec<Doc> = Vec::new();

        if let Some(vis) = &decl.visibility {
            parts.push(self.visibility_to_doc(vis));
        }
        if decl.is_static {
            parts.push(Doc::text("static "));
        }

        parts.push(Doc::text(format!("const {}", decl.name)));

        if let Some(ty) = &decl.type_ {
            parts.push(Doc::text(" as "));
            parts.push(Self::type_to_doc(ty));
        }

        parts.push(Doc::text(" = "));
        parts.push(self.expr_to_doc(&decl.initializer));
        parts.push(Doc::text(";"));

        Doc::Concat(parts)
    }

    /// Render a `var` declaration without a trailing semicolon.
    ///
    /// Used for both `var` statements and `for`-loop init clauses.
    fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts: Vec<Doc> = Vec::new();

        if let Some(vis) = &var.visibility {
            parts.push(self.visibility_to_doc(vis));
        }
        if var.is_static {
            parts.push(Doc::text("static "));
        }

        parts.push(Doc::text(format!("var {}", var.name)));

        if let Some(ty) = &var.type_ {
            parts.push(Doc::text(" as "));
            parts.push(Self::type_to_doc(ty));
        }

        if let Some(init) = &var.initializer {
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

    fn type_to_doc(ty: &Type) -> Doc {
        let base = if ty.generic_params.is_empty() {
            let suffix = if ty.optional { "?" } else { "" };
            Doc::text(format!("{}{}", ty.ident, suffix))
        } else {
            let params: Vec<Doc> = ty
                .generic_params
                .iter()
                .enumerate()
                .flat_map(|(i, p)| {
                    if i > 0 {
                        vec![Doc::text(" or "), Self::type_to_doc(p)]
                    } else {
                        vec![Self::type_to_doc(p)]
                    }
                })
                .collect();
            let suffix = if ty.optional { "?" } else { "" };
            Doc::concat(vec![
                Doc::text(format!("{}<", ty.ident)),
                Doc::Concat(params),
                Doc::text(format!(">{}", suffix)),
            ])
        };

        if ty.alternatives.is_empty() {
            return base;
        }

        let mut parts = vec![base];
        for alt in &ty.alternatives {
            parts.push(Doc::text(" or "));
            parts.push(Self::type_to_doc(alt));
        }

        Doc::Concat(parts)
    }

    fn if_stmt_to_doc(&self, s: &IfStmt) -> Doc {
        let cond_doc = self.condition_to_doc(&s.condition);
        let header = Doc::Group(vec![
            Doc::text("if ("),
            Doc::Indent(vec![cond_doc]),
            Doc::text(")"),
            Doc::flat_or_break(Doc::text(" "), Doc::HardLine),
        ]);

        let mut parts = vec![header, self.block_body_to_doc(&s.then_branch)];

        for (i, comment) in s.trailing_comments.iter().enumerate() {
            if i == 0 {
                parts.push(Doc::text(" "));
            } else {
                parts.push(Doc::HardLine);
            }
            parts.push(self.stmt_to_doc(comment));
        }

        match &s.else_branch {
            None => {}
            Some(ElseBranch::Block(b)) => {
                if s.trailing_comments.is_empty() {
                    parts.push(Doc::text(" else"));
                } else {
                    parts.push(Doc::HardLine);
                    parts.push(Doc::text("else"));
                }
                parts.push(self.block_to_doc(b));
            }
            Some(ElseBranch::If(inner)) => {
                if s.trailing_comments.is_empty() {
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
    /// For top-level `||` / `&&` chains, the operands are joined by a
    /// breakable [`Doc::Line`] so an enclosing [`Doc::Group`] can wrap the
    /// chain at line width. Other expressions delegate to `expr_to_doc`.
    fn condition_to_doc(&self, expr: &Expr) -> Doc {
        if let Expr::Binary(e) = expr {
            if matches!(e.operator, BinaryOperator::Or | BinaryOperator::And) {
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

                return Doc::Concat(parts);
            }
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
            return Doc::text(format!("/*{}*/", text));
        }

        let trimmed = text.trim_end();

        Doc::concat(vec![
            Doc::text(format!("/*{}", trimmed)),
            Doc::HardLine,
            Doc::text("*/"),
        ])
    }

    fn try_stmt_to_doc(&self, s: &TryStmt) -> Doc {
        let mut parts = vec![Doc::text("try"), self.block_to_doc(&s.body)];

        for catch in &s.catches {
            let mut header = vec![Doc::text(" catch ("), Doc::text(&catch.binding)];
            if let Some(ty) = &catch.type_filter {
                header.push(Doc::text(" instanceof "));
                header.push(Self::type_to_doc(ty));
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
        if block.stmts.is_empty() {
            return Doc::text("{}");
        }

        Doc::concat(vec![
            Doc::text("{"),
            Doc::Indent(vec![Doc::HardLine, self.stmts_to_doc(&block.stmts)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    /// Render a block as ` {\n    …\n}` with the opening brace on the same line.
    fn block_to_doc(&self, block: &BlockStmt) -> Doc {
        if block.stmts.is_empty() {
            return Doc::text(" {}");
        }

        Doc::concat(vec![
            Doc::text(" {"),
            Doc::Indent(vec![Doc::HardLine, self.stmts_to_doc(&block.stmts)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    /// Render a sequence of statements, inserting a [`Doc::BlankLine`] wherever
    /// the original source had one.
    fn stmts_to_doc(&self, stmts: &[Stmt]) -> Doc {
        let mut docs = Vec::new();

        for (i, stmt) in stmts.iter().enumerate() {
            if i > 0 {
                let prev = &stmts[i - 1];
                docs.push(self.gap_between_spans(Some(prev.span()), Some(stmt.span())));
            }
            docs.push(self.stmt_to_doc(stmt));
        }

        Doc::Concat(docs)
    }

    fn stmt_to_doc(&self, stmt: &Stmt) -> Doc {
        match stmt {
            Stmt::Block(block) => Doc::concat(vec![Doc::text("{"), self.block_inner_to_doc(block)]),

            Stmt::If(s) => self.if_stmt_to_doc(s),

            Stmt::While(s) => Doc::concat(vec![
                Doc::text("while ("),
                self.expr_to_doc(&s.condition),
                Doc::text(")"),
                self.block_to_doc(&s.body),
            ]),

            Stmt::For(s) => {
                let init_doc = match &s.init {
                    None => Doc::Empty,
                    Some(ForInit::Var(v)) => self.var_decl_to_doc(v),
                    Some(ForInit::Expr(e)) => self.expr_to_doc(e),
                };
                let cond_doc = s
                    .condition
                    .as_ref()
                    .map(|e| self.expr_to_doc(e))
                    .unwrap_or(Doc::Empty);
                let update_doc = s
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
                    Doc::text(")"),
                    self.block_to_doc(&s.body),
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
            Stmt::Try(s) => self.try_stmt_to_doc(s),
            Stmt::Var(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Stmt::Comment(text, _) => Doc::text(format!("//{}", text)),
            Stmt::BlockComment(text, _) => self.block_comment_to_doc(text),
            Stmt::Expr(e) => Doc::concat(vec![self.expr_to_doc(e), Doc::text(";")]),
        }
    }

    /// Render the interior of a standalone `{ … }` block statement.
    fn block_inner_to_doc(&self, block: &BlockStmt) -> Doc {
        if block.stmts.is_empty() {
            return Doc::text("}");
        }

        Doc::concat(vec![
            Doc::Indent(vec![Doc::HardLine, self.stmts_to_doc(&block.stmts)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn expr_to_doc(&self, expr: &Expr) -> Doc {
        match expr {
            Expr::Binary(e) => Doc::concat(vec![
                self.expr_to_doc(&e.left),
                Doc::text(format!(" {} ", operators::binary_op(&e.operator))),
                self.expr_to_doc(&e.right),
            ]),

            Expr::Unary(e) => match e.operator {
                monkey_c_parser::ast::UnaryOperator::PostInc => {
                    Doc::concat(vec![self.expr_to_doc(&e.operand), Doc::text("++")])
                }
                monkey_c_parser::ast::UnaryOperator::PostDec => {
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
                    &e.tail_comments,
                    false,
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
                    &e.tail_comments,
                    false,
                ),
            ]),

            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_to_doc(&e.expr),
                Doc::text(" as "),
                Self::type_to_doc(&e.target_type),
            ]),

            Expr::Array(e) => self.format_list(
                "[",
                "]",
                self.array_entries_to_items(&e.entries),
                &e.tail_comments,
                e.trailing_comma,
            ),

            Expr::Dict(e) => {
                if self.align_dict_pairs && !e.entries.is_empty() {
                    return self.format_dict_aligned(e);
                }

                self.format_list(
                    "{",
                    "}",
                    self.dict_entries_to_items(&e.entries),
                    &e.tail_comments,
                    e.trailing_comma,
                )
            }

            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Long(v) => v.to_string(),
                LiteralValue::Hex(s) => format!("0x{}", s),
                LiteralValue::Double(v) => v.to_string(),
                LiteralValue::String(v) => format!("\"{}\"", v),
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
                content: self.expr_to_doc(&a.value),
                trailing_comments: a
                    .trailing_comments
                    .iter()
                    .map(|c| self.stmt_to_doc(c))
                    .collect(),
            })
            .collect()
    }

    fn array_entries_to_items(&self, entries: &[ArrayEntry]) -> Vec<ListItem> {
        entries
            .iter()
            .map(|e| ListItem {
                content: self.expr_to_doc(&e.value),
                trailing_comments: e
                    .trailing_comments
                    .iter()
                    .map(|c| self.stmt_to_doc(c))
                    .collect(),
            })
            .collect()
    }

    fn dict_entries_to_items(&self, entries: &[DictEntry]) -> Vec<ListItem> {
        entries
            .iter()
            .map(|e| ListItem {
                content: Doc::concat(vec![
                    self.expr_to_doc(&e.key),
                    Doc::text(" => "),
                    self.expr_to_doc(&e.value),
                ]),
                trailing_comments: e
                    .trailing_comments
                    .iter()
                    .map(|c| self.stmt_to_doc(c))
                    .collect(),
            })
            .collect()
    }

    /// Format a bracketed list with the magic trailing comma rule:
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
            return Doc::text(format!("{}{}", open, close));
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

    /// Format a dict with column-aligned `=>` operators.
    ///
    /// If the dict fits on one line it is rendered inline without padding
    /// (alignment is only meaningful across multiple lines). If it breaks —
    /// either because it is too wide or has a trailing comma — keys are
    /// padded so all `=>` operators line up.
    fn format_dict_aligned(&self, e: &monkey_c_parser::ast::DictExpr) -> Doc {
        let has_any_comments = !e.tail_comments.is_empty()
            || e.entries.iter().any(|x| !x.trailing_comments.is_empty());

        let flat_items: Vec<Doc> = e
            .entries
            .iter()
            .map(|entry| {
                Doc::concat(vec![
                    self.expr_to_doc(&entry.key),
                    Doc::text(" => "),
                    self.expr_to_doc(&entry.value),
                ])
            })
            .collect();

        let aligned_items = doc::align_pairs(
            e.entries
                .iter()
                .map(|entry| (self.expr_to_doc(&entry.key), self.expr_to_doc(&entry.value)))
                .collect(),
            " => ",
        );

        // Flat content: comma-space separated, no padding needed
        let mut flat_inner = Vec::new();
        for (i, item) in flat_items.into_iter().enumerate() {
            if i > 0 {
                flat_inner.push(Doc::text(", "));
            }

            flat_inner.push(item);
        }

        // Break content: one aligned pair per line, with each entry's trailing
        // comments inline after its comma. A final comma is always emitted to
        // match the existing alignment style.
        let mut break_inner = Vec::new();
        let last_idx = aligned_items.len().saturating_sub(1);
        for (i, item) in aligned_items.into_iter().enumerate() {
            if i > 0 {
                break_inner.push(Doc::HardLine);
            }
            break_inner.push(item);
            break_inner.push(Doc::text(","));
            for c in &e.entries[i].trailing_comments {
                break_inner.push(Doc::text(" "));
                break_inner.push(self.stmt_to_doc(c));
            }
            let _ = last_idx;
        }

        for c in &e.tail_comments {
            break_inner.push(Doc::HardLine);
            break_inner.push(self.stmt_to_doc(c));
        }

        if e.trailing_comma || has_any_comments {
            return Doc::concat(vec![
                Doc::text("{"),
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(break_inner)]),
                Doc::HardLine,
                Doc::text("}"),
            ]);
        }

        Doc::Group(vec![
            Doc::text("{"),
            Doc::Indent(vec![Doc::flat_or_break(
                Doc::Concat(flat_inner),
                Doc::concat(vec![Doc::HardLine, Doc::Concat(break_inner)]),
            )]),
            Doc::flat_or_break(Doc::Empty, Doc::HardLine),
            Doc::text("}"),
        ])
    }

    /// Return a [`Doc::BlankLine`] if the original source had at least one blank
    /// line between these two spans, otherwise [`Doc::HardLine`].
    fn gap_between_spans(
        &self,
        prev_span: Option<&monkey_c_parser::ast::Span>,
        next_span: Option<&monkey_c_parser::ast::Span>,
    ) -> Doc {
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
