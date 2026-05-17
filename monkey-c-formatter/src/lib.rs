pub mod doc;
mod operators;

use doc::{render, Doc};
use monkey_c_parser::ast::{
    ArrayEntry, ArrayMember, Ast, BinaryOperator, BlockStmt, CallArg, CaseLabel, ConstDecl,
    DictEntry, DictMember, DictTypeEntry, DictTypeKey, ElseBranch, EnumDecl, Expr, ForInit,
    FunctionDecl, IfStmt, LiteralValue, Stmt, SwitchStmt, TryStmt, Type, TypeKind, VarDecl,
    Visibility,
};
use monkey_c_parser::line_index::LineIndex;

/// An item in a delimited list (array entry, dict pair, call arg, etc.) along
/// with any comments that trail it inside the bracketed list. Used by
/// [`Formatter::format_list`].
struct ListItem {
    content: Doc,
    trailing_comments: Vec<Doc>,
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
        format!("{}.0", s)
    }
}

fn top_level_binary(expr: &Expr) -> bool {
    matches!(expr, Expr::Binary(_))
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
            Ast::Enum(decl) => self.enum_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Comment(text, _) => Doc::text(format!("//{}", text)),
            Ast::BlockComment(text, _) => self.block_comment_to_doc(text),
            Ast::Annotation(names, _) => {
                let joined = names
                    .iter()
                    .map(|n| format!(":{n}"))
                    .collect::<Vec<_>>()
                    .join(", ");
                Doc::text(format!("({joined})"))
            }
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

    /// Render an `enum { … }`. Always multi-line — enums are declarations
    /// and never collapse to a single line. A trailing comma in source is
    /// preserved on the last variant.
    fn enum_to_doc(&self, decl: &EnumDecl) -> Doc {
        if decl.variants.is_empty() {
            return Doc::text("enum {}");
        }

        let last_idx = decl.variants.len() - 1;
        let mut inner = Vec::new();
        for (i, v) in decl.variants.iter().enumerate() {
            if i > 0 {
                inner.push(Doc::HardLine);
            }
            let mut parts = vec![Doc::text(&v.name)];
            if let Some(value) = &v.value {
                parts.push(Doc::text(" = "));
                parts.push(self.expr_to_doc(value));
            }
            if i != last_idx || decl.trailing_comma {
                parts.push(Doc::text(","));
            }
            for c in &v.trailing_comments {
                parts.push(Doc::text(" "));
                parts.push(self.stmt_to_doc(c));
            }
            inner.push(Doc::Concat(parts));
        }

        Doc::concat(vec![
            Doc::text("enum {"),
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
        let suffix = if ty.optional { "?" } else { "" };
        let base = match &ty.kind {
            TypeKind::Named {
                ident,
                generic_params,
            } => {
                if generic_params.is_empty() {
                    Doc::text(format!("{}{}", ident, suffix))
                } else {
                    let params: Vec<Doc> = generic_params
                        .iter()
                        .enumerate()
                        .flat_map(|(i, p)| {
                            if i > 0 {
                                vec![Doc::text(", "), Self::type_to_doc(p)]
                            } else {
                                vec![Self::type_to_doc(p)]
                            }
                        })
                        .collect();
                    Doc::concat(vec![
                        Doc::text(format!("{}<", ident)),
                        Doc::Concat(params),
                        Doc::text(format!(">{}", suffix)),
                    ])
                }
            }
            TypeKind::Dict {
                entries,
                trailing_comma,
            } => Self::inline_dict_type_to_doc(entries, *trailing_comma, suffix),
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

    /// Render an inline dict type `{ :k as T, "k" as U }`. A source-level
    /// trailing comma forces multi-line (magic-trailing-comma rule); otherwise
    /// fits on one line when it can.
    fn inline_dict_type_to_doc(
        entries: &[DictTypeEntry],
        trailing_comma: bool,
        suffix: &str,
    ) -> Doc {
        if entries.is_empty() {
            return Doc::text(format!("{{}}{}", suffix));
        }

        let entry_doc = |entry: &DictTypeEntry| {
            let key = match &entry.key {
                DictTypeKey::Symbol(s) => format!(":{s}"),
                DictTypeKey::String(s) => format!("\"{s}\""),
            };
            Doc::concat(vec![
                Doc::text(key),
                Doc::text(" as "),
                Self::type_to_doc(&entry.value_type),
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
                Doc::text(format!("}}{}", suffix)),
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
            Doc::text(format!("}}{}", suffix)),
        ])
    }

    /// Render `<keyword> (cond)` followed by either a space (when `cond` fits
    /// flat) or a hard line break (when it wraps), so the trailing `{` lands
    /// on its own line in the wrapped case. Used by `if`, `while`, `switch`.
    ///
    /// For non-binary conditions there's nothing to wrap at, so a plain
    /// `<keyword> (cond) ` is emitted regardless of width.
    fn paren_condition_header(&self, keyword: &str, cond: &Expr) -> Doc {
        let wrappable = top_level_binary(cond);
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
        let header = self.paren_condition_header("if", &s.condition);
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
            return Doc::text(format!("/*{}*/", text));
        }

        let trimmed = text.trim_end();

        Doc::concat(vec![
            Doc::text(format!("/*{}", trimmed)),
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

            for c in &case.leading_comments {
                body.push(self.stmt_to_doc(c));
                body.push(Doc::HardLine);
            }

            let mut header = vec![Doc::text("case ")];
            match &case.label {
                CaseLabel::Value(e) => {
                    header.push(self.expr_to_doc(e));
                }
                CaseLabel::InstanceOf(ty) => {
                    header.push(Doc::text("instanceof "));
                    header.push(Self::type_to_doc(ty));
                }
                CaseLabel::Default => {
                    header = vec![Doc::text("default")];
                }
            }
            header.push(Doc::text(":"));
            body.push(Doc::Concat(header));

            if !case.stmts.is_empty() {
                body.push(Doc::Indent(vec![
                    Doc::HardLine,
                    self.stmts_to_doc(&case.stmts),
                ]));
            }
        }

        for c in &s.tail_comments {
            body.push(Doc::HardLine);
            body.push(self.stmt_to_doc(c));
        }

        Doc::concat(vec![
            self.paren_condition_header("switch", &s.discriminant),
            Doc::text("{"),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(body)]),
            Doc::HardLine,
            Doc::text("}"),
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
                self.paren_condition_header("while", &s.condition),
                self.block_body_to_doc(&s.body),
            ]),

            Stmt::DoWhile(s) => Doc::concat(vec![
                Doc::text("do "),
                self.block_body_to_doc(&s.body),
                Doc::text(" while ("),
                self.expr_to_doc(&s.condition),
                Doc::text(");"),
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
            Stmt::Switch(s) => self.switch_stmt_to_doc(s),
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

            Expr::NewArray(e) => {
                let mut parts = vec![Doc::text("new")];
                if let Some(ty) = &e.element_type {
                    parts.push(Doc::text(" "));
                    parts.push(Self::type_to_doc(ty));
                } else {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::text("["));
                parts.push(self.expr_to_doc(&e.size));
                parts.push(Doc::text("]"));

                Doc::Concat(parts)
            }

            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_to_doc(&e.expr),
                Doc::text(" as "),
                Self::type_to_doc(&e.target_type),
            ]),

            Expr::Array(e) => self.format_array(e),

            Expr::Dict(e) => self.format_dict(e),

            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Number(v) => v.to_string(),
                LiteralValue::Long(v) => format!("{}l", v),
                LiteralValue::Hex(s) => format!("0x{}", s),
                LiteralValue::Float(v) => with_decimal_point(&v.to_string()),
                LiteralValue::Double(v) => format!("{}d", with_decimal_point(&v.to_string())),
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

    /// Format an array literal. Mirrors [`format_dict`](Self::format_dict):
    /// walks `members` so free-floating comments and blank lines between
    /// entries are preserved.
    fn format_array(&self, e: &monkey_c_parser::ast::ArrayExpr) -> Doc {
        if e.members.is_empty() {
            return Doc::text("[]");
        }

        let only_entries = e.members.iter().all(|m| matches!(m, ArrayMember::Entry(_)));
        let any_inline_comments = e.members.iter().any(|m| {
            if let ArrayMember::Entry(entry) = m {
                !entry.trailing_comments.is_empty()
            } else {
                false
            }
        });

        if only_entries && !e.trailing_comma && !any_inline_comments {
            let items: Vec<ListItem> = e
                .entries()
                .map(|entry| ListItem {
                    content: self.expr_to_doc(&entry.value),
                    trailing_comments: Vec::new(),
                })
                .collect();
            return self.format_list("[", "]", items, &[], false);
        }

        self.format_array_multiline(e)
    }

    /// Multi-line array body. Walks `members` interleaving entries, comments,
    /// and blank lines, the same shape as [`format_dict_multiline`].
    fn format_array_multiline(&self, e: &monkey_c_parser::ast::ArrayExpr) -> Doc {
        let entries: Vec<&ArrayEntry> = e.entries().collect();
        let last_entry_idx = entries.len().saturating_sub(1);
        let mut entry_seen = 0usize;

        let mut inner: Vec<Doc> = Vec::new();
        let mut pending_blank = false;
        let mut first = true;

        for member in &e.members {
            match member {
                ArrayMember::BlankLine => {
                    pending_blank = true;
                }
                ArrayMember::Comment(c) => {
                    if !first {
                        inner.push(if pending_blank {
                            Doc::BlankLine
                        } else {
                            Doc::HardLine
                        });
                    }
                    pending_blank = false;
                    inner.push(self.stmt_to_doc(c));
                    first = false;
                }
                ArrayMember::Entry(entry) => {
                    if !first {
                        inner.push(if pending_blank {
                            Doc::BlankLine
                        } else {
                            Doc::HardLine
                        });
                    }
                    pending_blank = false;

                    let mut parts = vec![self.expr_to_doc(&entry.value)];
                    let is_last = entry_seen == last_entry_idx;
                    if !is_last || e.trailing_comma {
                        parts.push(Doc::text(","));
                    }
                    for c in &entry.trailing_comments {
                        parts.push(Doc::text(" "));
                        parts.push(self.stmt_to_doc(c));
                    }
                    inner.push(Doc::Concat(parts));
                    entry_seen += 1;
                    first = false;
                }
            }
        }

        Doc::concat(vec![
            Doc::text("["),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text("]"),
        ])
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

    /// Format a dict literal. Walks `members` so free-floating comments and
    /// blank lines between entries are preserved in source order.
    ///
    /// When [`align_dict_pairs`](Self::align_dict_pairs) is on, key columns
    /// are padded so `=>` operators line up across all entries.
    fn format_dict(&self, e: &monkey_c_parser::ast::DictExpr) -> Doc {
        // Empty dict — single token.
        if e.members.is_empty() {
            return Doc::text("{}");
        }

        // Inline shortcut: only key-value entries, no trailing comma, no
        // comments. Wadler-Lindig group decides flat vs break.
        let only_entries = e.members.iter().all(|m| matches!(m, DictMember::Entry(_)));
        let any_inline_comments = e.members.iter().any(|m| {
            if let DictMember::Entry(entry) = m {
                !entry.trailing_comments.is_empty()
            } else {
                false
            }
        });
        if only_entries && !e.trailing_comma && !any_inline_comments {
            if self.align_dict_pairs {
                // Aligned mode: fit on one line if possible, otherwise break
                // multi-line with column-padded keys.
                return self.format_dict_aligned_or_inline(e);
            }
            return self.format_dict_inline_or_break(e);
        }

        self.format_dict_multiline(e)
    }

    /// Aligned-mode dict with no comments / trailing comma. Renders inline
    /// if it fits, otherwise breaks multi-line with `=>` columns aligned.
    fn format_dict_aligned_or_inline(&self, e: &monkey_c_parser::ast::DictExpr) -> Doc {
        let entries: Vec<&DictEntry> = e.entries().collect();
        let mut flat_inner = Vec::new();
        for (i, entry) in entries.iter().enumerate() {
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

    /// Multi-line dict body. Walks `members` interleaving entries, comments,
    /// and blank lines.
    fn format_dict_multiline(&self, e: &monkey_c_parser::ast::DictExpr) -> Doc {
        let entries: Vec<&DictEntry> = e
            .members
            .iter()
            .filter_map(|m| match m {
                DictMember::Entry(entry) => Some(entry),
                _ => None,
            })
            .collect();
        let last_entry_idx = entries.len().saturating_sub(1);
        let mut entry_seen = 0usize;

        // Pre-compute key docs and (for alignment) the max flat key width.
        let aligned = self.align_dict_pairs && !entries.is_empty();
        let max_key_width = if aligned {
            entries
                .iter()
                .filter_map(|entry| doc::flat_width(&self.expr_to_doc(&entry.key)))
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        let mut inner: Vec<Doc> = Vec::new();
        let mut pending_blank = false;
        let mut first = true;

        for member in &e.members {
            match member {
                DictMember::BlankLine => {
                    pending_blank = true;
                }
                DictMember::Comment(c) => {
                    if !first {
                        inner.push(if pending_blank {
                            Doc::BlankLine
                        } else {
                            Doc::HardLine
                        });
                    }
                    pending_blank = false;
                    inner.push(self.stmt_to_doc(c));
                    first = false;
                }
                DictMember::Entry(entry) => {
                    if !first {
                        inner.push(if pending_blank {
                            Doc::BlankLine
                        } else {
                            Doc::HardLine
                        });
                    }
                    pending_blank = false;

                    let key_doc = self.expr_to_doc(&entry.key);
                    let padding = if aligned {
                        doc::flat_width(&key_doc)
                            .map(|w| " ".repeat(max_key_width - w))
                            .unwrap_or_default()
                    } else {
                        String::new()
                    };

                    let mut parts = vec![
                        key_doc,
                        Doc::Text(padding),
                        Doc::text(" => "),
                        self.expr_to_doc(&entry.value),
                    ];

                    let is_last = entry_seen == last_entry_idx;
                    if !is_last || e.trailing_comma {
                        parts.push(Doc::text(","));
                    }
                    for c in &entry.trailing_comments {
                        parts.push(Doc::text(" "));
                        parts.push(self.stmt_to_doc(c));
                    }
                    inner.push(Doc::Concat(parts));
                    entry_seen += 1;
                    first = false;
                }
            }
        }

        Doc::concat(vec![
            Doc::text("{"),
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    /// Render an all-entries, no-comment dict either inline or broken via
    /// Wadler-Lindig group decision.
    fn format_dict_inline_or_break(&self, e: &monkey_c_parser::ast::DictExpr) -> Doc {
        let entries: Vec<&DictEntry> = e
            .members
            .iter()
            .filter_map(|m| match m {
                DictMember::Entry(entry) => Some(entry),
                _ => None,
            })
            .collect();

        let items: Vec<ListItem> = entries
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
