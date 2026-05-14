pub mod doc;
mod operators;

use doc::{render, Doc};
use monkey_c_parser::ast::{
    Ast, BlockStmt, ConstDecl, Expr, ForInit, FunctionDecl, LiteralValue, Stmt, Type, VarDecl,
    Visibility,
};
use monkey_c_parser::line_index::LineIndex;

/// Formats a Monkey C AST back into source text.
///
/// Construct with [`Formatter::new`], optionally configure with
/// [`Formatter::with_line_width`], then call [`Formatter::format`].
///
/// The formatter uses the Wadler-Lindig algorithm (see [`doc`]) to make
/// line-breaking decisions globally rather than per-node, so a dict or array
/// that fits on one line is kept there automatically.
pub struct Formatter {
    line_index: LineIndex,
    /// Maximum line width before a [`doc::Doc::Group`] is broken.
    line_width: usize,
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
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;

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
                decl.alias
                    .as_ref()
                    .map(|a| Doc::concat(vec![Doc::text(" as "), Doc::text(a)]))
                    .unwrap_or(Doc::Empty),
                Doc::text(";"),
            ]),
            Ast::Module(decl) => Doc::concat(vec![
                Doc::text(format!("module {} {{", decl.name)),
                Doc::Indent(vec![Doc::HardLine, self.decls_to_doc(&decl.body)]),
                Doc::HardLine,
                Doc::text("}"),
            ]),
            Ast::Class(decl) => Doc::concat(vec![
                Doc::text(format!("class {}", decl.name)),
                decl.extends
                    .as_ref()
                    .map(|e| Doc::text(format!(" extends {}", e)))
                    .unwrap_or(Doc::Empty),
                Doc::text(" {"),
                Doc::Indent(vec![Doc::HardLine, self.decls_to_doc(&decl.body)]),
                Doc::HardLine,
                Doc::text("}"),
            ]),
            Ast::Function(decl) => self.function_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Comment(text, _) => Doc::text(format!("//{}", text)),
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

        parts.push(Doc::text(format!("function {}(", decl.name)));

        for (i, arg) in decl.args.iter().enumerate() {
            if i > 0 {
                parts.push(Doc::text(", "));
            }
            parts.push(Doc::text(&arg.name));
            if let Some(ty) = &arg.type_ {
                parts.push(Doc::text(" as "));
                parts.push(self.type_to_doc(ty));
            }
        }

        parts.push(Doc::text(")"));

        if let Some(ret) = &decl.returns {
            parts.push(Doc::text(" as "));
            parts.push(self.type_to_doc(ret));
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
            parts.push(self.type_to_doc(ty));
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
            parts.push(self.type_to_doc(ty));
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

    fn type_to_doc(&self, ty: &Type) -> Doc {
        if ty.generic_params.is_empty() {
            let suffix = if ty.optional { "?" } else { "" };

            return Doc::text(format!("{}{}", ty.ident, suffix));
        }

        let params: Vec<Doc> = ty
            .generic_params
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
        let suffix = if ty.optional { "?" } else { "" };

        Doc::concat(vec![
            Doc::text(format!("{}<", ty.ident)),
            Doc::Concat(params),
            Doc::text(format!(">{}", suffix)),
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

            Stmt::If(s) => {
                let mut parts = vec![
                    Doc::text("if ("),
                    self.expr_to_doc(&s.condition),
                    Doc::text(")"),
                    self.block_to_doc(&s.then_branch),
                ];
                if let Some(else_branch) = &s.else_branch {
                    parts.push(Doc::text(" else"));
                    parts.push(self.block_to_doc(else_branch));
                }

                Doc::Concat(parts)
            }

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
            Stmt::Var(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Stmt::Comment(text, _) => Doc::text(format!("//{}", text)),
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

            Expr::Assign(e) => Doc::concat(vec![
                self.expr_to_doc(&e.target),
                Doc::text(format!(" {} ", operators::assign_op(&e.operator))),
                self.expr_to_doc(&e.value),
            ]),

            Expr::Call(e) => {
                let args: Vec<Doc> = e
                    .args
                    .iter()
                    .enumerate()
                    .flat_map(|(i, a)| {
                        if i > 0 {
                            vec![Doc::text(", "), self.expr_to_doc(a)]
                        } else {
                            vec![self.expr_to_doc(a)]
                        }
                    })
                    .collect();

                Doc::concat(vec![
                    self.expr_to_doc(&e.callee),
                    Doc::text("("),
                    Doc::Concat(args),
                    Doc::text(")"),
                ])
            }

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

            Expr::New(e) => {
                let args: Vec<Doc> = e
                    .args
                    .iter()
                    .enumerate()
                    .flat_map(|(i, a)| {
                        if i > 0 {
                            vec![Doc::text(", "), self.expr_to_doc(a)]
                        } else {
                            vec![self.expr_to_doc(a)]
                        }
                    })
                    .collect();

                Doc::concat(vec![
                    Doc::text(format!("new {}(", e.class)),
                    Doc::Concat(args),
                    Doc::text(")"),
                ])
            }

            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_to_doc(&e.expr),
                Doc::text(" as "),
                self.type_to_doc(&e.target_type),
            ]),

            Expr::Array(e) => {
                let items = e.elements.iter().map(|el| self.expr_to_doc(el)).collect();

                self.format_list("[", "]", items, e.trailing_comma)
            }

            Expr::Dict(e) => {
                let items = e
                    .pairs
                    .iter()
                    .map(|(k, v)| {
                        Doc::concat(vec![
                            self.expr_to_doc(k),
                            Doc::text(": "),
                            self.expr_to_doc(v),
                        ])
                    })
                    .collect();

                self.format_list("{", "}", items, e.trailing_comma)
            }

            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Long(v) => v.to_string(),
                LiteralValue::Double(v) => v.to_string(),
                LiteralValue::String(v) => format!("\"{}\"", v),
                LiteralValue::Boolean(v) => v.to_string(),
                LiteralValue::Null => "null".to_string(),
                LiteralValue::NaN => "NaN".to_string(),
            }),

            Expr::Ident(e) => Doc::text(&e.name),
            Expr::Me(_) => Doc::text("me"),
            Expr::Self_(_) => Doc::text("self"),
            Expr::Bling(_) => Doc::text("$"),
        }
    }

    /// Format a bracketed list with the magic trailing comma rule:
    /// - trailing comma → always multi-line, comma preserved after last item
    /// - no trailing comma → try flat; break only if the line would exceed [`self.line_width`]
    fn format_list(&self, open: &str, close: &str, items: Vec<Doc>, trailing_comma: bool) -> Doc {
        if items.is_empty() {
            return Doc::text(format!("{}{}", open, close));
        }

        if trailing_comma {
            let mut inner = Vec::new();
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::text(","));
                    inner.push(Doc::HardLine);
                }
                inner.push(item);
            }
            inner.push(Doc::text(","));

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
            inner.push(item);
        }

        Doc::Group(vec![
            Doc::text(open),
            Doc::Indent(vec![Doc::SoftLine, Doc::Concat(inner)]),
            Doc::SoftLine,
            Doc::text(close),
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
