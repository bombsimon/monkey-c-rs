pub mod doc;

use doc::{render, Doc};
use monkey_c_parser::ast::{
    AssignOperator, Ast, BinaryOperator, BlockStmt, Expr, ForInit, FunctionDecl, LiteralValue,
    Stmt, Type, UnaryOperator, VarStmt, Variable, Visibility,
};
use monkey_c_parser::line_index::LineIndex;

pub struct Formatter {
    line_index: LineIndex,
    line_width: usize,
}

impl Formatter {
    pub fn new(source: impl AsRef<str>) -> Self {
        let source = source.as_ref();
        Self {
            line_index: LineIndex::new(source),
            line_width: 100,
        }
    }

    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    pub fn format(&self, ast: &Ast) -> String {
        let doc = self.ast_to_doc(ast);
        render(&doc, self.line_width)
    }

    // -----------------------------------------------------------------------
    // Top-level declarations
    // -----------------------------------------------------------------------

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
            Ast::Comment(text, _) => Doc::text(format!("//{}", text)),
            Ast::Annotation(name, _) => Doc::text(format!("(:{name})")),
            Ast::Eof => Doc::Empty,
        }
    }

    /// Render a sequence of declarations with blank-line preservation.
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
        if decl.is_hidden {
            parts.push(Doc::text("hidden "));
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

    fn var_stmt_to_doc(&self, var_stmt: &VarStmt) -> Doc {
        Doc::concat(vec![
            self.var_decl_to_doc(&var_stmt.variable),
            Doc::text(";"),
        ])
    }

    fn var_decl_to_doc(&self, var: &Variable) -> Doc {
        let mut parts: Vec<Doc> = Vec::new();

        if let Some(vis) = &var.visibility {
            parts.push(self.visibility_to_doc(vis));
        }
        if var.is_static {
            parts.push(Doc::text("static "));
        }
        if var.is_hidden {
            parts.push(Doc::text("hidden "));
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
            Visibility::Public => "public ",
        })
    }

    fn type_to_doc(&self, ty: &Type) -> Doc {
        if ty.generic_params.is_empty() {
            let suffix = if ty.optional { "?" } else { "" };
            Doc::text(format!("{}{}", ty.ident, suffix))
        } else {
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
    }

    // -----------------------------------------------------------------------
    // Statements
    // -----------------------------------------------------------------------

    /// Render a block body inline: ` {\n    ...\n}`.
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

    /// Render a sequence of statements with blank-line preservation.
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
                    Some(ForInit::Var(v)) => self.var_decl_to_doc(&v.variable),
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

    // -----------------------------------------------------------------------
    // Expressions
    // -----------------------------------------------------------------------

    fn expr_to_doc(&self, expr: &Expr) -> Doc {
        match expr {
            Expr::Binary(e) => Doc::concat(vec![
                self.expr_to_doc(&e.left),
                Doc::text(format!(" {} ", self.binary_op_str(&e.operator))),
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
                    Doc::text(self.unary_prefix_op_str(&e.operator)),
                    self.expr_to_doc(&e.operand),
                ]),
            },

            Expr::Assign(e) => Doc::concat(vec![
                self.expr_to_doc(&e.target),
                Doc::text(format!(" {} ", self.assign_op_str(&e.operator))),
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
        }
    }

    // -----------------------------------------------------------------------
    // Collection formatting (magic trailing comma)
    // -----------------------------------------------------------------------

    /// Format `[items]` or `{items}` with the magic trailing comma rule:
    /// - trailing comma → always multi-line, trailing comma preserved
    /// - no trailing comma → try flat; break only if too wide
    fn format_list(&self, open: &str, close: &str, items: Vec<Doc>, trailing_comma: bool) -> Doc {
        if items.is_empty() {
            return Doc::text(format!("{}{}", open, close));
        }

        if trailing_comma {
            // Always multi-line, trailing comma after last item.
            let mut inner = Vec::new();
            for (i, item) in items.into_iter().enumerate() {
                if i > 0 {
                    inner.push(Doc::text(","));
                    inner.push(Doc::HardLine);
                }
                inner.push(item);
            }
            inner.push(Doc::text(","));

            Doc::concat(vec![
                Doc::text(open),
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text(close),
            ])
        } else {
            // Try to fit on one line; break if it doesn't fit.
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
    }

    // -----------------------------------------------------------------------
    // Blank-line helpers
    // -----------------------------------------------------------------------

    /// Return a separator Doc between two adjacent items based on how many
    /// blank lines separated them in the original source.
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

    // -----------------------------------------------------------------------
    // Operator helpers
    // -----------------------------------------------------------------------

    fn binary_op_str(&self, op: &BinaryOperator) -> &'static str {
        match op {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::Eq => "==",
            BinaryOperator::NotEq => "!=",
            BinaryOperator::Lt => "<",
            BinaryOperator::LtEq => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::GtEq => ">=",
            BinaryOperator::InstanceOf => "instanceof",
            BinaryOperator::And => "&&",
            BinaryOperator::Or => "||",
            BinaryOperator::BitAnd => "&",
            BinaryOperator::BitOr => "|",
            BinaryOperator::BitXor => "^",
        }
    }

    fn unary_prefix_op_str(&self, op: &UnaryOperator) -> &'static str {
        match op {
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "!",
            UnaryOperator::BitNot => "~",
            UnaryOperator::PreInc => "++",
            UnaryOperator::PreDec => "--",
            UnaryOperator::PostInc | UnaryOperator::PostDec => unreachable!(),
        }
    }

    fn assign_op_str(&self, op: &AssignOperator) -> &'static str {
        match op {
            AssignOperator::Assign => "=",
            AssignOperator::AddAssign => "+=",
            AssignOperator::SubAssign => "-=",
            AssignOperator::MulAssign => "*=",
            AssignOperator::DivAssign => "/=",
            AssignOperator::ModAssign => "%=",
            AssignOperator::BitAndAssign => "&=",
            AssignOperator::BitOrAssign => "|=",
            AssignOperator::BitXorAssign => "^=",
        }
    }
}
