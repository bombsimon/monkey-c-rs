use monkey_c_parser::ast::{
    Ast, AssignOperator, BinaryOperator, BlockStmt, ClassDecl, Expr, ForInit, FunctionDecl,
    ImportDecl, LiteralValue, Span, Stmt, UnaryOperator, Variable, VarStmt, Visibility,
};

pub struct Formatter {
    source: String,
    output: String,
    indent_level: usize,
    indent_size: usize,
}

impl Formatter {
    pub fn new(source: String) -> Self {
        Self {
            source,
            output: String::new(),
            indent_level: 0,
            indent_size: 4,
        }
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn format(&mut self, ast: &Ast) {
        self.format_ast(ast);
    }

    // -----------------------------------------------------------------------
    // Indent helpers
    // -----------------------------------------------------------------------

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level -= 1;
    }

    fn write_indent(&mut self) {
        self.output
            .push_str(&" ".repeat(self.indent_level * self.indent_size));
    }

    fn write_span(&mut self, span: &Span) {
        self.output.push_str(&self.source[span.start..span.end]);
    }

    // -----------------------------------------------------------------------
    // Top-level declarations
    // -----------------------------------------------------------------------

    fn format_ast(&mut self, ast: &Ast) {
        match ast {
            Ast::Document(nodes) => {
                for node in nodes {
                    self.format_ast(node);
                }
            }
            Ast::Import(decl) => self.format_import(decl),
            Ast::Class(decl) => self.format_class(decl),
            Ast::Function(decl) => self.format_function(decl),
            Ast::Variable(var_stmt) => self.format_var_stmt(var_stmt),
            Ast::Comment(comment, _) => {
                self.write_indent();
                self.output.push_str("// ");
                self.output.push_str(comment);
                self.output.push('\n');
            }
            Ast::Annotation(_, span) => {
                self.write_indent();
                self.write_span(span);
                self.output.push('\n');
            }
            Ast::Eof => {}
        }
    }

    fn format_import(&mut self, decl: &ImportDecl) {
        self.write_indent();
        self.output.push_str("import ");
        self.output.push_str(&decl.name);
        if let Some(alias) = &decl.alias {
            self.output.push_str(" as ");
            self.output.push_str(alias);
        }
        self.output.push_str(";\n");
    }

    fn format_class(&mut self, decl: &ClassDecl) {
        self.write_indent();
        self.output.push_str("class ");
        self.output.push_str(&decl.name);
        if let Some(extends) = &decl.extends {
            self.output.push_str(" extends ");
            self.output.push_str(extends);
        }
        self.output.push_str(" {\n");
        self.indent();
        for member in &decl.body {
            self.format_ast(member);
        }
        self.dedent();
        self.write_indent();
        self.output.push_str("}\n");
    }

    fn format_function(&mut self, decl: &FunctionDecl) {
        self.write_indent();

        if let Some(visibility) = &decl.visibility {
            self.format_visibility(visibility);
        }
        if decl.is_static {
            self.output.push_str("static ");
        }
        if decl.is_hidden {
            self.output.push_str("hidden ");
        }

        self.output.push_str("function ");
        self.output.push_str(&decl.name);
        self.output.push('(');
        for (i, arg) in decl.args.iter().enumerate() {
            if i > 0 {
                self.output.push_str(", ");
            }
            self.output.push_str(&arg.name);
            if let Some(type_) = &arg.type_ {
                self.output.push_str(" as ");
                self.output.push_str(&type_.ident);
            }
        }
        self.output.push(')');
        if let Some(returns) = &decl.returns {
            self.output.push_str(" as ");
            self.output.push_str(&returns.ident);
        }
        self.output.push_str(" {\n");
        self.indent();
        for stmt in &decl.body {
            self.format_stmt(stmt);
        }
        self.dedent();
        self.output.push('\n');
        self.write_indent();
        self.output.push_str("}\n");
    }

    fn format_visibility(&mut self, visibility: &Visibility) {
        match visibility {
            Visibility::Private => self.output.push_str("private "),
            Visibility::Protected => self.output.push_str("protected "),
            Visibility::Public => self.output.push_str("public "),
        }
    }

    // -----------------------------------------------------------------------
    // Statements
    // -----------------------------------------------------------------------

    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Block(block) => self.format_block_inline(block),
            Stmt::If(s) => {
                self.write_indent();
                self.output.push_str("if (");
                self.format_expr(&s.condition);
                self.output.push(')');
                self.format_block_inline(&s.then_branch);
                if let Some(else_branch) = &s.else_branch {
                    self.output.push_str(" else");
                    self.format_block_inline(else_branch);
                }
                self.output.push('\n');
            }
            Stmt::While(s) => {
                self.write_indent();
                self.output.push_str("while (");
                self.format_expr(&s.condition);
                self.output.push_str(") ");
                self.format_block_inline(&s.body);
                self.output.push('\n');
            }
            Stmt::For(s) => {
                self.write_indent();
                self.output.push_str("for (");
                if let Some(init) = &s.init {
                    match init {
                        ForInit::Var(var_stmt) => self.format_var_inline(&var_stmt.variable),
                        ForInit::Expr(expr) => self.format_expr(expr),
                    }
                }
                self.output.push_str("; ");
                if let Some(cond) = &s.condition {
                    self.format_expr(cond);
                }
                self.output.push_str("; ");
                if let Some(update) = &s.update {
                    self.format_expr(update);
                }
                self.output.push_str(") ");
                self.format_block_inline(&s.body);
                self.output.push('\n');
            }
            Stmt::Return(s) => {
                self.write_indent();
                self.output.push_str("return");
                if let Some(value) = &s.value {
                    self.output.push(' ');
                    self.format_expr(value);
                }
                self.output.push_str(";\n");
            }
            Stmt::Break(_) => {
                self.write_indent();
                self.output.push_str("break;\n");
            }
            Stmt::Continue(_) => {
                self.write_indent();
                self.output.push_str("continue;\n");
            }
            Stmt::Var(var_stmt) => self.format_var_stmt(var_stmt),
            Stmt::Comment(comment, _) => {
                self.write_indent();
                self.output.push_str("// ");
                self.output.push_str(comment);
                self.output.push('\n');
            }
            Stmt::Expr(expr) => {
                self.write_indent();
                self.format_expr(expr);
                self.output.push_str(";\n");
            }
        }
    }

    /// Format a block as `{\n ... \n}` with the opening brace on the same line as the caller.
    fn format_block_inline(&mut self, block: &BlockStmt) {
        self.output.push_str(" {\n");
        self.indent();
        for stmt in &block.stmts {
            self.format_stmt(stmt);
        }
        self.dedent();
        self.write_indent();
        self.output.push('}');
    }

    /// Format a `var` declaration as a full statement (with indent, semicolon, and newline).
    fn format_var_stmt(&mut self, var_stmt: &VarStmt) {
        self.write_indent();
        self.format_var_decl(&var_stmt.variable);
        self.output.push_str(";\n");
    }

    /// Format a variable declaration without leading indent or trailing semicolon/newline.
    /// Used both for `var` statements and for `for` loop init clauses.
    fn format_var_inline(&mut self, var: &Variable) {
        self.format_var_decl(var);
    }

    fn format_var_decl(&mut self, var: &Variable) {
        if let Some(visibility) = &var.visibility {
            self.format_visibility(visibility);
        }
        if var.is_static {
            self.output.push_str("static ");
        }
        if var.is_hidden {
            self.output.push_str("hidden ");
        }
        self.output.push_str("var ");
        self.output.push_str(&var.name);
        if let Some(initializer) = &var.initializer {
            self.output.push_str(" = ");
            self.format_expr(initializer);
        }
        if let Some(type_) = &var.type_ {
            self.output.push_str(" as ");
            self.output.push_str(&type_.ident);
        }
    }

    // -----------------------------------------------------------------------
    // Expressions
    // -----------------------------------------------------------------------

    fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary(e) => {
                self.format_expr(&e.left);
                self.output.push(' ');
                self.format_binary_op(&e.operator);
                self.output.push(' ');
                self.format_expr(&e.right);
            }
            Expr::Unary(e) => match e.operator {
                UnaryOperator::PostInc => {
                    self.format_expr(&e.operand);
                    self.output.push_str("++");
                }
                UnaryOperator::PostDec => {
                    self.format_expr(&e.operand);
                    self.output.push_str("--");
                }
                _ => {
                    self.format_unary_prefix_op(&e.operator);
                    self.format_expr(&e.operand);
                }
            },
            Expr::Assign(e) => {
                self.format_expr(&e.target);
                self.output.push(' ');
                self.format_assign_op(&e.operator);
                self.output.push(' ');
                self.format_expr(&e.value);
            }
            Expr::Call(e) => {
                self.format_expr(&e.callee);
                self.output.push('(');
                for (i, arg) in e.args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_expr(arg);
                }
                self.output.push(')');
            }
            Expr::Member(e) => {
                self.format_expr(&e.object);
                self.output.push('.');
                self.output.push_str(&e.property);
            }
            Expr::Index(e) => {
                self.format_expr(&e.object);
                self.output.push('[');
                self.format_expr(&e.index);
                self.output.push(']');
            }
            Expr::New(e) => {
                self.output.push_str("new ");
                self.output.push_str(&e.class);
                self.output.push('(');
                for (i, arg) in e.args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_expr(arg);
                }
                self.output.push(')');
            }
            Expr::TypeCast(e) => {
                self.format_expr(&e.expr);
                self.output.push_str(" as ");
                self.output.push_str(&e.target_type.ident);
            }
            Expr::Array(e) => {
                self.output.push('[');
                for (i, elem) in e.elements.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_expr(elem);
                }
                self.output.push(']');
            }
            Expr::Dict(e) => {
                self.output.push('{');
                for (i, (key, value)) in e.pairs.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format_expr(key);
                    self.output.push_str(": ");
                    self.format_expr(value);
                }
                self.output.push('}');
            }
            Expr::Lit(e) => match &e.value {
                LiteralValue::Long(v) => self.output.push_str(&v.to_string()),
                LiteralValue::Double(v) => self.output.push_str(&v.to_string()),
                LiteralValue::String(v) => {
                    self.output.push('"');
                    self.output.push_str(v);
                    self.output.push('"');
                }
                LiteralValue::Boolean(v) => self.output.push_str(&v.to_string()),
                LiteralValue::Null => self.output.push_str("null"),
                LiteralValue::NaN => self.output.push_str("NaN"),
            },
            Expr::Ident(e) => self.output.push_str(&e.name),
            Expr::Me(_) => self.output.push_str("me"),
            Expr::Self_(_) => self.output.push_str("self"),
        }
    }

    fn format_binary_op(&mut self, op: &BinaryOperator) {
        let s = match op {
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
        };
        self.output.push_str(s);
    }

    fn format_unary_prefix_op(&mut self, op: &UnaryOperator) {
        let s = match op {
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "!",
            UnaryOperator::BitNot => "~",
            UnaryOperator::PreInc => "++",
            UnaryOperator::PreDec => "--",
            UnaryOperator::PostInc | UnaryOperator::PostDec => unreachable!(),
        };
        self.output.push_str(s);
    }

    fn format_assign_op(&mut self, op: &AssignOperator) {
        let s = match op {
            AssignOperator::Assign => "=",
            AssignOperator::AddAssign => "+=",
            AssignOperator::SubAssign => "-=",
            AssignOperator::MulAssign => "*=",
            AssignOperator::DivAssign => "/=",
            AssignOperator::ModAssign => "%=",
            AssignOperator::BitAndAssign => "&=",
            AssignOperator::BitOrAssign => "|=",
            AssignOperator::BitXorAssign => "^=",
        };
        self.output.push_str(s);
    }
}
