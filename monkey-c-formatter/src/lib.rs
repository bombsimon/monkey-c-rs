use monkey_c_parser::ast::{Ast, Span};

pub struct Formatter {
    source: String,
    output: String,
    indent_level: usize,
    indent_size: usize,
    // Raw mode will never write newlines or ending semicolons with newlines. This is to be able to
    // format statements in specific contexts, e.g. variable assignment in `for()` loops.
    raw_mode: bool,
}

impl Formatter {
    pub fn new(source: String) -> Self {
        Self {
            source,
            output: String::new(),
            indent_level: 0,
            indent_size: 4,
            raw_mode: false,
        }
    }

    fn indent(&mut self) {
        self.indent_level += 1;
    }

    fn dedent(&mut self) {
        self.indent_level -= 1;
    }

    fn write_indent(&mut self) {
        if !self.raw_mode {
            self.output
                .push_str(&" ".repeat(self.indent_level * self.indent_size));
        }
    }

    fn write_newline(&mut self) {
        if !self.raw_mode {
            self.output.push('\n');
        }
    }

    fn write_semicolon(&mut self) {
        if !self.raw_mode {
            self.output.push_str(";\n");
        }
    }

    fn write_span(&mut self, span: &Span) {
        self.output.push_str(&self.source[span.start..span.end]);
    }

    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn format(&mut self, ast: &Ast) {
        match ast {
            Ast::Document(stmts) => {
                for stmt in stmts {
                    self.format(stmt);
                }
            }
            Ast::Comment(comment, _span) => {
                self.write_indent();
                // self.write_span(span);
                self.output.push_str("// ");
                self.output.push_str(comment);
                self.output.push('\n');
            }
            Ast::Annotation(_, span) => {
                self.write_indent();
                self.write_span(span);
                self.output.push('\n');
            }
            Ast::Import {
                name,
                alias,
                span: _,
            } => {
                self.write_indent();
                self.output.push_str("import ");
                self.output.push_str(name);

                if let Some(alias) = alias {
                    self.output.push_str(" as ");
                    self.output.push_str(alias);
                }

                self.output.push_str(";\n");
            }
            Ast::Class {
                name,
                extends,
                annotations: _,
                body,
                span: _,
            } => {
                self.write_indent();
                self.output.push_str("class ");
                self.output.push_str(name);

                if let Some(extends) = extends {
                    self.output.push_str(" extends ");
                    self.output.push_str(extends);
                }

                self.output.push_str(" {\n");
                self.indent();

                for stmt in body {
                    self.format(stmt);
                }

                self.dedent();
                self.write_indent();

                self.output.push_str("}\n");
            }
            Ast::Function {
                name,
                args,
                returns,
                annotations: _,
                body,
                span: _,
                ..
            } => {
                self.write_indent();
                self.output.push_str("function ");
                self.output.push_str(name);
                self.output.push('(');

                for (i, arg) in args.iter().enumerate() {
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

                if let Some(returns) = returns {
                    self.output.push_str(" as ");
                    self.output.push_str(&returns.ident);
                }

                self.output.push_str(" {\n");
                self.indent();

                for stmt in body {
                    self.format(stmt);
                }

                self.dedent();
                self.output.push_str("\n");
                self.write_indent();
                self.output.push_str("}\n");
            }
            Ast::Block(stmts, _) => {
                self.output.push_str(" {\n");
                self.indent();

                for stmt in stmts {
                    self.format(stmt);
                }

                self.dedent();
                self.write_indent();
                self.output.push_str("}");
            }
            Ast::If {
                condition,
                then_branch,
                else_branch,
                span: _,
            } => {
                self.write_indent();
                self.output.push_str("if (");
                self.format(condition);
                self.output.push_str(")");
                self.format(then_branch);

                if let Some(else_branch) = else_branch {
                    self.output.push_str(" else");
                    self.format(else_branch);
                }
            }
            Ast::While {
                condition,
                body,
                span: _,
            } => {
                self.write_indent();
                self.output.push_str("while (");
                self.format(condition);
                self.output.push_str(") ");
                self.format(body);
            }
            Ast::For {
                init,
                condition,
                update,
                body,
                span: _,
            } => {
                self.write_indent();
                self.raw_mode = true;
                self.output.push_str("for (");

                if let Some(init) = init {
                    self.format(init);
                    self.output.push_str("; ");
                }

                if let Some(condition) = condition {
                    self.format(condition);
                }

                self.output.push_str("; ");

                if let Some(update) = update {
                    self.format(update);
                }

                self.output.push_str(") ");
                self.raw_mode = false;

                self.format(body);
            }
            Ast::Return(value, _) => {
                self.write_indent();
                self.output.push_str("return");

                if let Some(value) = value {
                    self.output.push(' ');
                    self.format(value);
                }

                self.output.push_str(";\n");
            }
            Ast::Break(_) => {
                self.write_indent();
                self.output.push_str("break;\n");
            }
            Ast::Continue(_) => {
                self.write_indent();
                self.output.push_str("continue;\n");
            }
            Ast::Variable(var, _) => {
                self.write_indent();

                if let Some(visibility) = &var.visibility {
                    match visibility {
                        monkey_c_parser::ast::Visibility::Private => {
                            self.output.push_str("private ")
                        }
                        monkey_c_parser::ast::Visibility::Protected => {
                            self.output.push_str("protected ")
                        }
                        monkey_c_parser::ast::Visibility::Public => self.output.push_str("public "),
                    }
                }
                self.output.push_str("var ");
                self.output.push_str(&var.name);

                if let Some(initializer) = &var.initializer {
                    self.output.push_str(" = ");
                    self.format(initializer);
                }

                if let Some(type_) = &var.type_ {
                    self.output.push_str(" as ");
                    self.output.push_str(&type_.ident);
                }

                self.write_semicolon();
            }
            Ast::Assign {
                target,
                operator: _,
                value,
                span: _,
            } => {
                self.write_indent();
                self.format(target);
                self.output.push_str(" = ");
                self.format(value);
                self.output.push_str(";");
                self.write_newline();
            }
            Ast::Binary {
                left,
                operator,
                right,
                span: _,
            } => {
                self.format(left);
                self.output.push(' ');
                match operator {
                    monkey_c_parser::ast::BinaryOperator::Add => self.output.push('+'),
                    monkey_c_parser::ast::BinaryOperator::Sub => self.output.push('-'),
                    monkey_c_parser::ast::BinaryOperator::Mul => self.output.push('*'),
                    monkey_c_parser::ast::BinaryOperator::Div => self.output.push('/'),
                    monkey_c_parser::ast::BinaryOperator::Mod => self.output.push('%'),
                    monkey_c_parser::ast::BinaryOperator::Eq => self.output.push_str("=="),
                    monkey_c_parser::ast::BinaryOperator::NotEq => self.output.push_str("!="),
                    monkey_c_parser::ast::BinaryOperator::Lt => self.output.push('<'),
                    monkey_c_parser::ast::BinaryOperator::LtEq => self.output.push_str("<="),
                    monkey_c_parser::ast::BinaryOperator::Gt => self.output.push('>'),
                    monkey_c_parser::ast::BinaryOperator::GtEq => self.output.push_str(">="),
                    monkey_c_parser::ast::BinaryOperator::InstanceOf => {
                        self.output.push_str(" instanceof ")
                    }
                    monkey_c_parser::ast::BinaryOperator::And => self.output.push_str(" && "),
                    monkey_c_parser::ast::BinaryOperator::Or => self.output.push_str(" || "),
                    monkey_c_parser::ast::BinaryOperator::BitAnd => self.output.push_str(" & "),
                    monkey_c_parser::ast::BinaryOperator::BitOr => self.output.push_str(" | "),
                    monkey_c_parser::ast::BinaryOperator::BitXor => self.output.push_str(" ^ "),
                }
                self.output.push(' ');
                self.format(right);
            }
            Ast::Unary {
                operator,
                operand,
                span: _,
            } => {
                match operator {
                    monkey_c_parser::ast::UnaryOperator::Neg => self.output.push('-'),
                    monkey_c_parser::ast::UnaryOperator::Not => self.output.push('!'),
                    monkey_c_parser::ast::UnaryOperator::BitNot => self.output.push('~'),
                    monkey_c_parser::ast::UnaryOperator::PreInc => self.output.push_str("++"),
                    monkey_c_parser::ast::UnaryOperator::PreDec => self.output.push_str("--"),
                    monkey_c_parser::ast::UnaryOperator::PostInc => {
                        self.format(operand);
                        self.output.push_str("++");
                        return;
                    }
                    monkey_c_parser::ast::UnaryOperator::PostDec => {
                        self.format(operand);
                        self.output.push_str("--");
                        return;
                    }
                }
                self.format(operand);
            }
            Ast::Call {
                callee,
                args,
                span: _,
            } => {
                self.format(callee);
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format(arg);
                }
                self.output.push(')');
            }
            Ast::Member {
                object,
                property,
                span: _,
            } => {
                self.format(object);
                self.output.push('.');
                self.output.push_str(property);
            }
            Ast::Index {
                object,
                index,
                span: _,
            } => {
                self.format(object);
                self.output.push('[');
                self.format(index);
                self.output.push(']');
            }
            Ast::New {
                class,
                args,
                span: _,
            } => {
                self.output.push_str("new ");
                self.output.push_str(class);
                self.output.push('(');
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format(arg);
                }
                self.output.push(')');
            }
            Ast::TypeCast {
                expr,
                target_type,
                span: _,
            } => {
                self.format(expr);
                self.output.push_str(" as ");
                self.output.push_str(&target_type.ident);
            }
            Ast::Array(elems, _) => {
                self.output.push('[');
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format(elem);
                }
                self.output.push(']');
            }
            Ast::Dictionary(pairs, _) => {
                self.output.push('{');
                for (i, (key, value)) in pairs.iter().enumerate() {
                    if i > 0 {
                        self.output.push_str(", ");
                    }
                    self.format(key);
                    self.output.push_str(": ");
                    self.format(value);
                }
                self.output.push('}');
            }
            Ast::BasicLit(lit, _) => match lit {
                monkey_c_parser::ast::LiteralValue::Long(v) => self.output.push_str(&v.to_string()),
                monkey_c_parser::ast::LiteralValue::Double(v) => {
                    self.output.push_str(&v.to_string())
                }
                monkey_c_parser::ast::LiteralValue::String(v) => {
                    self.output.push('"');
                    self.output.push_str(v);
                    self.output.push('"');
                }
                monkey_c_parser::ast::LiteralValue::Boolean(v) => {
                    self.output.push_str(&v.to_string())
                }
                monkey_c_parser::ast::LiteralValue::Null => self.output.push_str("null"),
                monkey_c_parser::ast::LiteralValue::NaN => self.output.push_str("NaN"),
            },
            Ast::Identifier(name, _) => {
                self.output.push_str(name);
            }
            Ast::Me(_) => {
                self.output.push_str("me");
            }
            Ast::Self_(_) => {
                self.output.push_str("self");
            }
            Ast::Eof => {}
        }
    }
}
