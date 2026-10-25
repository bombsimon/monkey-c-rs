//! A single walk over the AST that calls every rule at the nodes it checks. Adding a rule means
//! calling its `check_*` function from the matching `dispatch_*` here.
use monkey_c_parser::ast::{Ast, Binding, CaseLabel, ElseBranch, Expr, ForInit, IfStmt, Stmt};

use crate::{Diagnostic, rules};

/// What rules can read besides the node they check, such as the source text to build fixes from.
pub struct LintContext<'a> {
    pub source: &'a str,
}

fn dispatch_expr(expr: &Expr, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::compound_assignment::check_expr(expr, ctx));
    diags.extend(rules::bool_comparison::check_expr(expr, ctx));
}

/// Called once with the file's top-level declarations, for rules about the whole file.
fn dispatch_document(nodes: &[Ast], ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::one_class_per_file::check_document(nodes, ctx));
}

fn dispatch_ast(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::super_initializer_call::check_ast(ast, ctx));
    diags.extend(rules::naming_convention::check_ast(ast, ctx));
}

fn dispatch_stmt(stmt: &Stmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::naming_convention::check_stmt(stmt, ctx));
}

/// Walk `ast`, calling every rule at each node it checks.
pub fn walk(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_ast(ast, ctx, diags);

    match ast {
        Ast::Document(nodes, _) => {
            dispatch_document(nodes, ctx, diags);
            for node in nodes {
                walk(node, ctx, diags);
            }
        }
        Ast::Module(decl) => {
            for node in &decl.body {
                walk(node, ctx, diags);
            }
        }
        Ast::Class(decl) => {
            for node in &decl.body {
                walk(node, ctx, diags);
            }
        }
        Ast::Function(decl) => {
            if let Some(body) = &decl.body {
                walk_stmts(&body.stmts, ctx, diags);
            }
        }
        Ast::Variable(decl) => walk_bindings(&decl.bindings, ctx, diags),
        Ast::Const(decl) => walk_bindings(&decl.bindings, ctx, diags),
        Ast::Typedef(_)
        | Ast::Enum(_)
        | Ast::Import(_)
        | Ast::Using(_)
        | Ast::Annotation(_, _)
        | Ast::Eof => {}
    }
}

fn walk_stmts(stmts: &[Stmt], ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    for stmt in stmts {
        walk_stmt(stmt, ctx, diags);
    }
}

fn walk_bindings(bindings: &[Binding], ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    for binding in bindings {
        if let Some(init) = &binding.initializer {
            walk_expr(init, ctx, diags);
        }
    }
}

fn walk_stmt(stmt: &Stmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_stmt(stmt, ctx, diags);

    match stmt {
        Stmt::Block(b) => walk_stmts(&b.stmts, ctx, diags),
        Stmt::If(s) => {
            // Only at the head of a chain, since it checks the whole chain at once.
            diags.extend(rules::ifs_same_cond::check_if(s, ctx));
            walk_if(s, ctx, diags);
        }
        Stmt::While(s) => {
            walk_expr(&s.condition.inner, ctx, diags);
            walk_stmts(&s.body.stmts, ctx, diags);
        }
        Stmt::DoWhile(s) => {
            walk_expr(&s.condition.inner, ctx, diags);
            walk_stmts(&s.body.stmts, ctx, diags);
        }
        Stmt::For(s) => {
            match &s.header.inner.init {
                Some(ForInit::Expr(exprs)) => {
                    for e in exprs {
                        walk_expr(e, ctx, diags);
                    }
                }
                Some(ForInit::Var(v)) => {
                    rules::naming_convention::check_var_decl(v, diags);
                    walk_bindings(&v.bindings, ctx, diags);
                }
                None => {}
            }

            if let Some(c) = &s.header.inner.condition {
                walk_expr(c, ctx, diags);
            }

            for update in s.header.inner.update.iter().flatten() {
                walk_expr(update, ctx, diags);
            }

            walk_stmts(&s.body.stmts, ctx, diags);
        }
        Stmt::Switch(s) => {
            walk_expr(&s.discriminant.inner, ctx, diags);
            for case in &s.cases {
                if let CaseLabel::Value(e) = &case.label {
                    walk_expr(e, ctx, diags);
                }

                walk_stmts(&case.stmts, ctx, diags);
            }
        }
        Stmt::Try(s) => {
            walk_stmts(&s.body.stmts, ctx, diags);

            for catch in &s.catches {
                walk_stmts(&catch.body.stmts, ctx, diags);
            }

            if let Some(finally) = &s.finally {
                walk_stmts(&finally.stmts, ctx, diags);
            }
        }
        Stmt::Throw(s) => walk_expr(&s.value, ctx, diags),
        Stmt::Return(s) => {
            if let Some(v) = &s.value {
                walk_expr(v, ctx, diags);
            }
        }
        Stmt::Var(v) => walk_bindings(&v.bindings, ctx, diags),
        Stmt::Expr(s) => walk_expr(&s.expr, ctx, diags),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

/// Every `if` passes through here, the head of a chain and each `else if` alike.
fn walk_if(s: &IfStmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::collapsible_if::check_if(s, ctx));

    walk_expr(&s.condition.inner, ctx, diags);
    walk_stmts(&s.then_branch.stmts, ctx, diags);

    match &s.else_branch {
        Some(ElseBranch::Block(b)) => walk_stmts(&b.stmts, ctx, diags),
        Some(ElseBranch::If(inner)) => walk_if(inner, ctx, diags),
        None => {}
    }
}

fn walk_expr(expr: &Expr, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_expr(expr, ctx, diags);

    match expr {
        Expr::Binary(b) => {
            walk_expr(&b.left, ctx, diags);
            walk_expr(&b.right, ctx, diags);
        }
        Expr::Unary(u) => walk_expr(&u.operand, ctx, diags),
        Expr::Ternary(t) => {
            walk_expr(&t.condition, ctx, diags);
            walk_expr(&t.then_expr, ctx, diags);
            walk_expr(&t.else_expr, ctx, diags);
        }
        Expr::Assign(a) => {
            walk_expr(&a.target, ctx, diags);
            walk_expr(&a.value, ctx, diags);
        }
        Expr::Call(c) => {
            walk_expr(&c.callee, ctx, diags);
            for arg in &c.args {
                walk_expr(&arg.value, ctx, diags);
            }
        }
        Expr::Member(m) => walk_expr(&m.object, ctx, diags),
        Expr::Index(i) => {
            walk_expr(&i.object, ctx, diags);
            walk_expr(&i.index, ctx, diags);
        }
        Expr::New(n) => {
            for arg in &n.args {
                walk_expr(&arg.value, ctx, diags);
            }
        }
        Expr::NewArray(n) => walk_expr(&n.size, ctx, diags),
        Expr::TypeCast(t) => walk_expr(&t.expr, ctx, diags),
        Expr::Array(a) => {
            for entry in &a.entries {
                walk_expr(&entry.value, ctx, diags);
            }
        }
        Expr::Dict(d) => {
            for entry in &d.entries {
                walk_expr(&entry.key, ctx, diags);
                walk_expr(&entry.value, ctx, diags);
            }
        }
        Expr::Paren(p) => walk_expr(&p.inner, ctx, diags),
        Expr::Lit(_) | Expr::Ident(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => {}
    }
}
