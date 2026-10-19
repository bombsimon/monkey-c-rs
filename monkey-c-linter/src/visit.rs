//! AST walker + rule dispatch.
//!
//! A single traversal of the AST. At each node, we call every registered
//! rule's `check_*` function once; rules return `Option<Diagnostic>` and the
//! driver collects them. Adding a rule is two steps:
//! 1. Add a `pub fn check_expr(...) -> Option<Diagnostic>` (and/or
//!    `check_stmt`, …) in a new `crate::rules::*` module.
//! 2. Add one line to the matching `dispatch_*` function here.
use monkey_c_parser::ast::{Ast, CaseLabel, ElseBranch, Expr, ForInit, IfStmt, Stmt};

use crate::{Diagnostic, rules};

/// Shared context passed to every rule. Currently just the source text used
/// by rules that build fix-replacement strings from source byte ranges.
pub struct LintContext<'a> {
    pub source: &'a str,
}

/// Walk `ast`, calling every registered rule at each visited node. Diagnostics
/// are pushed onto `diags` in source order.
pub fn walk(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    walk_ast(ast, ctx, diags);
}

fn dispatch_expr(expr: &Expr, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    if let Some(d) = rules::compound_assignment::check_expr(expr, ctx) {
        diags.push(d);
    }

    if let Some(d) = rules::bool_comparison::check_expr(expr, ctx) {
        diags.push(d);
    }
}

/// Called once for the file's top-level declaration list. Rules that need
/// to reason about the *whole file* — e.g. counting class declarations
/// across modules — hook here.
fn dispatch_document(nodes: &[Ast], ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::one_class_per_file::check_document(nodes, ctx));
}

/// Called once per [`Ast`] node visited by [`walk_ast`]. Rules that care
/// about a single declaration (a class, a function, a typedef, …) hook
/// here and pattern-match on the variant they want.
fn dispatch_ast(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    if let Some(d) = rules::super_initializer_call::check_ast(ast, ctx) {
        diags.push(d);
    }

    diags.extend(rules::naming_convention::check_ast(ast, ctx));
}

/// Called once per [`Stmt`] visited by [`walk_stmt`]. Rules that care about
/// a single statement (a `var` declaration, a `throw`, …) hook here and
/// pattern-match on the variant they want.
fn dispatch_stmt(stmt: &Stmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::naming_convention::check_stmt(stmt, ctx));
}

fn walk_ast(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_ast(ast, ctx, diags);

    match ast {
        Ast::Document(nodes, _) => {
            dispatch_document(nodes, ctx, diags);
            for n in nodes {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Module(decl) => {
            for n in &decl.body {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Class(decl) => {
            for n in &decl.body {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Function(decl) => {
            if let Some(body) = &decl.body {
                for stmt in &body.stmts {
                    walk_stmt(stmt, ctx, diags);
                }
            }
        }
        Ast::Variable(decl) => {
            for b in &decl.bindings {
                if let Some(init) = &b.initializer {
                    walk_expr(init, ctx, diags);
                }
            }
        }
        Ast::Const(decl) => {
            for b in &decl.bindings {
                if let Some(init) = &b.initializer {
                    walk_expr(init, ctx, diags);
                }
            }
        }
        Ast::Typedef(_)
        | Ast::Enum(_)
        | Ast::Import(_)
        | Ast::Using(_)
        | Ast::Annotation(_, _)
        | Ast::Eof => {}
    }
}

fn walk_stmt(stmt: &Stmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_stmt(stmt, ctx, diags);

    match stmt {
        Stmt::Block(b) => {
            for s in &b.stmts {
                walk_stmt(s, ctx, diags);
            }
        }
        Stmt::If(s) => {
            // Dispatch the whole-chain check only at the chain head; `walk_if`
            // recurses through `else if` arms but never re-runs this check, so
            // each chain is analyzed exactly once.
            diags.extend(rules::ifs_same_cond::check_if(s, ctx));
            walk_if(s, ctx, diags);
        }
        Stmt::While(s) => {
            walk_expr(&s.condition.inner, ctx, diags);
            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::DoWhile(s) => {
            walk_expr(&s.condition.inner, ctx, diags);
            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::For(s) => {
            if let Some(init) = &s.header.inner.init {
                match init {
                    ForInit::Expr(exprs) => {
                        for e in exprs {
                            walk_expr(e, ctx, diags);
                        }
                    }
                    ForInit::Var(v) => {
                        rules::naming_convention::check_var_decl(v, diags);

                        for b in &v.bindings {
                            if let Some(init) = &b.initializer {
                                walk_expr(init, ctx, diags);
                            }
                        }
                    }
                }
            }

            if let Some(c) = &s.header.inner.condition {
                walk_expr(c, ctx, diags);
            }

            if let Some(updates) = &s.header.inner.update {
                for u in updates {
                    walk_expr(u, ctx, diags);
                }
            }

            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::Switch(s) => {
            walk_expr(&s.discriminant.inner, ctx, diags);
            for case in &s.cases {
                if let CaseLabel::Value(e) = &case.label {
                    walk_expr(e, ctx, diags);
                }

                for sub in &case.stmts {
                    walk_stmt(sub, ctx, diags);
                }
            }
        }
        Stmt::Try(s) => {
            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }

            for catch in &s.catches {
                for sub in &catch.body.stmts {
                    walk_stmt(sub, ctx, diags);
                }
            }

            if let Some(fin) = &s.finally {
                for sub in &fin.stmts {
                    walk_stmt(sub, ctx, diags);
                }
            }
        }
        Stmt::Throw(s) => walk_expr(&s.value, ctx, diags),
        Stmt::Return(s) => {
            if let Some(v) = &s.value {
                walk_expr(v, ctx, diags);
            }
        }
        Stmt::Var(v) => {
            for b in &v.bindings {
                if let Some(init) = &b.initializer {
                    walk_expr(init, ctx, diags);
                }
            }
        }
        Stmt::Expr(s) => walk_expr(&s.expr, ctx, diags),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn walk_if(s: &IfStmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    // Hooked here rather than in `dispatch_*` because `walk_if` is the single
    // point every `IfStmt` flows through — chain head, nested `if`, and each
    // `else if` arm alike — so the collapsible check runs exactly once per node.
    if let Some(d) = rules::collapsible_if::check_if(s, ctx) {
        diags.push(d);
    }

    walk_expr(&s.condition.inner, ctx, diags);
    for sub in &s.then_branch.stmts {
        walk_stmt(sub, ctx, diags);
    }

    if let Some(else_b) = &s.else_branch {
        match else_b {
            ElseBranch::Block(b) => {
                for sub in &b.stmts {
                    walk_stmt(sub, ctx, diags);
                }
            }
            ElseBranch::If(inner) => walk_if(inner, ctx, diags),
        }
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
