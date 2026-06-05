//! AST walker + rule dispatch.
//!
//! A single traversal of the AST. At each node, we call every registered
//! rule's `check_*` function once; rules return `Option<Diagnostic>` and the
//! driver collects them. Adding a rule is two steps:
//! 1. Add a `pub fn check_expr(...) -> Option<Diagnostic>` (and/or
//!    `check_stmt`, …) in a new `crate::rules::*` module.
//! 2. Add one line to the matching `dispatch_*` function here.
//!
//! Each `Expr` is visited with an [`ExprPosition`] describing the syntactic
//! slot it sits in. Rules use this to decide when they apply — e.g., the
//! `unneeded-parens` rule only fires for expressions in slots where any
//! expression is unambiguously parseable.
use monkey_c_parser::ast::{
    Ast, CaseLabel, ElseBranch, Expr, ForInit, IfStmt, InterfaceMember, Stmt, Type, TypeKind,
};

use crate::{Diagnostic, rules};

/// The syntactic slot an [`Expr`] occupies in its parent. Rules can match on
/// this to determine whether they apply. New variants are added as new rules
/// need to distinguish positions; the catch-all is [`ExprPosition::Other`].
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprPosition {
    /// Right-hand side of `=` / `+=` / `<<=` / etc.
    AssignValue,
    /// Initializer of a `var`/`const` binding (`var x = <expr>`).
    Initializer,
    /// Value of a `return` statement.
    ReturnValue,
    /// Condition expression of `if`/`while`/`do-while`/`for`.
    Condition,
    /// Operand of a binary operator.
    BinaryOperand,
    /// Operand of a unary operator (prefix or postfix).
    UnaryOperand,
    /// Function-call argument or `new`-expression argument.
    CallArg,
    /// Object position of a `.member` or `[index]` (`obj.foo`, `arr[i]`).
    MemberOrIndexObject,
    /// Anywhere else not covered above. New positions are promoted as rules
    /// need to distinguish them.
    Other,
}

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

fn dispatch_expr(expr: &Expr, pos: ExprPosition, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    if let Some(d) = rules::unneeded_parens::check_expr(expr, pos, ctx) {
        diags.push(d);
    }

    if let Some(d) = rules::compound_assignment::check_expr(expr, pos, ctx) {
        diags.push(d);
    }
}

/// Called for each list of sibling [`Ast`] declarations (document body,
/// module body, class body). Rules that care about *order* or *grouping* of
/// siblings hook here.
fn dispatch_ast_seq(seq: &[Ast], ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    diags.extend(rules::import_order::check_ast_seq(seq, ctx));
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

fn dispatch_type(ty: &Type, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    if let Some(d) = rules::unneeded_parens::check_type(ty, ctx) {
        diags.push(d);
    }
}

fn walk_ast(ast: &Ast, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_ast(ast, ctx, diags);

    match ast {
        Ast::Document(nodes, _) => {
            dispatch_document(nodes, ctx, diags);
            dispatch_ast_seq(nodes, ctx, diags);
            for n in nodes {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Module(decl) => {
            dispatch_ast_seq(&decl.body, ctx, diags);
            for n in &decl.body {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Class(decl) => {
            dispatch_ast_seq(&decl.body, ctx, diags);
            for n in &decl.body {
                walk_ast(n, ctx, diags);
            }
        }
        Ast::Function(decl) => {
            for arg in &decl.args.inner {
                if let Some(t) = &arg.type_ {
                    walk_type(t, ctx, diags);
                }
            }

            if let Some(ret) = &decl.returns {
                walk_type(ret, ctx, diags);
            }

            for stmt in &decl.body.stmts {
                walk_stmt(stmt, ctx, diags);
            }
        }
        Ast::Variable(decl) => {
            for b in &decl.bindings {
                if let Some(t) = &b.type_ {
                    walk_type(t, ctx, diags);
                }

                if let Some(init) = &b.initializer {
                    walk_expr(init, ExprPosition::Initializer, ctx, diags);
                }
            }
        }
        Ast::Const(decl) => {
            for b in &decl.bindings {
                if let Some(t) = &b.type_ {
                    walk_type(t, ctx, diags);
                }

                if let Some(init) = &b.initializer {
                    walk_expr(init, ExprPosition::Initializer, ctx, diags);
                }
            }
        }
        Ast::Typedef(decl) => walk_type(&decl.type_, ctx, diags),
        Ast::Enum(_) | Ast::Import(_) | Ast::Using(_) | Ast::Annotation(_, _) | Ast::Eof => {}
    }
}

fn walk_stmt(stmt: &Stmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    match stmt {
        Stmt::Block(b) => {
            for s in &b.stmts {
                walk_stmt(s, ctx, diags);
            }
        }
        Stmt::If(s) => walk_if(s, ctx, diags),
        Stmt::While(s) => {
            walk_expr(&s.condition.inner, ExprPosition::Condition, ctx, diags);
            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::DoWhile(s) => {
            walk_expr(&s.condition, ExprPosition::Condition, ctx, diags);
            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::For(s) => {
            if let Some(init) = &s.header.inner.init {
                match init {
                    ForInit::Expr(e) => walk_expr(e, ExprPosition::Other, ctx, diags),
                    ForInit::Var(v) => {
                        for b in &v.bindings {
                            if let Some(t) = &b.type_ {
                                walk_type(t, ctx, diags);
                            }

                            if let Some(init) = &b.initializer {
                                walk_expr(init, ExprPosition::Initializer, ctx, diags);
                            }
                        }
                    }
                }
            }

            if let Some(c) = &s.header.inner.condition {
                walk_expr(c, ExprPosition::Condition, ctx, diags);
            }

            if let Some(updates) = &s.header.inner.update {
                for u in updates {
                    walk_expr(u, ExprPosition::Other, ctx, diags);
                }
            }

            for sub in &s.body.stmts {
                walk_stmt(sub, ctx, diags);
            }
        }
        Stmt::Switch(s) => {
            walk_expr(&s.discriminant.inner, ExprPosition::Other, ctx, diags);
            for case in &s.cases {
                match &case.label {
                    CaseLabel::Value(e) => walk_expr(e, ExprPosition::Other, ctx, diags),
                    CaseLabel::InstanceOf(t) => walk_type(t, ctx, diags),
                    CaseLabel::Default => {}
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
                if let Some(t) = &catch.type_filter {
                    walk_type(t, ctx, diags);
                }

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
        Stmt::Throw(s) => walk_expr(&s.value, ExprPosition::Other, ctx, diags),
        Stmt::Return(s) => {
            if let Some(v) = &s.value {
                walk_expr(v, ExprPosition::ReturnValue, ctx, diags);
            }
        }
        Stmt::Var(v) => {
            for b in &v.bindings {
                if let Some(t) = &b.type_ {
                    walk_type(t, ctx, diags);
                }

                if let Some(init) = &b.initializer {
                    walk_expr(init, ExprPosition::Initializer, ctx, diags);
                }
            }
        }
        Stmt::Expr(e) => walk_expr(e, ExprPosition::Other, ctx, diags),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn walk_type(ty: &Type, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_type(ty, ctx, diags);

    match &ty.kind {
        TypeKind::Named { generic_params, .. } => {
            for p in generic_params {
                walk_type(p, ctx, diags);
            }
        }
        TypeKind::Dict { entries, .. } => {
            for entry in entries {
                walk_type(&entry.value_type, ctx, diags);
            }
        }
        TypeKind::Tuple { elements } => {
            for el in elements {
                walk_type(el, ctx, diags);
            }
        }
        TypeKind::Method { args, returns, .. } => {
            for arg in args {
                if let Some(t) = &arg.type_ {
                    walk_type(t, ctx, diags);
                }
            }

            if let Some(ret) = returns {
                walk_type(ret, ctx, diags);
            }
        }
        TypeKind::Interface { members, .. } => {
            for m in members {
                match m {
                    InterfaceMember::Function(f) => {
                        for arg in &f.args {
                            if let Some(t) = &arg.type_ {
                                walk_type(t, ctx, diags);
                            }
                        }

                        if let Some(ret) = &f.returns {
                            walk_type(ret, ctx, diags);
                        }
                    }
                    InterfaceMember::Variable(v) => walk_type(&v.type_, ctx, diags),
                }
            }
        }
        TypeKind::Group(group) => walk_type(&group.inner, ctx, diags),
    }

    for alt in &ty.alternatives {
        walk_type(alt, ctx, diags);
    }
}

fn walk_if(s: &IfStmt, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    walk_expr(&s.condition.inner, ExprPosition::Condition, ctx, diags);
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

fn walk_expr(expr: &Expr, pos: ExprPosition, ctx: &LintContext, diags: &mut Vec<Diagnostic>) {
    dispatch_expr(expr, pos, ctx, diags);

    match expr {
        Expr::Binary(b) => {
            walk_expr(&b.left, ExprPosition::BinaryOperand, ctx, diags);
            walk_expr(&b.right, ExprPosition::BinaryOperand, ctx, diags);
        }
        Expr::Unary(u) => walk_expr(&u.operand, ExprPosition::UnaryOperand, ctx, diags),
        Expr::Ternary(t) => {
            walk_expr(&t.cond, ExprPosition::Condition, ctx, diags);
            walk_expr(&t.then_expr, ExprPosition::Other, ctx, diags);
            walk_expr(&t.else_expr, ExprPosition::Other, ctx, diags);
        }
        Expr::Assign(a) => {
            walk_expr(&a.target, ExprPosition::Other, ctx, diags);
            walk_expr(&a.value, ExprPosition::AssignValue, ctx, diags);
        }
        Expr::Call(c) => {
            walk_expr(&c.callee, ExprPosition::Other, ctx, diags);
            for arg in &c.args {
                walk_expr(&arg.value, ExprPosition::CallArg, ctx, diags);
            }
        }
        Expr::Member(m) => walk_expr(&m.object, ExprPosition::MemberOrIndexObject, ctx, diags),
        Expr::Index(i) => {
            walk_expr(&i.object, ExprPosition::MemberOrIndexObject, ctx, diags);
            walk_expr(&i.index, ExprPosition::Other, ctx, diags);
        }
        Expr::New(n) => {
            for arg in &n.args {
                walk_expr(&arg.value, ExprPosition::CallArg, ctx, diags);
            }
        }
        Expr::NewArray(n) => {
            walk_expr(&n.size, ExprPosition::Other, ctx, diags);

            if let Some(t) = &n.element_type {
                walk_type(t, ctx, diags);
            }
        }
        Expr::TypeCast(t) => {
            walk_expr(&t.expr, ExprPosition::Other, ctx, diags);
            walk_type(&t.target_type, ctx, diags);
        }
        Expr::Array(a) => {
            for entry in &a.entries {
                walk_expr(&entry.value, ExprPosition::Other, ctx, diags);
            }
        }
        Expr::Dict(d) => {
            for entry in &d.entries {
                walk_expr(&entry.key, ExprPosition::Other, ctx, diags);
                walk_expr(&entry.value, ExprPosition::Other, ctx, diags);
            }
        }
        Expr::Paren(p) => walk_expr(&p.inner, ExprPosition::Other, ctx, diags),
        Expr::Lit(_) | Expr::Ident(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => {}
    }
}
