//! What a file means, as opposed to how it's spelled, so formatting can be checked to never change
//! it. Every rewrite the formatter can make is applied to the whole tree here, wherever it's safe
//! or not, since a rewrite that changes how the code reads leaves a different tree behind to
//! compare.

use monkey_c_parser::ast::{
    Ast, BlockStmt, CaseLabel, ElseBranch, Expr, ForInit, IfStmt, InterfaceMember, LiteralValue,
    Parameter, Separated, Span, Spanned, Stmt, Type, TypeKind, UnaryOperator, UnionAlternative,
    UnionSeparator, VarDecl,
};

use std::mem;

/// `ast` with every spelling rewrite applied and every source position left out.
pub fn meaning(mut ast: Ast) -> String {
    normalize_ast(&mut ast);

    without_positions(&format!("{ast:?}"))
}

/// Positions are the only numbers outside the quoted strings of the tree's debug output, since
/// literals keep their source text.
fn without_positions(debug: &str) -> String {
    let mut out = String::with_capacity(debug.len());
    let mut quote = None;
    let mut escaped = false;

    for c in debug.chars() {
        match quote {
            Some(_) if escaped => escaped = false,
            Some(_) if c == '\\' => escaped = true,
            Some(open) if c == open => quote = None,
            Some(_) => {}
            None if c == '"' || c == '\'' => quote = Some(c),
            None if c.is_ascii_digit() => continue,
            None => {}
        }

        out.push(c);
    }

    out
}

fn normalize_ast(ast: &mut Ast) {
    match ast {
        Ast::Document(nodes, _) => normalize_decls(nodes),
        Ast::Annotation(entries, _) => {
            for entry in entries {
                entry.preceding_comma_pos = None;
                entry.args.iter_mut().for_each(normalize_expr);
            }
        }
        Ast::Class(decl) => normalize_decls(&mut decl.body),
        Ast::Module(decl) => normalize_decls(&mut decl.body),
        Ast::Const(decl) => decl.bindings.items.iter_mut().for_each(|binding| {
            binding.type_.iter_mut().for_each(normalize_type);
            binding
                .initializer
                .iter_mut()
                .for_each(|e| normalize_expr(e));
        }),
        Ast::Variable(decl) => normalize_var(decl),
        Ast::Enum(decl) => decl
            .variants
            .items
            .iter_mut()
            .for_each(|variant| variant.value.iter_mut().for_each(normalize_expr)),
        Ast::Function(decl) => {
            normalize_params(&mut decl.parameters.inner);
            decl.returns.iter_mut().for_each(normalize_type);
            decl.body.iter_mut().for_each(normalize_block);
        }
        Ast::Typedef(decl) => normalize_type(&mut decl.type_),
        Ast::Import(_) | Ast::Using(_) | Ast::Eof => {}
    }
}

/// Sorts every run of imports, the way the formatter writes them. An annotation applies to the
/// declaration after it, so the import it's written on stays first.
fn normalize_decls(nodes: &mut [Ast]) {
    nodes.iter_mut().for_each(normalize_ast);

    let mut start = 0;
    while start < nodes.len() {
        let annotated = start
            .checked_sub(1)
            .is_some_and(|previous| matches!(nodes[previous], Ast::Annotation(..)));
        let first = if annotated { start + 1 } else { start };
        let len = nodes[start..]
            .iter()
            .take_while(|node| import_key(node).is_some())
            .count();

        if first < start + len {
            nodes[first..start + len].sort_by_key(import_key);
        }

        start += len.max(1);
    }
}

fn import_key(node: &Ast) -> Option<(u8, String)> {
    let (name, kind) = match node {
        Ast::Using(decl) => (&decl.name.node, 0),
        Ast::Import(decl) => (&decl.name.node, 1),
        _ => return None,
    };

    let toybox = name == "Toybox" || name.starts_with("Toybox.");

    Some((if toybox { kind } else { kind + 2 }, name.clone()))
}

fn normalize_var(decl: &mut VarDecl) {
    for binding in decl.bindings.items.iter_mut() {
        binding.type_.iter_mut().for_each(normalize_type);
        binding
            .initializer
            .iter_mut()
            .for_each(|e| normalize_expr(e));
    }
}

fn normalize_params(params: &mut Separated<Parameter>) {
    for param in params.items.iter_mut() {
        param.type_.iter_mut().for_each(normalize_type);
        param.initializer.iter_mut().for_each(|e| normalize_expr(e));
    }
}

fn normalize_block(block: &mut BlockStmt) {
    block.stmts.iter_mut().for_each(normalize_stmt);
}

fn normalize_stmt(stmt: &mut Stmt) {
    match stmt {
        Stmt::Block(block) => normalize_block(block),
        Stmt::If(s) => normalize_if(s),
        Stmt::While(s) => {
            normalize_expr(&mut s.condition.inner);
            normalize_block(&mut s.body);
        }
        Stmt::DoWhile(s) => {
            normalize_block(&mut s.body);
            normalize_expr(&mut s.condition.inner);
        }
        Stmt::For(s) => {
            let header = &mut s.header.inner;
            match &mut header.init {
                Some(ForInit::Var(decl)) => normalize_var(decl),
                Some(ForInit::Expr(exprs)) => exprs.iter_mut().for_each(normalize_expr),
                None => {}
            }

            header.condition.iter_mut().for_each(normalize_expr);
            header.update.iter_mut().flatten().for_each(normalize_expr);
            normalize_block(&mut s.body);
        }
        Stmt::Return(s) => s.value.iter_mut().for_each(normalize_expr),
        Stmt::Switch(s) => {
            normalize_expr(&mut s.discriminant.inner);

            for case in &mut s.cases {
                match &mut case.label {
                    CaseLabel::Value(e) => normalize_expr(e),
                    CaseLabel::InstanceOf(ty) => normalize_type(ty),
                    CaseLabel::Default => {}
                }

                case.stmts.iter_mut().for_each(normalize_stmt);
            }
        }
        Stmt::Try(s) => {
            normalize_block(&mut s.body);

            for catch in &mut s.catches {
                catch.type_filter.iter_mut().for_each(normalize_type);
                normalize_block(&mut catch.body);
            }

            s.finally.iter_mut().for_each(normalize_block);
        }
        Stmt::Throw(s) => normalize_expr(&mut s.value),
        Stmt::Var(decl) => normalize_var(decl),
        Stmt::Expr(s) => normalize_expr(&mut s.expr),
        Stmt::Break(_) | Stmt::Continue(_) => {}
    }
}

fn normalize_if(s: &mut IfStmt) {
    normalize_expr(&mut s.condition.inner);
    normalize_block(&mut s.then_branch);

    match &mut s.else_branch {
        Some(ElseBranch::Block(block)) => normalize_block(block),
        Some(ElseBranch::If(inner)) => normalize_if(inner),
        None => {}
    }
}

/// Parentheses and the `@` of a resource reference go, since the tree already holds the grouping
/// they spell out.
fn normalize_expr(expr: &mut Expr) {
    loop {
        let unwrapped = match expr {
            Expr::Paren(_) => {
                let Expr::Paren(paren) = mem::replace(expr, placeholder_expr()) else {
                    unreachable!()
                };

                *paren.inner
            }
            Expr::Unary(unary) if unary.operator == UnaryOperator::ResourceRef => {
                let Expr::Unary(unary) = mem::replace(expr, placeholder_expr()) else {
                    unreachable!()
                };

                *unary.operand
            }
            _ => break,
        };

        *expr = unwrapped;
    }

    match expr {
        Expr::Binary(e) => {
            normalize_expr(&mut e.left);
            normalize_expr(&mut e.right);
        }
        Expr::Unary(e) => normalize_expr(&mut e.operand),
        Expr::Ternary(e) => {
            normalize_expr(&mut e.condition);
            normalize_expr(&mut e.then_expr);
            normalize_expr(&mut e.else_expr);
        }
        Expr::Assign(e) => {
            normalize_expr(&mut e.target);
            normalize_expr(&mut e.value);
        }
        Expr::Call(e) => {
            normalize_expr(&mut e.callee);
            e.args
                .items
                .iter_mut()
                .for_each(|arg| normalize_expr(&mut arg.value));
        }
        Expr::Member(e) => normalize_expr(&mut e.object),
        Expr::Index(e) => {
            normalize_expr(&mut e.object);
            normalize_expr(&mut e.index);
        }
        Expr::New(e) => {
            // `new Foo` is written `new Foo()`.
            e.args_open = None;
            e.args
                .items
                .iter_mut()
                .for_each(|arg| normalize_expr(&mut arg.value));
        }
        Expr::NewArray(e) => {
            e.element_type.iter_mut().for_each(normalize_type);
            normalize_expr(&mut e.size);
        }
        Expr::TypeCast(e) => {
            normalize_expr(&mut e.expr);
            normalize_type(&mut e.target_type);
        }
        Expr::Array(e) => e
            .entries
            .items
            .iter_mut()
            .for_each(|entry| normalize_expr(&mut entry.value)),
        Expr::Dict(e) => {
            for entry in e.entries.items.iter_mut() {
                normalize_expr(&mut entry.key);
                normalize_expr(&mut entry.value);
            }
        }
        Expr::Lit(e) => normalize_literal(&mut e.value),
        Expr::Ident(_) | Expr::Paren(_) | Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => {}
    }
}

/// Groups are opened up, and a union is rewritten as its members other than `Null`, with the
/// first marked nullable when any member could be `null`. That's what both `T?` and `T or Null`
/// say.
fn normalize_type(ty: &mut Type) {
    match &mut ty.kind {
        TypeKind::Named { generic_params, .. } => {
            generic_params.iter_mut().for_each(normalize_type);
        }
        TypeKind::Dict { entries, .. } => entries
            .items
            .iter_mut()
            .for_each(|entry| normalize_type(&mut entry.value_type)),
        TypeKind::Interface { members, .. } => {
            for member in members {
                match member {
                    InterfaceMember::Function(method) => {
                        normalize_params(&mut method.args.inner);
                        method.returns.iter_mut().for_each(normalize_type);
                    }
                    InterfaceMember::Variable(var) => normalize_type(&mut var.type_),
                }
            }
        }
        TypeKind::Tuple { elements } => elements.iter_mut().for_each(normalize_type),
        TypeKind::Method { args, returns, .. } => {
            normalize_params(&mut args.inner);
            returns.iter_mut().for_each(|ret| normalize_type(ret));
        }
        TypeKind::Group(group) => normalize_type(&mut group.inner),
    }

    for alternative in &mut ty.alternatives {
        normalize_type(&mut alternative.type_);
    }

    if matches!(ty.kind, TypeKind::Group(_)) {
        let TypeKind::Group(group) = mem::replace(&mut ty.kind, placeholder_kind()) else {
            unreachable!()
        };
        let inner = *group.inner;

        ty.kind = inner.kind;
        ty.optional |= inner.optional;
        ty.alternatives.splice(0..0, inner.alternatives);
    }

    let first = Type {
        kind: mem::replace(&mut ty.kind, placeholder_kind()),
        alternatives: Vec::new(),
        optional: ty.optional,
        span: ty.span,
    };
    let mut members = vec![first];
    members.extend(
        ty.alternatives
            .drain(..)
            .map(|alternative| alternative.type_),
    );

    let mut nullable = false;
    for member in &mut members {
        nullable |= mem::take(&mut member.optional) || is_null(member);
    }

    let mut members = members.into_iter().filter(|member| !is_null(member));
    let Some(first) = members.next() else {
        ty.kind = TypeKind::Named {
            ident: "Null".to_string(),
            generic_params: Vec::new(),
        };
        ty.optional = false;

        return;
    };

    ty.kind = first.kind;
    ty.optional = nullable;
    ty.alternatives = members
        .map(|member| UnionAlternative {
            separator: Spanned {
                span: Span { start: 0, end: 0 },
                node: UnionSeparator::Or,
            },
            type_: member,
        })
        .collect();
}

fn is_null(ty: &Type) -> bool {
    let TypeKind::Named {
        ident,
        generic_params,
    } = &ty.kind
    else {
        return false;
    };

    generic_params.is_empty() && matches!(ident.as_str(), "Null" | "Lang.Null" | "Toybox.Lang.Null")
}

fn placeholder_expr() -> Expr {
    Expr::Me(Span { start: 0, end: 0 })
}

fn placeholder_kind() -> TypeKind {
    TypeKind::Tuple {
        elements: Vec::new(),
    }
}

/// Number literals in one spelling per value: lowercase, with a leading zero, no trailing zeros
/// past the first after the `.` and no padding in the exponent. Whether there's a `.`, an exponent
/// or a suffix at all is left alone, since that's the type.
fn normalize_literal(value: &mut LiteralValue) {
    match value {
        LiteralValue::Hex(text) | LiteralValue::HexLong(text) => {
            *text = text.to_ascii_lowercase();
        }
        LiteralValue::Long(text) => *text = normalize_long(text),
        LiteralValue::Float(lit) => {
            lit.digits = normalize_fraction(&lit.digits);
            lit.leading_dot = false;
            lit.exponent = lit.exponent.as_deref().map(normalize_exponent);
            lit.suffix = lit.suffix.map(|suffix| suffix.to_ascii_lowercase());
        }
        LiteralValue::Double(lit) => {
            lit.digits = normalize_fraction(&lit.digits);
            lit.leading_dot = false;
            lit.exponent = lit.exponent.as_deref().map(normalize_exponent);
            lit.suffix = lit.suffix.to_ascii_lowercase();
        }
        _ => {}
    }
}

/// A long keeps its text whole, so its fraction and exponent are split out like a float's.
fn normalize_long(text: &str) -> String {
    let body = &text[..text.len() - 1];
    let (digits, exponent) = match body.find(['e', 'E']) {
        Some(index) => (&body[..index], normalize_exponent(&body[index..])),
        None => (body, String::new()),
    };

    format!("{}{exponent}l", normalize_fraction(digits))
}

fn normalize_fraction(digits: &str) -> String {
    let digits = match digits.strip_prefix('.') {
        Some(fraction) => format!("0.{fraction}"),
        None => digits.to_string(),
    };

    let Some((whole, fraction)) = digits.split_once('.') else {
        return digits;
    };

    let fraction = fraction.trim_end_matches('0');
    let fraction = if fraction.is_empty() { "0" } else { fraction };

    format!("{whole}.{fraction}")
}

fn normalize_exponent(exponent: &str) -> String {
    let exponent = exponent.to_ascii_lowercase();
    let digits = exponent[1..].trim_start_matches('+');
    let (sign, digits) = match digits.strip_prefix('-') {
        Some(digits) => ("-", digits),
        None => ("", digits),
    };
    let digits = digits.trim_start_matches('0');
    if digits.is_empty() {
        return "e0".to_string();
    }

    format!("e{sign}{digits}")
}
