use crate::ast::{Expr, Ident, Parens, Position, Span, Type, VarDecl};

/// A statement inside a function body.
#[derive(Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    /// Leaving the innermost loop or `switch`, e.g. `break;`.
    Break(Span),
    /// Skipping to the next iteration of a loop, e.g. `continue;`.
    Continue(Span),
    /// A block on its own, e.g. `{ … }`.
    Block(BlockStmt),
    /// A conditional with optional `else if` and `else` branches.
    If(IfStmt),
    /// A loop testing its condition first, e.g. `while (cond) { … }`.
    While(WhileStmt),
    /// A loop testing its condition last, e.g. `do { … } while (cond);`.
    DoWhile(DoWhileStmt),
    /// A loop with a header, e.g. `for (init; cond; update) { … }`.
    For(ForStmt),
    /// Returning from a function, e.g. `return value;`.
    Return(ReturnStmt),
    /// Picking a branch by value or type, e.g. `switch (value) { case 1: … }`.
    Switch(SwitchStmt),
    /// Catching exceptions, e.g. `try { … } catch (e) { … }`.
    Try(TryStmt),
    /// Raising an exception, e.g. `throw value;`.
    Throw(ThrowStmt),
    /// A local variable declaration, e.g. `var x = 1;`.
    Var(VarDecl),
    /// An expression used as a statement, like a call or an assignment.
    Expr(ExprStmt),
}

/// An expression followed by `;`, such as an assignment or a call.
#[derive(Debug, PartialEq)]
pub struct ExprStmt {
    pub expr: Expr,
    pub semi_pos: Position,
    pub span: Span,
}

/// The statements between `{` and `}`, with the span covering both braces.
#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

/// If statement with potential `else if` and `else` chains.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#if-statements>
#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Parens<Expr>,
    pub then_branch: BlockStmt,
    pub else_branch: Option<ElseBranch>,
    pub else_kw_start: Option<Position>,
    pub span: Span,
}

/// The branch following an `else` keyword, either a plain block or another `if` statement (i.e.
/// `else if`).
#[derive(Debug, PartialEq)]
pub enum ElseBranch {
    Block(BlockStmt),
    If(Box<IfStmt>),
}

impl ElseBranch {
    /// The source range of the block or the `if` after `else`.
    pub fn span(&self) -> &Span {
        match self {
            ElseBranch::Block(b) => &b.span,
            ElseBranch::If(s) => &s.span,
        }
    }
}

/// While statement, e.g. `while (cond) { ... }`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#loops>
#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub condition: Parens<Expr>,
    pub body: BlockStmt,
    pub span: Span,
}

/// A `do { ... } while (cond)` statement.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#loops>
#[derive(Debug, PartialEq)]
pub struct DoWhileStmt {
    pub body: BlockStmt,
    pub while_kw_start: Position,
    pub condition: Parens<Expr>,
    pub semi_pos: Position,
    pub span: Span,
}

/// The init clause of a `for` loop, either a `var` declaration or one or more comma-separated
/// expressions, e.g. `for (i = 0, j = 1; …)`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#loops>
#[derive(Debug, PartialEq)]
pub enum ForInit {
    Var(VarDecl),
    Expr(Vec<Expr>),
}

/// The `(init; cond; update)` of a `for` loop, each part optional, with where its two `;` are.
#[derive(Debug, PartialEq)]
pub struct ForHeader {
    pub init: Option<ForInit>,
    pub first_semi: Position,
    pub condition: Option<Expr>,
    pub second_semi: Position,
    pub update: Option<Vec<Expr>>,
}

/// A for loop statement containing the header and block body.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#loops>
#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub header: Parens<ForHeader>,
    pub body: BlockStmt,
    pub span: Span,
}

/// A return statement, with or without a value.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#returning-values-from-functions>
#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub semi_pos: Position,
    pub span: Span,
}

/// A `switch (discriminant) { … }` statement.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#switch-case-statements>
#[derive(Debug, PartialEq)]
pub struct SwitchStmt {
    pub discriminant: Parens<Expr>,
    pub cases: Vec<SwitchCase>,
    pub brace_start: Position,
    pub span: Span,
}

/// One arm of a `switch`, with the statements up to the next arm. An arm without statements
/// falls through to the next.
#[derive(Debug, PartialEq)]
pub struct SwitchCase {
    pub label: CaseLabel,
    pub stmts: Vec<Stmt>,
    /// From `case` or `default` through the `:`.
    pub label_span: Span,
    pub span: Span,
}

/// What a `switch` arm matches.
#[derive(Debug, PartialEq)]
pub enum CaseLabel {
    /// Matching a value, e.g. `case 1:`.
    Value(Expr),
    /// Matching a type, e.g. `case instanceof Number:`.
    InstanceOf(Type),
    /// The fallback arm, `default:`.
    Default,
}

/// A `try` statement: `try { … } catch (…) { … }` with optional `finally`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#exception-handling>
#[derive(Debug, PartialEq)]
pub struct TryStmt {
    pub body: BlockStmt,
    pub catches: Vec<CatchClause>,
    pub finally: Option<BlockStmt>,
    pub span: Span,
}

/// A single `catch (binding [instanceof Type]) { … }` arm of a try statement.
#[derive(Debug, PartialEq)]
pub struct CatchClause {
    pub binding: Ident,
    /// The `Type` of `catch (e instanceof Type)`, or `None` to catch anything.
    pub type_filter: Option<Type>,
    pub body: BlockStmt,
    pub span: Span,
}

/// A `throw <expr>;` statement.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#exception-handling>
#[derive(Debug, PartialEq)]
pub struct ThrowStmt {
    pub value: Expr,
    pub semi_pos: Position,
    pub span: Span,
}

impl Stmt {
    /// The source range of the statement, its `;` included.
    pub fn span(&self) -> &Span {
        match self {
            Stmt::Block(s) => &s.span,
            Stmt::If(s) => &s.span,
            Stmt::While(s) => &s.span,
            Stmt::DoWhile(s) => &s.span,
            Stmt::For(s) => &s.span,
            Stmt::Return(s) => &s.span,
            Stmt::Switch(s) => &s.span,
            Stmt::Try(s) => &s.span,
            Stmt::Throw(s) => &s.span,
            Stmt::Break(s) | Stmt::Continue(s) => s,
            Stmt::Var(s) => &s.span,
            Stmt::Expr(s) => &s.span,
        }
    }
}
