use crate::ast::{Expr, Ident, Parens, Position, Span, Type, VarDecl};

/// A statement node.
#[derive(Debug, PartialEq)]
#[allow(clippy::large_enum_variant)]
pub enum Stmt {
    Break(Span),
    Continue(Span),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    DoWhile(DoWhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Switch(SwitchStmt),
    Try(TryStmt),
    Throw(ThrowStmt),
    Var(VarDecl),
    /// A bare expression used as a statement (e.g. an assignment or call).
    Expr(Expr),
}

/// A block is code between an opening `{` and a closing `}`.
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
    pub condition: Expr,
    pub span: Span,
}

/// The init clause of a `for` loop, either a `var` declaration or one or more comma-separated
/// expressions (`for (i = 0, j = 1; ...; ...)`).
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#loops>
#[derive(Debug, PartialEq)]
pub enum ForInit {
    Var(VarDecl),
    Expr(Vec<Expr>),
}

/// The `(init; cond; update)` triple of a `for` loop. `first_semi` and `second_semi` are the byte
/// offsets of the two `;` delimiters, ordered here as they appear in source: `init ; cond ;
/// update`.
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

/// A return statement.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#returning-values-from-functions>
#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub semi_pos: Position,
    pub span: Span,
}

/// A `switch (discriminant) { … }` statement. `cases` preserves source order, including any
/// `default` arm.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#switch-case-statements>
#[derive(Debug, PartialEq)]
pub struct SwitchStmt {
    pub discriminant: Parens<Expr>,
    pub cases: Vec<SwitchCase>,
    pub brace_start: Position,
    pub span: Span,
}

/// A single arm inside a `switch` body.
#[derive(Debug, PartialEq)]
pub struct SwitchCase {
    pub label: CaseLabel,
    /// Statements until the next `case`/`default` or the closing `}`.
    /// Empty when the case immediately falls through to the next.
    pub stmts: Vec<Stmt>,
    /// Span from `case`/`default` through the `:`.
    pub label_span: Span,
    pub span: Span,
}

/// What a `case` (or `default`) arm matches against.
#[derive(Debug, PartialEq)]
pub enum CaseLabel {
    /// `case <expr>:` equality match against the discriminant.
    Value(Expr),
    /// `case instanceof <Type>:` type match.
    InstanceOf(Type),
    /// `default:` fall-through fallback.
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
    /// Type filter from `catch (e instanceof Type)`; `None` means catch-all.
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
    /// Return the source span of this statement.
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
            Stmt::Expr(e) => e.span(),
        }
    }
}
