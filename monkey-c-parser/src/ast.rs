pub type Ident = String;

#[derive(Debug, PartialEq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub ident: Ident,
    pub generic_params: Vec<Type>,
    pub optional: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

/// Used for both function parameters and variable declarations.
#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Ident,
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
    pub initializer: Option<Box<Expr>>,
    pub is_static: bool,
    pub is_hidden: bool,
}

// ---------------------------------------------------------------------------
// Operators
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Mod,        // %
    Eq,         // ==
    NotEq,      // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    InstanceOf, // instanceof
    And,        // &&
    Or,         // ||
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,     // -
    Not,     // !
    BitNot,  // ~
    PreInc,  // ++x
    PreDec,  // --x
    PostInc, // x++
    PostDec, // x--
}

#[derive(Debug, PartialEq)]
pub enum AssignOperator {
    Assign,       // =
    AddAssign,    // +=
    SubAssign,    // -=
    MulAssign,    // *=
    DivAssign,    // /=
    ModAssign,    // %=
    BitAndAssign, // &=
    BitOrAssign,  // |=
    BitXorAssign, // ^=
}

// ---------------------------------------------------------------------------
// Literals
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Long(i64),
    Double(f64),
    String(String),
    Boolean(bool),
    Null,
    NaN,
}

// ---------------------------------------------------------------------------
// Expressions
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Assign(AssignExpr),
    Call(CallExpr),
    Member(MemberExpr),
    Index(IndexExpr),
    New(NewExpr),
    TypeCast(TypeCastExpr),
    Array(ArrayExpr),
    Dict(DictExpr),
    Lit(LitExpr),
    Ident(IdentExpr),
    Me(Span),
    Self_(Span),
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub right: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub operator: AssignOperator,
    pub value: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct MemberExpr {
    pub object: Box<Expr>,
    pub property: Ident,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct NewExpr {
    pub class: Ident,
    pub args: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct TypeCastExpr {
    pub expr: Box<Expr>,
    pub target_type: Type,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ArrayExpr {
    pub elements: Vec<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub pairs: Vec<(Expr, Expr)>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct LitExpr {
    pub value: LiteralValue,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct IdentExpr {
    pub name: Ident,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Statements
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Break(Span),
    Continue(Span),
    Var(VarStmt),
    Comment(String, Span),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: BlockStmt,
    pub else_branch: Option<BlockStmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: BlockStmt,
    pub span: Span,
}

/// The init clause of a `for` loop — either a `var` declaration or an expression.
#[derive(Debug, PartialEq)]
pub enum ForInit {
    Var(VarStmt),
    Expr(Expr),
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub init: Option<ForInit>,
    pub condition: Option<Expr>,
    pub update: Option<Expr>,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct VarStmt {
    pub variable: Variable,
    pub span: Span,
}

// ---------------------------------------------------------------------------
// Top-level declarations
// ---------------------------------------------------------------------------

#[derive(Debug, PartialEq)]
pub enum Ast {
    Document(Vec<Ast>),
    Import(ImportDecl),
    Class(ClassDecl),
    Function(FunctionDecl),
    Variable(VarStmt),
    Comment(String, Span),
    Annotation(String, Span),
    Eof,
}

#[derive(Debug, PartialEq)]
pub struct ImportDecl {
    pub name: Ident,
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Ident,
    pub extends: Option<Ident>,
    pub annotations: Vec<String>,
    pub body: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Ident,
    pub args: Vec<Variable>,
    pub returns: Option<Type>,
    pub annotations: Vec<String>,
    pub body: Vec<Stmt>,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    pub is_hidden: bool,
    pub span: Span,
}
