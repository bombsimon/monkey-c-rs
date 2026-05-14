/// An identifier — a plain string name.
pub type Ident = String;

/// Visibility modifier on a declaration.
///
/// `Hidden` is synonymous with `Protected` — both restrict access to the
/// declaring class and its subclasses. The distinction is preserved so the
/// formatter can round-trip the original keyword.
#[derive(Debug, PartialEq)]
pub enum Visibility {
    Private,
    Protected,
    /// The `hidden` keyword — an alias for `protected`.
    Hidden,
    Public,
}

/// A Monkey C type annotation, optionally generic and optionally nullable.
#[derive(Debug, PartialEq)]
pub struct Type {
    /// The base type name, e.g. `Array` in `Array<Number>?`.
    pub ident: Ident,
    /// Type parameters, e.g. `[Number]` in `Array<Number>`.
    pub generic_params: Vec<Type>,
    /// Whether the type is nullable (`?` suffix).
    pub optional: bool,
}

/// A half-open byte range `[start, end)` into the original source string.
///
/// All offsets are global — measured from byte 0 of the file.
/// Use [`LineIndex`](crate::line_index::LineIndex) to convert to line/column.
#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    /// Byte offset of the first character of this node.
    pub start: usize,
    /// Byte offset one past the last character of this node.
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

/// A named, optionally-typed binding used for function parameters.
#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Ident,
    /// Declared type (`as Type`), if present.
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
    /// Default value (`= expr`), if present.
    pub initializer: Option<Box<Expr>>,
    pub is_static: bool,
}

/// A binary (two-operand) operator.
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

/// A unary (single-operand) operator.
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

/// A compound assignment operator.
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

/// A compile-time constant value.
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Long(i64),
    Double(f64),
    String(String),
    Boolean(bool),
    Null,
    NaN,
}

/// An expression node.
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
    /// The `me` keyword — reference to the current instance.
    Me(Span),
    /// The `self` keyword — reference to the current class.
    Self_(Span),
    /// The `$` bling symbol — reference to the global namespace.
    Bling(Span),
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
    /// The property name after the `.`.
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
    /// Fully qualified class name, e.g. `MyModule.Foo`.
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
    /// Whether the source had a trailing comma — drives the magic trailing comma
    /// formatting rule (trailing comma → always multi-line).
    pub trailing_comma: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub pairs: Vec<(Expr, Expr)>,
    /// Whether the source had a trailing comma after the last pair.
    /// See [`ArrayExpr::trailing_comma`].
    pub trailing_comma: bool,
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

/// A statement node.
#[derive(Debug, PartialEq)]
pub enum Stmt {
    /// A line comment (`// …`). The string contains the raw text after `//`.
    Comment(String, Span),
    Break(Span),
    Continue(Span),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Var(VarDecl),
    /// A bare expression used as a statement (e.g. an assignment or call).
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
    Var(VarDecl),
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

/// A `var` declaration, at module/class scope or inside a function body.
#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub name: Ident,
    /// Declared type (`as Type`), if present.
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
    /// Initial value (`= expr`), if present.
    pub initializer: Option<Box<Expr>>,
    pub is_static: bool,
    pub span: Span,
}

/// A top-level AST node. Also used for class and module body members.
#[derive(Debug, PartialEq)]
pub enum Ast {
    /// A line comment (`// …`). The string contains the raw text after `//`.
    Comment(String, Span),
    /// A `(:AnnotationName)` decorator.
    Annotation(String, Span),
    /// The root of a parsed file.
    Document(Vec<Ast>),
    Import(ImportDecl),
    Module(ModuleDecl),
    Class(ClassDecl),
    Function(FunctionDecl),
    Variable(VarDecl),
    Const(ConstDecl),
    Eof,
}

#[derive(Debug, PartialEq)]
pub struct ImportDecl {
    /// Fully qualified import path, e.g. `Toybox.WatchUi`.
    pub name: Ident,
    /// Optional `as Alias`.
    pub alias: Option<Ident>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ModuleDecl {
    pub name: Ident,
    pub body: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Ident,
    /// Base class name from `extends BaseClass`.
    pub extends: Option<Ident>,
    pub body: Vec<Ast>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Ident,
    pub args: Vec<Variable>,
    /// Return type from `as ReturnType`.
    pub returns: Option<Type>,
    pub body: BlockStmt,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    pub span: Span,
}

/// A `const` declaration at module or class scope.
///
/// Unlike [`VarDecl`], the initializer is mandatory — `const` without a value
/// is a syntax error. `const` also cannot appear inside a function body.
///
/// Note that `const` only prevents the binding from being reassigned; it does
/// not deeply freeze its value (a `const` array still has mutable elements).
#[derive(Debug, PartialEq)]
pub struct ConstDecl {
    pub name: Ident,
    /// Declared type (`as Type`), if present.
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
    /// The required initial value.
    pub initializer: Expr,
    pub is_static: bool,
    pub span: Span,
}

impl Expr {
    /// Return the source span of this expression.
    pub fn span(&self) -> &Span {
        match self {
            Expr::Binary(e) => &e.span,
            Expr::Unary(e) => &e.span,
            Expr::Assign(e) => &e.span,
            Expr::Call(e) => &e.span,
            Expr::Member(e) => &e.span,
            Expr::Index(e) => &e.span,
            Expr::New(e) => &e.span,
            Expr::TypeCast(e) => &e.span,
            Expr::Array(e) => &e.span,
            Expr::Dict(e) => &e.span,
            Expr::Lit(e) => &e.span,
            Expr::Ident(e) => &e.span,
            Expr::Me(s) | Expr::Self_(s) | Expr::Bling(s) => s,
        }
    }
}

impl Stmt {
    /// Return the source span of this statement.
    pub fn span(&self) -> &Span {
        match self {
            Stmt::Block(s) => &s.span,
            Stmt::If(s) => &s.span,
            Stmt::While(s) => &s.span,
            Stmt::For(s) => &s.span,
            Stmt::Return(s) => &s.span,
            Stmt::Break(s) | Stmt::Continue(s) => s,
            Stmt::Var(s) => &s.span,
            Stmt::Comment(_, s) => s,
            Stmt::Expr(e) => e.span(),
        }
    }
}

impl Ast {
    /// Return the source span of this node, if it has one.
    ///
    /// `Document` and `Eof` have no meaningful span.
    pub fn span(&self) -> Option<&Span> {
        match self {
            Ast::Document(_) | Ast::Eof => None,
            Ast::Import(d) => Some(&d.span),
            Ast::Module(d) => Some(&d.span),
            Ast::Class(d) => Some(&d.span),
            Ast::Function(d) => Some(&d.span),
            Ast::Variable(v) => Some(&v.span),
            Ast::Const(c) => Some(&c.span),
            Ast::Comment(_, s) | Ast::Annotation(_, s) => Some(s),
        }
    }
}
