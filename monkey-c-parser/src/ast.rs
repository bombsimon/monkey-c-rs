/// An identifier — a plain string name.
pub type Ident = String;

/// A symbol name — the string part of a `:symbolName` literal.
pub type Symbol = String;

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
///
/// Multiple types joined by `or` (e.g. `Number or Null`) are represented as
/// a primary type plus one or more `alternatives`. For simple types the
/// alternatives list is empty.
#[derive(Debug, PartialEq)]
pub struct Type {
    /// The primary (or only) type name, e.g. `Array` in `Array<Number>?`.
    pub ident: Ident,
    /// Type parameters, e.g. `[Number]` in `Array<Number>`.
    pub generic_params: Vec<Type>,
    /// Additional alternatives in a union, e.g. `[Null]` in `Number or Null`.
    pub alternatives: Vec<Type>,
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
    /// Comments trailing this parameter in a function decl, between the value
    /// and the next `,` / closing `)`. Empty for non-parameter uses.
    pub trailing_comments: Vec<Stmt>,
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
    Has,        // has
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
    /// A hex-formatted integer literal (`0x…`). Stores the raw digits so the
    /// formatter can preserve the original casing.
    Hex(String),
    Double(f64),
    String(String),
    Boolean(bool),
    /// A symbol literal, e.g. `:mySymbol`.
    Symbol(String),
    Null,
    NaN,
}

/// An expression node.
#[derive(Debug, PartialEq)]
pub enum Expr {
    Binary(BinaryExpr),
    Unary(UnaryExpr),
    Ternary(TernaryExpr),
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
    /// A parenthesised sub-expression. Preserved from source so the formatter
    /// can re-emit user-written grouping verbatim.
    Paren(ParenExpr),
    /// The `me` keyword — reference to the current instance.
    Me(Span),
    /// The `self` keyword — reference to the current class.
    Self_(Span),
    /// The `$` bling symbol — reference to the global namespace.
    Bling(Span),
}

#[derive(Debug, PartialEq)]
pub struct ParenExpr {
    pub inner: Box<Expr>,
    pub span: Span,
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

/// A ternary conditional `cond ? then : else`. Right-associative — chains
/// like `a ? b : c ? d : e` parse as `a ? b : (c ? d : e)`.
#[derive(Debug, PartialEq)]
pub struct TernaryExpr {
    pub cond: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
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
    pub args: Vec<CallArg>,
    /// Comments inside the call parens that aren't attached to any argument
    /// (only populated when `args` is empty).
    pub tail_comments: Vec<Stmt>,
    pub span: Span,
}

/// A single argument inside a call or `new` expression, plus any comments that
/// trail it (between the value and the next `,` / closing `)`).
#[derive(Debug, PartialEq)]
pub struct CallArg {
    pub value: Expr,
    pub trailing_comments: Vec<Stmt>,
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
    pub args: Vec<CallArg>,
    pub tail_comments: Vec<Stmt>,
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
    pub entries: Vec<ArrayEntry>,
    /// Comments inside the brackets that aren't attached to any entry (only
    /// populated when `entries` is empty).
    pub tail_comments: Vec<Stmt>,
    /// Whether the source had a trailing comma — drives the magic trailing comma
    /// formatting rule (trailing comma → always multi-line).
    pub trailing_comma: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ArrayEntry {
    pub value: Expr,
    pub trailing_comments: Vec<Stmt>,
}

#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub entries: Vec<DictEntry>,
    /// Comments inside the braces that aren't attached to any entry (only
    /// populated when `entries` is empty).
    pub tail_comments: Vec<Stmt>,
    /// Whether the source had a trailing comma after the last entry.
    /// See [`ArrayExpr::trailing_comma`].
    pub trailing_comma: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct DictEntry {
    pub key: Expr,
    pub value: Expr,
    pub trailing_comments: Vec<Stmt>,
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
    /// A block comment (`/* … */`). The string contains the raw text between
    /// the delimiters.
    BlockComment(String, Span),
    Break(Span),
    Continue(Span),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
    For(ForStmt),
    Return(ReturnStmt),
    Try(TryStmt),
    Throw(ThrowStmt),
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
    /// Comments that appear immediately after the `}` of `then_branch`. When
    /// `else_branch` is `Some` they sit between the closing `}` and `else`;
    /// otherwise they trail the whole if statement. Only ever line or block
    /// comments.
    pub trailing_comments: Vec<Stmt>,
    pub else_branch: Option<ElseBranch>,
    pub span: Span,
}

/// The branch following an `else` keyword — either a plain block or another
/// `if` statement (i.e. `else if`).
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

/// A `try` statement: `try { … } catch (…) { … }` with optional `finally`.
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
#[derive(Debug, PartialEq)]
pub struct ThrowStmt {
    pub value: Expr,
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
    /// A block comment (`/* … */`). The string contains the raw text between
    /// the delimiters.
    BlockComment(String, Span),
    /// A `(:AnnotationName)` decorator. The name is a symbol identifier.
    Annotation(Symbol, Span),
    /// The root of a parsed file.
    Document(Vec<Ast>),
    Import(ImportDecl),
    Using(UsingDecl),
    Typedef(TypedefDecl),
    Module(ModuleDecl),
    Class(ClassDecl),
    Function(FunctionDecl),
    Variable(VarDecl),
    Const(ConstDecl),
    Eof,
}

/// An `import Toybox.Lang;` declaration. `import` brings a module suffix and
/// all of its classes into the type namespace; it does not support aliasing.
#[derive(Debug, PartialEq)]
pub struct ImportDecl {
    /// Fully qualified import path, e.g. `Toybox.WatchUi`.
    pub name: Ident,
    pub span: Span,
}

/// A `using Toybox.Lang;` or `using Toybox.Lang as Lng;` declaration.
/// `using` brings the module suffix into the file's namespace, optionally
/// under an alias.
#[derive(Debug, PartialEq)]
pub struct UsingDecl {
    pub name: Ident,
    pub alias: Option<Ident>,
    pub span: Span,
}

/// A `typedef Name as Type;` declaration. The right-hand side is a regular
/// type, so union types via `or` are supported (e.g.
/// `typedef Numeric as Number or Float or Long or Double;`).
#[derive(Debug, PartialEq)]
pub struct TypedefDecl {
    pub name: Ident,
    pub type_: Type,
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
    /// Comments inside the parameter list parens that aren't attached to any
    /// parameter (only populated when `args` is empty).
    pub args_tail_comments: Vec<Stmt>,
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
            Expr::Ternary(e) => &e.span,
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
            Expr::Paren(e) => &e.span,
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
            Stmt::Try(s) => &s.span,
            Stmt::Throw(s) => &s.span,
            Stmt::Break(s) | Stmt::Continue(s) => s,
            Stmt::Var(s) => &s.span,
            Stmt::Comment(_, s) | Stmt::BlockComment(_, s) => s,
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
            Ast::Using(d) => Some(&d.span),
            Ast::Typedef(d) => Some(&d.span),
            Ast::Module(d) => Some(&d.span),
            Ast::Class(d) => Some(&d.span),
            Ast::Function(d) => Some(&d.span),
            Ast::Variable(v) => Some(&v.span),
            Ast::Const(c) => Some(&c.span),
            Ast::Comment(_, s) | Ast::BlockComment(_, s) | Ast::Annotation(_, s) => Some(s),
        }
    }
}
