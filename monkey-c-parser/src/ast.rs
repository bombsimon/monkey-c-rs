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

/// A Monkey C type annotation, optionally nullable and optionally part of
/// an `or`-union with other peers.
///
/// `,` inside `<…>` and the `or` keyword are *not* interchangeable:
/// `Dictionary<String, Number>` has two distinct arguments;
/// `Array<Number or Null>` has one argument that is itself a union. Each
/// generic argument may carry its own union.
#[derive(Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    /// `or`-joined union peers, excluding the first (which is this type
    /// itself). Empty for non-union types.
    pub alternatives: Vec<Type>,
    /// Whether the type is nullable (`?` suffix).
    pub optional: bool,
}

/// What kind of type this is, before alternatives/optional are applied.
#[derive(Debug, PartialEq)]
pub enum TypeKind {
    /// A named type, possibly generic: `Number`, `Array<T>`,
    /// `Dictionary<K, V>`.
    Named {
        ident: Ident,
        /// Comma-separated arguments inside `<…>`. Empty for non-generic
        /// types.
        generic_params: Vec<Type>,
    },
    /// An inline dictionary type: `{ :key1 as T, "key2" as U }`. Used as a
    /// type annotation, e.g. `function f(opts as { :flag as Boolean })`.
    ///
    /// When `trailing_comma` is `true`, the formatter always renders
    /// multi-line; otherwise it tries to fit on one line.
    Dict {
        entries: Vec<DictTypeEntry>,
        trailing_comma: bool,
    },
    /// An inline interface type: `interface { function foo() as X; var bar as Y; … }`.
    /// Members are function signatures and/or `var` declarations — see
    /// [`InterfaceMember`]. Used as a `typedef` right-hand side and as an
    /// inline type annotation on function parameters.
    ///
    /// `body_span` covers `{` through `}` so comment attachment can pick
    /// this as the smallest containing scope for comments inside the body.
    Interface {
        members: Vec<InterfaceMember>,
        body_span: Span,
    },
}

/// One member inside an `interface { … }` type — either a function
/// signature or a typed variable declaration.
#[derive(Debug, PartialEq)]
pub enum InterfaceMember {
    Function(InterfaceMethod),
    Variable(InterfaceVar),
}

/// One method signature inside an `interface { … }` type. Mirrors
/// [`FunctionDecl`] minus the body.
#[derive(Debug, PartialEq)]
pub struct InterfaceMethod {
    pub name: Ident,
    pub args: Vec<Variable>,
    pub returns: Option<Type>,
    pub span: Span,
}

/// One variable declaration inside an `interface { … }` type. The type
/// annotation is required since the variable has no initializer.
#[derive(Debug, PartialEq)]
pub struct InterfaceVar {
    pub name: Ident,
    pub type_: Type,
    pub span: Span,
}

/// A single `key as Type` entry in an inline dictionary type.
#[derive(Debug, PartialEq)]
pub struct DictTypeEntry {
    pub key: DictTypeKey,
    pub value_type: Type,
}

/// A key in an inline dictionary type. Either a symbol literal (`:name`)
/// or a string literal (`"name"`).
#[derive(Debug, PartialEq)]
pub enum DictTypeKey {
    Symbol(String),
    String(String),
}

impl Type {
    /// The name of a [`TypeKind::Named`] type, or `None` for an inline
    /// dictionary type.
    pub fn ident(&self) -> Option<&str> {
        match &self.kind {
            TypeKind::Named { ident, .. } => Some(ident),
            TypeKind::Dict { .. } | TypeKind::Interface { .. } => None,
        }
    }

    /// Comma-separated generic arguments. Empty for non-generic types and
    /// for inline dictionary types.
    pub fn generic_params(&self) -> &[Type] {
        match &self.kind {
            TypeKind::Named { generic_params, .. } => generic_params,
            TypeKind::Dict { .. } | TypeKind::Interface { .. } => &[],
        }
    }
}

/// A half-open byte range `[start, end)` into the original source string.
///
/// All offsets are global — measured from byte 0 of the file.
/// Use [`LineIndex`](crate::line_index::LineIndex) to convert to line/column.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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

/// A parenthesised wrapper around an inner AST piece. Tracks the source
/// positions of the opening `(` and closing `)`. Used wherever the grammar
/// requires parens — `if (cond)`, `function f(args)`, `for (init; cond; update)`,
/// `switch (disc)` — so the comment-attachment pass can recognise the
/// "between `)` and `{`" slot without each parent carrying a `header_end`
/// field.
#[derive(Debug, PartialEq)]
pub struct Parens<T> {
    /// Byte offset of the opening `(`.
    pub open: usize,
    pub inner: T,
    /// Byte offset just past the closing `)`.
    pub close: usize,
}

impl<T> std::ops::Deref for Parens<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
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
    /// Source span of this parameter, used to attach comments via the
    /// [`crate::comments::CommentsMap`].
    pub span: Span,
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
    AndKeyword, // and
    Or,         // ||
    OrKeyword,  // or
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    LeftShift,  // <<
    RightShift, // >>
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
    Assign,           // =
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    ModAssign,        // %=
    BitAndAssign,     // &=
    BitOrAssign,      // |=
    BitXorAssign,     // ^=
    LeftShiftAssign,  // <<=
    RightShiftAssign, // >>=
}

/// A 32-bit floating point literal together with the source-form flags
/// needed to round-trip it. The lexer parses `value` from the written digits;
/// flags capture which surface form the user wrote so the formatter can
/// re-emit the same shape (`0f`, `0.5`, `0.5f`, `.978`, `.5f`).
#[derive(Debug, Clone, PartialEq)]
pub struct FloatLit {
    pub value: f32,
    /// Source contained a `.` (`0.5`, `.978`). False for integer-form
    /// literals that gain Float-ness only through the `f` suffix (`0f`).
    pub has_dot: bool,
    /// Source omitted the leading zero (`.5`, `.5f`). Implies `has_dot`.
    pub leading_dot: bool,
    /// Source had an explicit `f` suffix.
    pub has_suffix: bool,
}

/// A 64-bit floating point literal. Unlike [`FloatLit`], the `d` suffix is
/// always required in source, so there is no `has_suffix` flag.
#[derive(Debug, Clone, PartialEq)]
pub struct DoubleLit {
    pub value: f64,
    /// Source contained a `.` (`78.0d`). False for integer-form Double
    /// literals (`78d`).
    pub has_dot: bool,
    /// Source omitted the leading zero (`.5d`). Implies `has_dot`.
    pub leading_dot: bool,
}

impl std::fmt::Display for FloatLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

impl std::fmt::Display for DoubleLit {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

/// A compile-time constant value.
#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    /// 32-bit signed integer (no suffix in source).
    Number(i32),
    /// 64-bit signed integer (`l` suffix in source).
    Long(i64),
    /// A hex-formatted integer literal (`0x…`). Stores the raw digits so the
    /// formatter can preserve the original casing.
    Hex(String),
    /// A hex-formatted 64-bit integer literal (`0x…l`). Stores the raw digits
    /// so the formatter can preserve the original casing.
    HexLong(String),
    /// 32-bit floating point number. Source-form flags let the formatter
    /// round-trip the exact written form (`0f`, `0.5`, `0.5f`, `.978`, `.5f`).
    Float(FloatLit),
    /// 64-bit floating point number. Source-form flags let the formatter
    /// round-trip the exact written form (`78d`, `78.0d`, `.5d`).
    Double(DoubleLit),
    String(String),
    /// A character literal: `'a'`. Stores the decoded character(s) between
    /// the single quotes.
    Char(String),
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
    NewArray(NewArrayExpr),
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
    pub span: Span,
}

/// A single argument inside a call or `new` expression.
#[derive(Debug, PartialEq)]
pub struct CallArg {
    pub value: Expr,
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
    pub span: Span,
}

/// A `new [size]` or `new Array<Number>[size]` allocation. `element_type`
/// is `None` for the bare form (typeless array of the given size).
#[derive(Debug, PartialEq)]
pub struct NewArrayExpr {
    pub element_type: Option<Type>,
    pub size: Box<Expr>,
    /// A `]b` suffix marks the allocation as a `Lang.ByteArray` (`new [N]b`).
    pub is_byte_array: bool,
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
    /// Whether the source had a trailing comma — drives the magic trailing
    /// comma formatting rule (trailing comma → always multi-line, blank lines
    /// preserved).
    pub trailing_comma: bool,
    /// A `]b` suffix marks the literal as a `Lang.ByteArray`. The parser
    /// recognises any `b` identifier immediately following the closing `]`.
    pub is_byte_array: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ArrayEntry {
    pub value: Expr,
}

#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub entries: Vec<DictEntry>,
    /// See [`ArrayExpr::trailing_comma`].
    pub trailing_comma: bool,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct DictEntry {
    pub key: Expr,
    pub value: Expr,
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

/// A source comment, either a `// …` line comment or a `/* … */` block
/// comment. `is_block` is the distinction; the stored `text` is the raw
/// content between the comment delimiters.
#[derive(Debug, PartialEq, Clone)]
pub struct CommentStmt {
    pub text: String,
    pub is_block: bool,
    pub span: Span,
}

/// All comments in a parsed file, in source order. Comments are not woven
/// into the AST itself; consumers attach them to nodes via a separate pass.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct CommentTable {
    pub comments: Vec<CommentStmt>,
}

impl CommentTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, comment: CommentStmt) {
        self.comments.push(comment);
    }
}

/// Output of [`crate::parser::Parser::parse`]. Carries both the AST and the
/// source-order comment table.
#[derive(Debug)]
pub struct ParseOutput {
    pub ast: Ast,
    pub comments: CommentTable,
}

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

#[derive(Debug, PartialEq)]
pub struct BlockStmt {
    pub stmts: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct IfStmt {
    pub condition: Parens<Expr>,
    pub then_branch: BlockStmt,
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
    pub condition: Parens<Expr>,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct DoWhileStmt {
    pub body: BlockStmt,
    pub condition: Expr,
    /// Byte offset just past the `do` keyword. Used by the comment-attachment
    /// pass to recognise comments in the `do /* C */ {` slot.
    pub header_end: usize,
    pub span: Span,
}

/// The init clause of a `for` loop — either a `var` declaration or an expression.
#[derive(Debug, PartialEq)]
pub enum ForInit {
    Var(VarDecl),
    Expr(Expr),
}

/// The `(init; cond; update)` triple of a `for` loop.
#[derive(Debug, PartialEq)]
pub struct ForHeader {
    pub init: Option<ForInit>,
    pub condition: Option<Expr>,
    pub update: Option<Expr>,
}

#[derive(Debug, PartialEq)]
pub struct ForStmt {
    pub header: Parens<ForHeader>,
    pub body: BlockStmt,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ReturnStmt {
    pub value: Option<Expr>,
    pub span: Span,
}

/// A `switch (discriminant) { … }` statement. `cases` preserves source order,
/// including any `default` arm.
#[derive(Debug, PartialEq)]
pub struct SwitchStmt {
    pub discriminant: Parens<Expr>,
    pub cases: Vec<SwitchCase>,
    /// Byte offset of the body's opening `{`.
    pub brace_start: usize,
    pub span: Span,
}

/// A single arm inside a `switch` body.
#[derive(Debug, PartialEq)]
pub struct SwitchCase {
    pub label: CaseLabel,
    /// Statements until the next `case`/`default` or the closing `}`.
    /// Empty when the case immediately falls through to the next.
    pub stmts: Vec<Stmt>,
    /// Span from `case`/`default` through the `:`. Used by the comment
    /// attachment pass as the anchor for trailing comments that sit on the
    /// same source line as `case X:` so they don't accidentally attach to
    /// the inner `X` and render before the colon.
    pub label_span: Span,
    pub span: Span,
}

/// What a `case` (or `default`) arm matches against.
#[derive(Debug, PartialEq)]
pub enum CaseLabel {
    /// `case <expr>:` — equality match against the discriminant.
    Value(Expr),
    /// `case instanceof <Type>:` — type match.
    InstanceOf(Type),
    /// `default:` — fall-through fallback.
    Default,
}

/// A `try` statement: `try { … } catch (…) { … }` with optional `finally`.
#[derive(Debug, PartialEq)]
pub struct TryStmt {
    pub body: BlockStmt,
    pub catches: Vec<CatchClause>,
    pub finally: Option<BlockStmt>,
    /// Byte offset just past the `try` keyword. Used by the comment-attachment
    /// pass to recognise comments in the `try /* C */ {` slot.
    pub header_end: usize,
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

/// A single `name [as Type] [= init]` binding inside a `var` or `const`
/// declaration. Each binding in a comma-separated list carries its own type
/// annotation and initializer independently.
#[derive(Debug, PartialEq)]
pub struct Binding {
    pub name: Ident,
    pub type_: Option<Type>,
    pub initializer: Option<Box<Expr>>,
    pub span: Span,
}

/// A `var` declaration, at module/class scope or inside a function body.
/// Holds one or more comma-separated bindings: `var a, b = 2, c as Number;`.
#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub bindings: Vec<Binding>,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    pub span: Span,
}

/// One entry inside an annotation group: a symbol name plus an optional
/// argument list. `(:foo)` parses with empty `args`; `(:typecheck(false))`
/// gets `args = [false]`.
#[derive(Debug, PartialEq)]
pub struct AnnotationEntry {
    pub name: Symbol,
    pub args: Vec<Expr>,
}

/// A top-level AST node. Also used for class and module body members.
#[derive(Debug, PartialEq)]
pub enum Ast {
    /// A `(:Name)`, `(:Name(args…))`, or `(:Name1, :Name2(arg), …)` decorator.
    /// Each entry is a symbol identifier with an optional list of argument
    /// expressions (`(:typecheck(false))`). Monkey C supports applying multiple
    /// annotations in one parenthesised group.
    Annotation(Vec<AnnotationEntry>, Span),
    /// The root of a parsed file. The span covers the entire source so that
    /// top-level standalone comments can attach as `Dangling(Inside)` of the
    /// document.
    Document(Vec<Ast>, Span),
    Import(ImportDecl),
    Using(UsingDecl),
    Typedef(TypedefDecl),
    Module(ModuleDecl),
    Class(ClassDecl),
    Function(FunctionDecl),
    Enum(EnumDecl),
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
    /// Byte offset of the body's opening `{`.
    pub brace_start: usize,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Ident,
    /// Base class name from `extends BaseClass`.
    pub extends: Option<Ident>,
    pub body: Vec<Ast>,
    /// Byte offset of the body's opening `{`.
    pub brace_start: usize,
    pub span: Span,
}

/// An `enum [Name] { … }` declaration. The name is optional — anonymous
/// enums simply introduce each variant as a top-level constant; named enums
/// (`enum Dog { SPOT = "Spot", … }`) introduce a type that the variants
/// belong to. Values are auto-incremented from the last explicit value
/// (or 0 if none yet) for variants without an explicit `= value`.
#[derive(Debug, PartialEq)]
pub struct EnumDecl {
    /// `Some` when the source had `enum Name {`; `None` for `enum {`.
    pub name: Option<Ident>,
    pub variants: Vec<EnumVariant>,
    pub trailing_comma: bool,
    /// Byte offset of the body's opening `{`.
    pub brace_start: usize,
    pub span: Span,
}

/// One entry in an [`EnumDecl`].
#[derive(Debug, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    /// Explicit value (`= <expr>`); `None` means the variant is implicitly
    /// `previous + 1`.
    pub value: Option<Expr>,
    /// Span from the variant's name through its value (or just the name if
    /// no `= value`). Used by the [`crate::comments`] attachment pass to
    /// place leading/trailing comments.
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Ident,
    pub args: Parens<Vec<Variable>>,
    /// Return type from `as ReturnType`.
    pub returns: Option<Type>,
    pub body: BlockStmt,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    /// Byte offset where the "between header and `{`" region ends. With a
    /// return type this is the end of `as ReturnType`; without, it equals
    /// `args.close`. Used by the comment-attachment pass to recognise
    /// `function f() as X /* C */ {` style header comments.
    pub header_end: usize,
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
    pub bindings: Vec<Binding>,
    pub visibility: Option<Visibility>,
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
            Expr::NewArray(e) => &e.span,
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

impl Ast {
    /// Return the source span of this node, if it has one.
    ///
    /// `Eof` has no meaningful span.
    pub fn span(&self) -> Option<&Span> {
        match self {
            Ast::Eof => None,
            Ast::Import(d) => Some(&d.span),
            Ast::Using(d) => Some(&d.span),
            Ast::Typedef(d) => Some(&d.span),
            Ast::Module(d) => Some(&d.span),
            Ast::Class(d) => Some(&d.span),
            Ast::Function(d) => Some(&d.span),
            Ast::Enum(d) => Some(&d.span),
            Ast::Variable(v) => Some(&v.span),
            Ast::Const(c) => Some(&c.span),
            Ast::Document(_, s) | Ast::Annotation(_, s) => Some(s),
        }
    }
}
