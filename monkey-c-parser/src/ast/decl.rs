use crate::ast::{
    BlockStmt, Expr, Ident, Parens, Position, Span, Spanned, Symbol, Type, Visibility,
};

/// A top-level AST node.
#[derive(Debug, PartialEq)]
pub enum Ast {
    /// The root of a parsed file. The span covers the entire source.
    Document(Vec<Ast>, Span),
    /// A `(:Name)`, `(:Name(args…))`, or `(:Name1, :Name2(arg), …)` decorator. Each entry is a
    /// symbol identifier with an optional list of argument expressions (`(:typecheck(false))`).
    /// Monkey C supports applying multiple annotations in one parenthesised group.
    ///
    /// <https://developer.garmin.com/connect-iq/monkey-c/annotations/>
    Annotation(Vec<AnnotationEntry>, Span),
    Class(ClassDecl),
    Const(ConstDecl),
    Enum(EnumDecl),
    Function(FunctionDecl),
    Import(ImportDecl),
    Module(ModuleDecl),
    Typedef(TypedefDecl),
    Using(UsingDecl),
    Variable(VarDecl),
    Eof,
}

/// A named, optionally-typed binding used for function parameters.
#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: Spanned<Ident>,
    pub type_: Option<Type>,
    pub as_kw_start: Option<Position>,
    pub visibility: Option<Visibility>,
    pub initializer: Option<Box<Expr>>,
    pub is_static: bool,
    pub span: Span,
}

/// A single `name [as Type] [= init]` binding inside a `var` or `const` declaration. Each binding
/// in a comma-separated list carries its own type annotation and initializer independently.
#[derive(Debug, PartialEq)]
pub struct Binding {
    pub name: Spanned<Ident>,
    pub type_: Option<Type>,
    pub as_kw_start: Option<Position>,
    pub initializer: Option<Box<Expr>>,
    pub assign_kw_start: Option<Position>,
    pub span: Span,
}

/// A `var` declaration, at module/class scope or inside a function body. Holds one or more
/// comma-separated bindings: `var a, b = 2, c as Number;`.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/>
#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub bindings: Vec<Binding>,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    /// `0` when `var` appears in a `for`-init — the loop's first `;` acts as terminator.
    pub semi_pos: Position,
    pub span: Span,
}

/// One entry inside an annotation group: a symbol name plus an optional argument list. `(:foo)`
/// parses with empty `args`; `(:typecheck(false))` gets `args = [false]`.
#[derive(Debug, PartialEq)]
pub struct AnnotationEntry {
    pub name: Symbol,
    pub args: Vec<Expr>,
    pub span: Span,
}

/// An `import Toybox.Lang;` declaration. `import` brings a module suffix and all of its classes
/// into the type namespace; it does not support aliasing.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/#the-import-statement>
#[derive(Debug, PartialEq)]
pub struct ImportDecl {
    /// Fully qualified import path, e.g. `Toybox.WatchUi`.
    pub name: Spanned<Ident>,
    pub span: Span,
}

/// A `using Toybox.Lang;` or `using Toybox.Lang as Lng;` declaration. `using` brings the module
/// suffix into the file's namespace, optionally under an alias.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#using-statements>
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/>
#[derive(Debug, PartialEq)]
pub struct UsingDecl {
    pub name: Spanned<Ident>,
    pub alias: Option<Spanned<Ident>>,
    pub as_kw_start: Option<Position>,
    pub span: Span,
}

/// A `typedef Name as Type;` declaration. The right-hand side is a regular type, so union types via
/// `or` are supported (e.g. `typedef Numeric as Number or Float or Long or Double;`).
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/#named-versus-anonymous-types>
#[derive(Debug, PartialEq)]
pub struct TypedefDecl {
    pub name: Spanned<Ident>,
    pub as_kw_start: Position,
    pub type_: Type,
    pub span: Span,
}

// A `module Name` declaration for declaring a module.
//
// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#modules>
#[derive(Debug, PartialEq)]
pub struct ModuleDecl {
    pub name: Spanned<Ident>,
    pub body: Vec<Ast>,
    pub brace_start: Position,
    pub span: Span,
}

// A `class Name` declaration for declaring a class.
//
// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#defining-classes>
#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Spanned<Ident>,
    pub extends: Option<Spanned<Ident>>,
    pub extends_kw_start: Option<Position>,
    pub body: Vec<Ast>,
    pub brace_start: Position,
    pub span: Span,
}

/// An `enum [Name] { … }` declaration.
///
/// Anonymous enums introduce each variant as a top-level constant.
///
/// Named enums (`enum Dog { SPOT = "Spot", … }`) introduce a type that the variants belong to.
///
/// Values are auto-incremented from the last explicit value (or 0 if none yet) for variants without
/// an explicit `= value`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#enumerations>
#[derive(Debug, PartialEq)]
pub struct EnumDecl {
    pub name: Option<Spanned<Ident>>,
    pub variants: Vec<EnumVariant>,
    pub trailing_comma: bool,
    pub brace_start: Position,
    pub span: Span,
}

/// One entry in an [`EnumDecl`].
#[derive(Debug, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    /// Explicit value (`= <expr>`); `None` means the variant is implicitly `previous + 1`.
    pub value: Option<Expr>,
    pub assign_kw_start: Option<Position>,
    /// Span from the variant's name through its value (or just the name if no `= value`).
    pub span: Span,
}

// A `function name(...)` declaration.
//
// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#functions>
// <https://developer.garmin.com/connect-iq/monkey-c/functions/>
#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Spanned<Ident>,
    pub parameters: Parens<Vec<Parameter>>,
    /// See [`CallExpr::args_trailing_comma`](crate::ast::CallExpr::args_trailing_comma).
    pub parameters_trailing_comma: bool,
    /// Return type from `as ReturnType`.
    pub returns: Option<Type>,
    pub as_kw_start: Option<Position>,
    /// `None` for abstract / interface method declarations (`function f() as T;`).
    pub body: Option<BlockStmt>,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    pub span: Span,
}

/// A `const` declaration at module or class scope.
///
/// Unlike [`VarDecl`], the initializer is mandatory,`const` without a value is a syntax error.
/// `const` also cannot appear inside a function body.
///
/// Note that `const` only prevents the binding from being reassigned; it does not deeply freeze its
/// value (a `const` array still has mutable elements).
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#constants>
#[derive(Debug, PartialEq)]
pub struct ConstDecl {
    pub bindings: Vec<Binding>,
    pub visibility: Option<Visibility>,
    pub is_static: bool,
    pub semi_pos: Position,
    pub span: Span,
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
