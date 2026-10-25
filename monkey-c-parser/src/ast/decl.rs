use crate::ast::{
    BlockStmt, Expr, Ident, Modifiers, Parens, Position, Separated, Span, Spanned, Symbol, Type,
};

/// A declaration, or the whole file as [`Ast::Document`]. Classes and modules hold more of these
/// in their bodies, while function bodies hold [`Stmt`](crate::ast::Stmt)s.
#[derive(Debug, PartialEq)]
pub enum Ast {
    /// The root of a parsed file, with a span covering the whole source.
    Document(Vec<Ast>, Span),
    /// An annotation group, e.g. `(:test)` or `(:a, :typecheck(false))`. Several annotations can
    /// share one group, and the group is a sibling of the declaration it applies to rather than
    /// part of it.
    ///
    /// <https://developer.garmin.com/connect-iq/monkey-c/annotations/>
    Annotation(Vec<AnnotationEntry>, Span),
    /// A class declaration, e.g. `class Name extends Parent { … }`.
    Class(ClassDecl),
    /// A constant declaration, e.g. `const NAME = value;`.
    Const(ConstDecl),
    /// An enum declaration, e.g. `enum Name { A, B = 2 }`.
    Enum(EnumDecl),
    /// A function declaration, e.g. `function name(args) as Type { … }`.
    Function(FunctionDecl),
    /// An import of a module's types, e.g. `import Toybox.Lang;`.
    Import(ImportDecl),
    /// A module declaration, e.g. `module Name { … }`.
    Module(ModuleDecl),
    /// A named type, e.g. `typedef Name as Type;`.
    Typedef(TypedefDecl),
    /// Bringing a module into scope, e.g. `using Toybox.Lang as Lang;`.
    Using(UsingDecl),
    /// A variable declaration, e.g. `var name = value;`.
    Variable(VarDecl),
    /// The end of the input, only seen while parsing and never inside a document.
    Eof,
}

/// A function parameter, optionally typed, e.g. `name` or `name as Type`.
#[derive(Debug, PartialEq)]
pub struct Parameter {
    pub name: Spanned<Ident>,
    pub type_: Option<Type>,
    pub as_kw_start: Option<Position>,
    pub initializer: Option<Box<Expr>>,
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

/// A `var` declaration with one or more bindings, as in `var a, b = 2, c as Number;`, anywhere
/// from module scope to a `for` header.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/>
#[derive(Debug, PartialEq)]
pub struct VarDecl {
    pub bindings: Separated<Binding>,
    pub modifiers: Modifiers,
    pub semi_pos: Position,
    pub span: Span,
}

/// One entry inside an annotation group: a symbol name and optional arguments, e.g. `:foo` or
/// `:typecheck(false)`. Entries may be separated by commas or only by whitespace, and the
/// formatter keeps whichever was written.
#[derive(Debug, PartialEq)]
pub struct AnnotationEntry {
    pub name: Symbol,
    pub args: Vec<Expr>,
    pub preceding_comma_pos: Option<Position>,
    pub span: Span,
}

/// An `import Toybox.Lang;` declaration. `import` brings a module suffix and all of its classes
/// into the type namespace; it does not support aliasing.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/#the-import-statement>
#[derive(Debug, PartialEq)]
pub struct ImportDecl {
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

/// A `module Name { … }` declaration.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#modules>
#[derive(Debug, PartialEq)]
pub struct ModuleDecl {
    pub name: Spanned<Ident>,
    pub body: Vec<Ast>,
    pub brace_start: Position,
    pub modifiers: Modifiers,
    pub span: Span,
}

/// A `class Name extends Parent { … }` declaration. The parent can be dotted, e.g. `WatchUi.View`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#defining-classes>
#[derive(Debug, PartialEq)]
pub struct ClassDecl {
    pub name: Spanned<Ident>,
    pub extends: Option<Spanned<Ident>>,
    pub extends_kw_start: Option<Position>,
    pub body: Vec<Ast>,
    pub brace_start: Position,
    pub modifiers: Modifiers,
    pub span: Span,
}

/// An `enum [Name] { … }` declaration. An anonymous enum introduces each variant as a constant,
/// while a named one introduces a type the variants belong to. A variant without a value is one
/// more than the one before it, starting from 0.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#enumerations>
#[derive(Debug, PartialEq)]
pub struct EnumDecl {
    pub name: Option<Spanned<Ident>>,
    pub variants: Separated<EnumVariant>,
    pub brace_start: Position,
    pub modifiers: Modifiers,
    pub span: Span,
}

/// One entry in an [`EnumDecl`].
/// One variant of an [`EnumDecl`], e.g. `NAME` or `NAME = value`.
#[derive(Debug, PartialEq)]
pub struct EnumVariant {
    pub name: Ident,
    pub value: Option<Expr>,
    pub assign_kw_start: Option<Position>,
    pub span: Span,
}

/// A `function name(args) as Type { … }` declaration. `body` is `None` for a declaration without
/// one, e.g. `function name() as Type;` in an abstract class.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#functions>
/// <https://developer.garmin.com/connect-iq/monkey-c/functions/>
#[derive(Debug, PartialEq)]
pub struct FunctionDecl {
    pub name: Spanned<Ident>,
    pub parameters: Parens<Separated<Parameter>>,
    pub returns: Option<Type>,
    pub as_kw_start: Option<Position>,
    /// `None` for abstract / interface method declarations (`function f() as T;`).
    pub body: Option<BlockStmt>,
    pub modifiers: Modifiers,
    pub span: Span,
}

/// A `const` declaration at module or class scope. Unlike [`VarDecl`] every binding needs a value,
/// and `const` can't appear inside a function body. It only prevents reassigning the binding, so
/// a `const` array still has mutable elements.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#constants>
#[derive(Debug, PartialEq)]
pub struct ConstDecl {
    pub bindings: Separated<Binding>,
    pub modifiers: Modifiers,
    pub semi_pos: Position,
    pub span: Span,
}

impl Ast {
    /// The source range of the node, or `None` for [`Ast::Eof`].
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
