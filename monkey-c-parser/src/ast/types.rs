use crate::ast::{Ident, Parameter, Parens, Position, Span, Spanned};

/// A Monkey C type annotation, optionally nullable and optionally part of
/// an `or`-union with other peers.
///
/// `,` inside `<…>` and the `or` keyword are *not* interchangeable:
/// `Dictionary<String, Number>` has two distinct arguments;
/// `Array<Number or Null>` has one argument that is itself a union. Each
/// generic argument may carry its own union.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/>
#[derive(Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    /// `or`-joined union peers, excluding the first (which is this type
    /// itself). Empty for non-union types.
    pub alternatives: Vec<Type>,
    /// Whether the type is nullable (`?` suffix).
    pub optional: bool,
    pub span: Span,
}

/// What kind of type this is, before alternatives/optional are applied.
#[derive(Debug, PartialEq)]
pub enum TypeKind {
    /// A named type, possibly generic: `Number`, `Array<T>`, `Dictionary<K, V>`.
    Named {
        ident: Ident,
        /// Comma-separated arguments inside `<…>`. Empty for non-generic types.
        generic_params: Vec<Type>,
    },
    /// An inline dictionary type: `{ :key1 as T, "key2" as U }`. Used as a type annotation, e.g.
    /// `function f(opts as { :flag as Boolean })`.
    Dict {
        entries: Vec<DictTypeEntry>,
        trailing_comma: bool,
    },
    /// An inline interface type: `interface { function foo() as X; var bar as Y; … }`. Members are
    /// function signatures and/or `var` declarations — see [`InterfaceMember`]. Used as a `typedef`
    /// right-hand side and as an inline type annotation on function parameters.
    ///
    /// `body_span` covers `{` through `}` so comment attachment can pick this as the smallest
    /// containing scope for comments inside the body.
    Interface {
        members: Vec<InterfaceMember>,
        body_span: Span,
    },
    /// A tuple type: `[T1, T2, …]`. Models an array with a positionally-typed shape, `[Number,
    /// String, Boolean]` is a 3-element array whose entries must match the listed types. Used in
    /// `as`-annotations: `function f() as [StartView, StartDelegate]`.
    Tuple { elements: Vec<Type> },
    /// A callable / method-reference type: `Method(arg as T, …) as Return`. `name` is typically
    /// `Method` but the parser accepts any identifier in this position so unfamiliar SDK callable
    /// types still parse. The return is optional,`Method(x as Number)` (no `as`) is allowed.
    Method {
        name: Ident,
        args: Vec<Parameter>,
        returns: Option<Box<Type>>,
    },
    /// A parenthesised type: `(T)`. Preserved from source so the formatter can re-emit user-written
    /// grouping, which matters when the `?` or `or` suffix would otherwise bind to a sub-part,
    /// `(Method(x) as Void)?` is the whole callable being nullable, not the return type.
    ///
    /// The [`Parens`] wrapper carries the open/close byte offsets so the linter can replace just
    /// the parens (not the inner text) when the grouping is redundant.
    Group(Parens<Box<Type>>),
}

/// One member inside an `interface { … }` type — either a function signature or a typed variable
/// declaration.
#[derive(Debug, PartialEq)]
pub enum InterfaceMember {
    Function(InterfaceMethod),
    Variable(InterfaceVar),
}

/// One method signature inside an `interface { … }` type. Mirrors
/// [`FunctionDecl`](crate::ast::FunctionDecl) minus the body.
#[derive(Debug, PartialEq)]
pub struct InterfaceMethod {
    pub name: Spanned<Ident>,
    pub args: Vec<Parameter>,
    pub returns: Option<Type>,
    pub as_kw_start: Option<Position>,
    pub span: Span,
}

/// One variable declaration inside an `interface { … }` type. The type annotation is required since
/// the variable has no initializer.
#[derive(Debug, PartialEq)]
pub struct InterfaceVar {
    pub name: Spanned<Ident>,
    pub as_kw_start: Position,
    pub type_: Type,
    pub span: Span,
}

/// A single `key as Type` entry in an inline dictionary type.
#[derive(Debug, PartialEq)]
pub struct DictTypeEntry {
    pub key: DictTypeKey,
    pub value_type: Type,
    pub span: Span,
}

/// A key in an inline dictionary type. Either a symbol literal (`:name`)
/// or a string literal (`"name"`).
#[derive(Debug, PartialEq)]
pub enum DictTypeKey {
    Symbol(String),
    String(String),
}

impl Type {
    /// The name of a [`TypeKind::Named`] type, or `None` for an inline dictionary type. Unwraps
    /// [`TypeKind::Group`] transparently.
    pub fn ident(&self) -> Option<&str> {
        match &self.kind {
            TypeKind::Named { ident, .. } => Some(ident),
            TypeKind::Method { name, .. } => Some(name),
            TypeKind::Group(group) => group.inner.ident(),
            TypeKind::Dict { .. } | TypeKind::Interface { .. } | TypeKind::Tuple { .. } => None,
        }
    }

    /// Comma-separated generic arguments. Empty for non-generic types and for inline dictionary
    /// types. Unwraps [`TypeKind::Group`] transparently.
    pub fn generic_params(&self) -> &[Type] {
        match &self.kind {
            TypeKind::Named { generic_params, .. } => generic_params,
            TypeKind::Group(group) => group.inner.generic_params(),
            TypeKind::Dict { .. }
            | TypeKind::Interface { .. }
            | TypeKind::Tuple { .. }
            | TypeKind::Method { .. } => &[],
        }
    }
}
