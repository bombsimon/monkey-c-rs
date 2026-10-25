use crate::ast::{Ident, Parameter, Parens, Position, Separated, Span, Spanned};

/// A type annotation, optionally nullable and optionally a union with other types.
///
/// A `,` and `or` aren't interchangeable inside `<…>`: `Dictionary<String, Number>` has two
/// generic arguments, while `Array<Number or Null>` has one that is itself a union.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/>
#[derive(Debug, PartialEq)]
pub struct Type {
    pub kind: TypeKind,
    /// The union members after the first, which is this type itself.
    pub alternatives: Vec<UnionAlternative>,
    /// Written with a `?`, as in `String?`.
    pub optional: bool,
    pub span: Span,
}

/// One peer of a union together with the separator written before it. `or` and `|` mean the
/// same thing, but the one written is kept so the formatter can keep it.
#[derive(Debug, PartialEq)]
pub struct UnionAlternative {
    pub separator: Spanned<UnionSeparator>,
    pub type_: Type,
}

/// How the members of a union are separated. Both mean the same.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum UnionSeparator {
    Or,
    Pipe,
}

impl UnionSeparator {
    /// The separator as written in source.
    pub fn as_str(self) -> &'static str {
        match self {
            UnionSeparator::Or => "or",
            UnionSeparator::Pipe => "|",
        }
    }
}

/// The shape of a [`Type`], before its union members and `?`.
#[derive(Debug, PartialEq)]
pub enum TypeKind {
    /// A named type, possibly generic, e.g. `Number`, `Array<T>` or `Dictionary<K, V>`.
    Named {
        ident: Ident,
        generic_params: Vec<Type>,
    },
    /// An inline dictionary type, e.g. `{ :key1 as T, "key2" as U }`. `body_span` covers the
    /// braces.
    Dict {
        entries: Separated<DictTypeEntry>,
        body_span: Span,
    },
    /// An inline interface type of function signatures and typed variables, e.g.
    /// `interface { function foo() as X; var bar as Y; }`. `body_span` covers the braces.
    Interface {
        members: Vec<InterfaceMember>,
        body_span: Span,
    },
    /// A tuple type, an array whose entries have the listed types by position, e.g.
    /// `[Number, String]`.
    Tuple { elements: Vec<Type> },
    /// A callable type, e.g. `Method(arg as T) as Return`, where the return type is optional. Any
    /// name is accepted in place of `Method` so unfamiliar SDK callable types still parse.
    Method {
        name: Ident,
        args: Parens<Separated<Parameter>>,
        returns: Option<Box<Type>>,
    },
    /// A parenthesized type, which matters when a `?` or union would otherwise bind to part of it,
    /// e.g. `(Method(x) as Void)?` is a nullable callable rather than one returning `Void?`.
    Group(Parens<Box<Type>>),
}

/// A member of an `interface { … }` type, either a function signature or a typed variable.
#[derive(Debug, PartialEq)]
pub enum InterfaceMember {
    Function(InterfaceMethod),
    Variable(InterfaceVar),
}

/// A method signature inside an interface type, like [`FunctionDecl`](crate::ast::FunctionDecl)
/// without a body.
#[derive(Debug, PartialEq)]
pub struct InterfaceMethod {
    pub name: Spanned<Ident>,
    pub args: Parens<Separated<Parameter>>,
    pub returns: Option<Type>,
    pub as_kw_start: Option<Position>,
    pub span: Span,
}

/// A variable inside an interface type, e.g. `var name as Type;`. The type is required.
#[derive(Debug, PartialEq)]
pub struct InterfaceVar {
    pub name: Spanned<Ident>,
    pub as_kw_start: Position,
    pub type_: Type,
    pub span: Span,
}

/// One `key as Type` entry of an inline dictionary type.
#[derive(Debug, PartialEq)]
pub struct DictTypeEntry {
    pub key: DictTypeKey,
    pub value_type: Type,
    pub span: Span,
}

/// The key of a [`DictTypeEntry`], a symbol like `:name` or a string like `"name"`, without the
/// `:` or quotes.
#[derive(Debug, PartialEq)]
pub enum DictTypeKey {
    Symbol(String),
    String(String),
}

impl InterfaceMember {
    /// The source range of the member, its `;` included.
    pub fn span(&self) -> Span {
        match self {
            InterfaceMember::Function(method) => method.span,
            InterfaceMember::Variable(variable) => variable.span,
        }
    }
}

impl Type {
    /// The name of a named or method type, looking through groups.
    pub fn ident(&self) -> Option<&str> {
        match &self.kind {
            TypeKind::Named { ident, .. } => Some(ident),
            TypeKind::Method { name, .. } => Some(name),
            TypeKind::Group(group) => group.inner.ident(),
            TypeKind::Dict { .. } | TypeKind::Interface { .. } | TypeKind::Tuple { .. } => None,
        }
    }

    /// The generic arguments of a named type, looking through groups.
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
