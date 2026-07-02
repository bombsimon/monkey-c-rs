use crate::ast::{
    AssignOperator, BinaryOperator, Ident, LiteralValue, Position, Span, Type, UnaryOperator,
};

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

/// An expression that lives between two parenthesis like `(1 + 2)`.
#[derive(Debug, PartialEq)]
pub struct ParenExpr {
    pub inner: Box<Expr>,
    pub span: Span,
}

/// A binary expression of format `<left> <op> <right>`.
//
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operators>
#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub op_pos: Position,
    pub right: Box<Expr>,
    pub span: Span,
}

/// A unary expression.
#[derive(Debug, PartialEq)]
pub struct UnaryExpr {
    pub operator: UnaryOperator,
    pub operand: Box<Expr>,
    pub span: Span,
}

/// A ternary conditional `cond ? then : else`. Right-associative, chains like `a ? b : c ? d : e`
/// parse as `a ? b : (c ? d : e)`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#if-statements>
#[derive(Debug, PartialEq)]
pub struct TernaryExpr {
    pub condition: Box<Expr>,
    pub then_expr: Box<Expr>,
    pub else_expr: Box<Expr>,
    pub span: Span,
}

/// Assigning a variable to a value such as `foo = bar` or `biz += 1`.
#[derive(Debug, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub operator: AssignOperator,
    pub value: Box<Expr>,
    pub span: Span,
}

/// A call expression is the call site of a function or method, e.g. `someFn(arg1, arg2)`.
#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<CallArg>,
    /// `true` when the source had a trailing comma after the last argument.
    /// Drives the magic trailing comma formatting rule — forces multi-line
    /// rendering even when the call would otherwise fit on a single line.
    pub args_trailing_comma: bool,
    pub args_open: Position,
    pub span: Span,
}

/// A single argument inside a call or `new` expression.
#[derive(Debug, PartialEq)]
pub struct CallArg {
    pub value: Expr,
}

/// A member expression representing a member property of an object, e.g. `object.property`.
#[derive(Debug, PartialEq)]
pub struct MemberExpr {
    pub object: Box<Expr>,
    /// The property name after the `.`. Multiple dots are chained in separate [`Expr::Member`] nodes.
    pub property: Ident,
    pub span: Span,
}

/// Indexing into arrays, e.g. `array[idx]`.
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Array.html>
#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

/// Creating a new instance of an object, e.g. `new MyModule.Foo`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#miscellaneous-operators>
#[derive(Debug, PartialEq)]
pub struct NewExpr {
    pub class: Ident,
    pub args: Vec<CallArg>,
    /// See [`CallExpr::args_trailing_comma`].
    pub args_trailing_comma: bool,
    pub args_open: Option<Position>,
    pub span: Span,
}

/// A `new [size]` or `new Array<Number>[size]` allocation. `element_type` is `None` for the bare
/// form (typeless array of the given size).
#[derive(Debug, PartialEq)]
pub struct NewArrayExpr {
    pub element_type: Option<Type>,
    pub size: Box<Expr>,
    /// A `]b` suffix marks the allocation as a `Lang.ByteArray` (`new [N]b`).
    pub is_byte_array: bool,
    pub span: Span,
}

/// A cast of a value to a type, e.g. `foo as Number`.
///
/// <https://developer.garmin.com/connect-iq/monkey-c/monkey-types/#type-casting>
#[derive(Debug, PartialEq)]
pub struct TypeCastExpr {
    pub expr: Box<Expr>,
    pub target_type: Type,
    pub span: Span,
}

/// An array expression, e.g. `[1, 2, 3]`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#arrays>
#[derive(Debug, PartialEq)]
pub struct ArrayExpr {
    pub entries: Vec<ArrayEntry>,
    pub trailing_comma: bool,
    /// A `]b` suffix marks the literal as a `Lang.ByteArray`. The parser
    /// recognises any `b` identifier immediately following the closing `]`.
    pub is_byte_array: bool,
    pub span: Span,
}

/// An element in an array.
#[derive(Debug, PartialEq)]
pub struct ArrayEntry {
    pub value: Expr,
}

/// A dictionary expression, e.g. `{:key => value}`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#dictionaries>
#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub entries: Vec<DictEntry>,
    pub trailing_comma: bool,
    pub span: Span,
}

/// An element in a dictionary.
#[derive(Debug, PartialEq)]
pub struct DictEntry {
    pub key: Expr,
    pub value: Expr,
}

/// A literal value and belonging span. A literal value are things like strings, numbers, symbols
/// etc.
#[derive(Debug, PartialEq)]
pub struct LitExpr {
    pub value: LiteralValue,
    pub span: Span,
}

/// An identifier and belonging span.
#[derive(Debug, PartialEq)]
pub struct IdentExpr {
    pub name: Ident,
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
