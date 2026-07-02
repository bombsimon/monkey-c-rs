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

#[derive(Debug, PartialEq)]
pub struct ParenExpr {
    pub inner: Box<Expr>,
    pub span: Span,
}

#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    /// Byte offset of the operator token — used to split comments between the
    /// left operand and the operator vs between the operator and the right operand.
    pub op_pos: Position,
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
    /// `true` when the source had a trailing comma after the last argument.
    /// Drives the magic trailing comma formatting rule — forces multi-line
    /// rendering even when the call would otherwise fit on a single line.
    pub args_trailing_comma: bool,
    /// Combined with `span.end`, defines the argument list zone for comment placement.
    pub args_open: Position,
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
    /// See [`CallExpr::args_trailing_comma`].
    pub args_trailing_comma: bool,
    /// See [`CallExpr::args_open`]. `None` when `new Foo` omits the
    /// argument list entirely (equivalent to `new Foo()`).
    pub args_open: Option<Position>,
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
