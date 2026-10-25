use crate::ast::{
    AssignOperator, BinaryOperator, Ident, LiteralValue, Position, Separated, Span, Type,
    UnaryOperator,
};

/// An expression. Assignment is an expression in the tree even though Monkey C only allows it
/// as a statement or `for` clause.
#[derive(Debug, PartialEq)]
pub enum Expr {
    /// A binary operation, e.g. `a + b`.
    Binary(BinaryExpr),
    /// A prefix or postfix operation, e.g. `-a` or `a++`.
    Unary(UnaryExpr),
    /// A conditional, e.g. `a ? b : c`.
    Ternary(TernaryExpr),
    /// An assignment, e.g. `a = b` or `a += b`.
    Assign(AssignExpr),
    /// A function or method call, e.g. `f(a, b)`.
    Call(CallExpr),
    /// Access to a member, e.g. `a.b`.
    Member(MemberExpr),
    /// Indexing into an array or dictionary, e.g. `a[i]`.
    Index(IndexExpr),
    /// Creating an instance of a class, e.g. `new Foo(a)`.
    New(NewExpr),
    /// Allocating an array of a given size, e.g. `new [size]`.
    NewArray(NewArrayExpr),
    /// A cast to a type, e.g. `a as Number`.
    TypeCast(TypeCastExpr),
    /// An array literal, e.g. `[1, 2]`.
    Array(ArrayExpr),
    /// A dictionary literal, e.g. `{:a => 1}`.
    Dict(DictExpr),
    /// A literal value, e.g. `1`, `"text"` or `:symbol`.
    Lit(LitExpr),
    /// A reference to a name, e.g. `count`.
    Ident(IdentExpr),
    /// A parenthesized expression, kept so the grouping written survives.
    Paren(ParenExpr),
    /// The `me` keyword, referring to the current instance.
    Me(Span),
    /// The `self` keyword, referring to the current class.
    Self_(Span),
    /// The `$` bling symbol, referring to the global namespace.
    Bling(Span),
}

/// An expression in parentheses, e.g. `(1 + 2)`.
#[derive(Debug, PartialEq)]
pub struct ParenExpr {
    pub inner: Box<Expr>,
    pub span: Span,
}

/// A binary expression, e.g. `a + b`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operators>
#[derive(Debug, PartialEq)]
pub struct BinaryExpr {
    pub left: Box<Expr>,
    pub operator: BinaryOperator,
    pub op_pos: Position,
    pub right: Box<Expr>,
    pub span: Span,
}

/// A prefix or postfix operator applied to `operand`, e.g. `-a` or `a++`.
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
    pub question_pos: Position,
    pub then_expr: Box<Expr>,
    pub colon_pos: Position,
    pub else_expr: Box<Expr>,
    pub span: Span,
}

/// Assigning a value to a variable, e.g. `foo = bar` or `biz += 1`.
#[derive(Debug, PartialEq)]
pub struct AssignExpr {
    pub target: Box<Expr>,
    pub operator: AssignOperator,
    pub op_pos: Position,
    pub value: Box<Expr>,
    pub span: Span,
}

/// The call site of a function or method, e.g. `someFn(arg1, arg2)`. `args_open` is where the
/// `(` is.
#[derive(Debug, PartialEq)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Separated<CallArg>,
    pub args_open: Position,
    pub span: Span,
}

/// One argument of a call or `new`.
#[derive(Debug, PartialEq)]
pub struct CallArg {
    pub value: Expr,
}

/// Access to a member of an object, e.g. `object.property`. A chain like `a.b.c` nests, with the
/// last property outermost.
#[derive(Debug, PartialEq)]
pub struct MemberExpr {
    pub object: Box<Expr>,
    pub property: Ident,
    pub span: Span,
}

/// Indexing into an array or dictionary, e.g. `array[idx]`.
///
/// <https://developer.garmin.com/connect-iq/api-docs/Toybox/Lang/Array.html>
#[derive(Debug, PartialEq)]
pub struct IndexExpr {
    pub object: Box<Expr>,
    pub index: Box<Expr>,
    pub span: Span,
}

/// Creating a new instance of a class, e.g. `new MyModule.Foo(args)`. `args_open` is `None` for
/// `new Foo` without parentheses, which means the same as `new Foo()`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#miscellaneous-operators>
#[derive(Debug, PartialEq)]
pub struct NewExpr {
    pub class: Ident,
    pub class_span: Span,
    pub args: Separated<CallArg>,
    pub args_open: Option<Position>,
    pub span: Span,
}

/// Allocating an array of a given size, e.g. `new [size]` or `new Array<Number>[size]`, which is
/// a `ByteArray` when written as `new [size]b`.
#[derive(Debug, PartialEq)]
pub struct NewArrayExpr {
    pub element_type: Option<Type>,
    pub size: Box<Expr>,
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

/// An array literal, e.g. `[1, 2, 3]`, which is a `ByteArray` when written as `[1, 2, 3]b`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#arrays>
#[derive(Debug, PartialEq)]
pub struct ArrayExpr {
    pub entries: Separated<ArrayEntry>,
    pub is_byte_array: bool,
    pub span: Span,
}

/// One entry of an array literal.
#[derive(Debug, PartialEq)]
pub struct ArrayEntry {
    pub value: Expr,
}

/// A dictionary literal, e.g. `{:key => value}`. Keys can be any expression.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#dictionaries>
#[derive(Debug, PartialEq)]
pub struct DictExpr {
    pub entries: Separated<DictEntry>,
    pub span: Span,
}

/// One `key => value` entry of a dictionary literal.
#[derive(Debug, PartialEq)]
pub struct DictEntry {
    pub key: Expr,
    pub arrow_pos: Position,
    pub value: Expr,
}

/// A literal value with its span, e.g. a number, string or symbol.
#[derive(Debug, PartialEq)]
pub struct LitExpr {
    pub value: LiteralValue,
    pub span: Span,
}

/// A reference to a name with its span, e.g. `count`.
#[derive(Debug, PartialEq)]
pub struct IdentExpr {
    pub name: Ident,
    pub span: Span,
}

impl Expr {
    /// The source range of the expression, parentheses included for [`Expr::Paren`].
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
