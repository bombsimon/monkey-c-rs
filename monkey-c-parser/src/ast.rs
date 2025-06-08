pub type Ident = String;

#[derive(Debug, PartialEq)]
pub enum Visibility {
    Private,
    Protected,
    Public,
}

#[derive(Debug, PartialEq)]
pub struct Type {
    pub ident: Ident,
    pub generic_params: Vec<Type>,
    pub optional: bool,
}

#[derive(Debug, PartialEq)]
pub struct Variable {
    pub name: Ident,
    pub type_: Option<Type>,
    pub visibility: Option<Visibility>,
    pub initializer: Option<Box<Ast>>,
    pub is_static: bool,
    pub is_hidden: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl From<(usize, usize)> for Span {
    fn from((start, end): (usize, usize)) -> Self {
        Self { start, end }
    }
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    // Top-level declarations
    Document(Vec<Ast>),
    Import {
        name: Ident,
        alias: Option<Ident>,
        span: Span,
    },
    Class {
        name: Ident,
        extends: Option<Ident>,
        annotations: Vec<String>,
        body: Vec<Ast>,
        span: Span,
    },
    Function {
        name: Ident,
        args: Vec<Variable>,
        returns: Option<Type>,
        annotations: Vec<String>,
        body: Vec<Ast>,
        visibility: Option<Visibility>,
        is_static: bool,
        is_hidden: bool,
        span: Span,
    },

    // Comments and annotations
    Comment(String, Span),
    Annotation(String, Span),

    // Statements
    Block(Vec<Ast>, Span),
    If {
        condition: Box<Ast>,
        then_branch: Box<Ast>,
        else_branch: Option<Box<Ast>>,
        span: Span,
    },
    While {
        condition: Box<Ast>,
        body: Box<Ast>,
        span: Span,
    },
    For {
        init: Option<Box<Ast>>,
        condition: Option<Box<Ast>>,
        update: Option<Box<Ast>>,
        body: Box<Ast>,
        span: Span,
    },
    Return(Option<Box<Ast>>, Span),
    Break(Span),
    Continue(Span),

    // Declarations & Assignments
    Variable(Variable, Span),
    Assign {
        target: Box<Ast>,
        operator: AssignOperator,
        value: Box<Ast>,
        span: Span,
    },

    // Expressions
    Binary {
        left: Box<Ast>,
        operator: BinaryOperator,
        right: Box<Ast>,
        span: Span,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Ast>,
        span: Span,
    },
    Call {
        callee: Box<Ast>,
        args: Vec<Ast>,
        span: Span,
    },
    Member {
        object: Box<Ast>,
        property: Ident,
        span: Span,
    },
    Index {
        object: Box<Ast>,
        index: Box<Ast>,
        span: Span,
    },
    New {
        class: Ident,
        args: Vec<Ast>,
        span: Span,
    },
    TypeCast {
        expr: Box<Ast>,
        target_type: Type,
        span: Span,
    },
    Array(Vec<Ast>, Span),
    Dictionary(Vec<(Ast, Ast)>, Span),

    // Literals & Identifiers
    BasicLit(LiteralValue, Span),
    Identifier(Ident, Span),
    Me(Span),
    Self_(Span),
    Eof,
}

#[derive(Debug, PartialEq)]
pub enum LiteralValue {
    Long(i64),
    Double(f64),
    String(String),
    Boolean(bool),
    Null,
    NaN,
}

#[derive(Debug, PartialEq)]
pub enum BinaryOperator {
    // Arithmetic
    Add, // +
    Sub, // -
    Mul, // *
    Div, // /
    Mod, // %

    // Comparison
    Eq,         // ==
    NotEq,      // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    InstanceOf, // instanceof

    // Logical
    And, // &&
    Or,  // ||

    // Bitwise
    BitAnd, // &
    BitOr,  // |
    BitXor, // ^
}

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
