#![allow(dead_code)]
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
}

#[derive(Debug, PartialEq)]
pub enum Ast {
    // Top-level declarations
    Document(Vec<Ast>),
    Import {
        name: Ident,
        alias: Option<Ident>,
    },
    Class {
        name: Ident,
        extends: Option<Ident>,
        annotations: Vec<String>,
        body: Vec<Ast>,
    },
    Function {
        name: Ident,
        args: Vec<Variable>,
        returns: Option<Type>,
        annotations: Vec<String>,
        body: Vec<Ast>,
    },
    
    // Comments and annotations
    Comment(String),
    Annotation(String),
    
    // Statements
    Block(Vec<Ast>),
    If {
        condition: Box<Ast>,
        then_branch: Box<Ast>,
        else_branch: Option<Box<Ast>>,
    },
    While {
        condition: Box<Ast>,
        body: Box<Ast>,
    },
    For {
        init: Option<Box<Ast>>,
        condition: Option<Box<Ast>>,
        update: Option<Box<Ast>>,
        body: Box<Ast>,
    },
    Return(Option<Box<Ast>>),
    Break,
    Continue,
    
    // Declarations & Assignments
    Variable(Variable),
    Assign {
        target: Box<Ast>,
        operator: AssignOperator,
        value: Box<Ast>,
    },
    
    // Expressions
    Binary {
        left: Box<Ast>,
        operator: BinaryOperator,
        right: Box<Ast>,
    },
    Unary {
        operator: UnaryOperator,
        operand: Box<Ast>,
    },
    Call {
        callee: Box<Ast>,
        args: Vec<Ast>,
    },
    Member {
        object: Box<Ast>,
        property: Ident,
    },
    Index {
        object: Box<Ast>,
        index: Box<Ast>,
    },
    New {
        class: Ident,
        args: Vec<Ast>,
    },
    TypeCast {
        expr: Box<Ast>,
        target_type: Type,
    },
    Array(Vec<Ast>),
    Dictionary(Vec<(Ast, Ast)>),
    
    // Literals & Identifiers
    BasicLit(LiteralValue),
    Identifier(Ident),
    Me,
    Self_,
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
    Add,      // +
    Sub,      // -
    Mul,      // *
    Div,      // /
    Mod,      // %
    
    // Comparison
    Eq,       // ==
    NotEq,    // !=
    Lt,       // <
    LtEq,     // <=
    Gt,       // >
    GtEq,     // >=
    
    // Logical
    And,      // &&
    Or,       // ||
    
    // Bitwise
    BitAnd,   // &
    BitOr,    // |
    BitXor,   // ^
}

#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Neg,      // -
    Not,      // !
    BitNot,   // ~
    PreInc,   // ++x
    PreDec,   // --x
    PostInc,  // x++
    PostDec,  // x--
}

#[derive(Debug, PartialEq)]
pub enum AssignOperator {
    Assign,          // =
    AddAssign,       // +=
    SubAssign,       // -=
    MulAssign,       // *=
    DivAssign,      // /=
    ModAssign,      // %=
    BitAndAssign,   // &=
    BitOrAssign,    // |=
    BitXorAssign,   // ^=
}
