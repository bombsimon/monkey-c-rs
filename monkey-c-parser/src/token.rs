#![allow(dead_code)]
use std::str::FromStr;

/// Language specification https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Type {
    Newline,
    LParen,     // (
    RParen,     // )
    LSqBracket, // [
    RSqBracket, // ]
    LBracket,   // {
    RBracket,   // }
    Eof,

    Identifier(String),
    Comment(String),

    // Characters
    Comma,        // ,
    Colon,        // :
    SemiColon,    // ;
    Dot,          // .
    QuestionMark, // ?

    // Types
    Number(i32), // Not used
    Float(f32),  // Not used
    Long(i64),
    Double(f64),
    Boolean(bool),
    Char(char),
    String(String),
    Symbol(String),

    // Containers
    Array,
    Dictionary,

    // Keywords not in spec?
    Import,
    Use,

    // Keywords
    And,
    As,
    Break,
    Catch,
    Case,
    Class,
    Const,
    Continue,
    Default,
    Do,
    Else,
    Enum,
    Extends,
    Finally,
    For,
    Function,
    Has,
    Hidden,
    If,
    InstanceOf,
    Me,
    Module,
    NaN,
    Native,
    New,
    Null,
    Or,
    Private,
    Protected,
    Public,
    Return,
    Self_,
    Static,
    Switch,
    Throw,
    Try,
    Using,
    Var,
    While,

    // ArithmeticOperator
    Plus,      // +
    Minus,     // -
    Multiply,  // *
    Divide,    // /
    Modulu,    // %
    Increment, // ++
    Decrement, // --

    // RelationalOperator
    Equal,          // ==
    NotEqual,       // !=
    Greater,        // >
    Less,           // <
    GreaterOrEqual, // >=
    LessOrEqual,    // <=

    // LogicalOperator
    LogicalAnd, // &&
    LogicalOr,  // ||
    LogicalNot, // !

    // BitwiseOperator
    BitwiseAnd, // &
    BitwiseOr,  // |
    BitwiseXor, // ^
    BitwiseNot, // ~

    // AssignmentOperator
    Assign,           // =
    AssignAdd,        // +=
    AssignSubtract,   // -=
    AssignMultiply,   // *=
    AssignDivide,     // /=
    AssignRemainder,  // %=
    AssignLeftShift,  // <<=
    AssignRightShift, // >>=
    AssignAnd,        // &=
    AssignOr,         // |=
    AssignXor,        // ^=

    // MiscellaneousOperator
    TernaryIf,   // ?
    TernaryElse, // :
}

impl FromStr for Type {
    type Err = ();

    fn from_str(input: &str) -> Result<Type, Self::Err> {
        match input {
            "," => Ok(Type::Comma),
            ":" => Ok(Type::Colon),
            ";" => Ok(Type::SemiColon),
            "." => Ok(Type::Dot),
            "?" => Ok(Type::QuestionMark),

            // "?" => Ok(Type::TernaryIf),
            // ":" => Ok(Type::TernaryElse),
            "(" => Ok(Type::LParen),
            ")" => Ok(Type::RParen),
            "[" => Ok(Type::LSqBracket),
            "]" => Ok(Type::RSqBracket),
            "{" => Ok(Type::LBracket),
            "}" => Ok(Type::RBracket),

            "import" => Ok(Type::Import),
            "use" => Ok(Type::Use),

            "and" => Ok(Type::And),
            "as" => Ok(Type::As),
            "break" => Ok(Type::Break),
            "catch" => Ok(Type::Catch),
            "case" => Ok(Type::Case),
            "class" => Ok(Type::Class),
            "const" => Ok(Type::Const),
            "continue" => Ok(Type::Continue),
            "default" => Ok(Type::Default),
            "do" => Ok(Type::Do),
            "else" => Ok(Type::Else),
            "enum" => Ok(Type::Enum),
            "extends" => Ok(Type::Extends),
            "finally" => Ok(Type::Finally),
            "for" => Ok(Type::For),
            "function" => Ok(Type::Function),
            "has" => Ok(Type::Has),
            "hidden" => Ok(Type::Hidden),
            "if" => Ok(Type::If),
            "instanceof" => Ok(Type::InstanceOf),
            "me" => Ok(Type::Me),
            "module" => Ok(Type::Module),
            "nan" => Ok(Type::NaN),
            "native" => Ok(Type::Native),
            "new" => Ok(Type::New),
            "null" => Ok(Type::Null),
            "or" => Ok(Type::Or),
            "private" => Ok(Type::Private),
            "protected" => Ok(Type::Protected),
            "public" => Ok(Type::Public),
            "return" => Ok(Type::Return),
            "self" => Ok(Type::Self_),
            "static" => Ok(Type::Static),
            "switch" => Ok(Type::Switch),
            "throw" => Ok(Type::Throw),
            "try" => Ok(Type::Try),
            "using" => Ok(Type::Using),
            "var" => Ok(Type::Var),
            "while" => Ok(Type::While),

            "+" => Ok(Type::Plus),
            "-" => Ok(Type::Minus),
            "*" => Ok(Type::Multiply),
            "/" => Ok(Type::Divide),
            "%" => Ok(Type::Modulu),
            "++" => Ok(Type::Increment),
            "--" => Ok(Type::Decrement),

            "==" => Ok(Type::Equal),
            "!=" => Ok(Type::NotEqual),
            ">" => Ok(Type::Greater),
            "<" => Ok(Type::Less),
            ">=" => Ok(Type::GreaterOrEqual),
            "<=" => Ok(Type::LessOrEqual),

            "&&" => Ok(Type::LogicalAnd),
            "||" => Ok(Type::LogicalOr),
            "!" => Ok(Type::LogicalNot),

            "&" => Ok(Type::BitwiseAnd),
            "|" => Ok(Type::BitwiseOr),
            "^" => Ok(Type::BitwiseXor),
            "~" => Ok(Type::BitwiseNot),

            "=" => Ok(Type::Assign),
            "+=" => Ok(Type::AssignAdd),
            "-=" => Ok(Type::AssignSubtract),
            "*=" => Ok(Type::AssignMultiply),
            "/=" => Ok(Type::AssignDivide),
            "%=" => Ok(Type::AssignRemainder),
            "<<=" => Ok(Type::AssignLeftShift),
            ">>=" => Ok(Type::AssignRightShift),
            "&=" => Ok(Type::AssignAnd),
            "|=" => Ok(Type::AssignOr),
            "^=" => Ok(Type::AssignXor),

            "true" => Ok(Type::Boolean(true)),
            "false" => Ok(Type::Boolean(false)),

            _ => Err(()),
        }
    }
}

pub(crate) type Pos = usize;
pub(crate) type Span = (Pos, Type, Pos);
