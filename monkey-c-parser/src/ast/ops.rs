/// An operator between two operands. `and` and `or` are kept apart from `&&` and `||` so the
/// spelling written survives, though they mean the same. [`BinaryOperator::as_str`] gives the
/// spelling.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operator-precedence>
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#instanceof-and-has>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    InstanceOf,
    Has,
    And,
    AndKeyword,
    Or,
    OrKeyword,
    BitAnd,
    BitOr,
    BitXor,
    LeftShift,
    RightShift,
}

impl BinaryOperator {
    /// The operator as written in source, like `&&` or `instanceof`.
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOperator::Add => "+",
            BinaryOperator::Sub => "-",
            BinaryOperator::Mul => "*",
            BinaryOperator::Div => "/",
            BinaryOperator::Mod => "%",
            BinaryOperator::Eq => "==",
            BinaryOperator::NotEq => "!=",
            BinaryOperator::Lt => "<",
            BinaryOperator::LtEq => "<=",
            BinaryOperator::Gt => ">",
            BinaryOperator::GtEq => ">=",
            BinaryOperator::InstanceOf => "instanceof",
            BinaryOperator::Has => "has",
            BinaryOperator::And => "&&",
            BinaryOperator::AndKeyword => "and",
            BinaryOperator::Or => "||",
            BinaryOperator::OrKeyword => "or",
            BinaryOperator::BitAnd => "&",
            BinaryOperator::BitOr => "|",
            BinaryOperator::BitXor => "^",
            BinaryOperator::LeftShift => "<<",
            BinaryOperator::RightShift => ">>",
        }
    }

    /// Relative binding strength (higher binds tighter). Unlike C, the bitwise and shift operators
    /// share levels with the arithmetic ones, and `instanceof` and `has` bind as tight as `*`. The
    /// reference doesn't list this, so it was confirmed by running expressions in the simulator.
    ///
    /// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operator-precedence>
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Or | BinaryOperator::OrKeyword => 1,
            BinaryOperator::And | BinaryOperator::AndKeyword => 2,
            BinaryOperator::Eq
            | BinaryOperator::NotEq
            | BinaryOperator::Lt
            | BinaryOperator::LtEq
            | BinaryOperator::Gt
            | BinaryOperator::GtEq => 3,
            BinaryOperator::Add
            | BinaryOperator::Sub
            | BinaryOperator::BitOr
            | BinaryOperator::BitXor => 4,
            BinaryOperator::Mul
            | BinaryOperator::Div
            | BinaryOperator::Mod
            | BinaryOperator::LeftShift
            | BinaryOperator::RightShift
            | BinaryOperator::BitAnd
            | BinaryOperator::InstanceOf
            | BinaryOperator::Has => 5,
        }
    }
}

/// An operator with a single operand, before it like `-x` or after it like `x++`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#arithmetic-operators>
#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Pos,
    Neg,
    Not,
    BitNot,
    PreInc,
    PreDec,
    PostInc,
    PostDec,
    /// The legacy `@` in front of a resource like `@Rez.Strings.name`.
    ResourceRef,
}

/// `=` or a compound assignment like `+=`.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#assignment-operators>
#[derive(Debug, PartialEq)]
pub enum AssignOperator {
    Assign,
    AddAssign,
    SubAssign,
    MulAssign,
    DivAssign,
    ModAssign,
    BitAndAssign,
    BitOrAssign,
    BitXorAssign,
    LeftShiftAssign,
    RightShiftAssign,
}

impl UnaryOperator {
    /// The operator as written in source, the same for `++x` and `x++`.
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOperator::Pos => "+",
            UnaryOperator::Neg => "-",
            UnaryOperator::Not => "!",
            UnaryOperator::BitNot => "~",
            UnaryOperator::PreInc => "++",
            UnaryOperator::PreDec => "--",
            UnaryOperator::PostInc => "++",
            UnaryOperator::PostDec => "--",
            UnaryOperator::ResourceRef => "@",
        }
    }
}

impl AssignOperator {
    /// The operator as written in source, like `+=`.
    pub fn as_str(&self) -> &'static str {
        match self {
            AssignOperator::Assign => "=",
            AssignOperator::AddAssign => "+=",
            AssignOperator::SubAssign => "-=",
            AssignOperator::MulAssign => "*=",
            AssignOperator::DivAssign => "/=",
            AssignOperator::ModAssign => "%=",
            AssignOperator::BitAndAssign => "&=",
            AssignOperator::BitOrAssign => "|=",
            AssignOperator::BitXorAssign => "^=",
            AssignOperator::LeftShiftAssign => "<<=",
            AssignOperator::RightShiftAssign => ">>=",
        }
    }
}
