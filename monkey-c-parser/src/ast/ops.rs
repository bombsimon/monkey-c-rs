/// A binary (two-operand) operator.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operator-precedence>
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#instanceof-and-has>
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOperator {
    Add,        // +
    Sub,        // -
    Mul,        // *
    Div,        // /
    Mod,        // %
    Eq,         // ==
    NotEq,      // !=
    Lt,         // <
    LtEq,       // <=
    Gt,         // >
    GtEq,       // >=
    InstanceOf, // instanceof
    Has,        // has
    And,        // &&
    AndKeyword, // and
    Or,         // ||
    OrKeyword,  // or
    BitAnd,     // &
    BitOr,      // |
    BitXor,     // ^
    LeftShift,  // <<
    RightShift, // >>
}

impl BinaryOperator {
    /// Relative binding strength (higher binds tighter).
    ///
    /// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#operator-precedence>
    pub fn precedence(&self) -> u8 {
        match self {
            BinaryOperator::Or | BinaryOperator::OrKeyword => 1,
            BinaryOperator::And | BinaryOperator::AndKeyword => 2,
            BinaryOperator::BitOr => 3,
            BinaryOperator::BitXor => 4,
            BinaryOperator::BitAnd => 5,
            BinaryOperator::Eq | BinaryOperator::NotEq => 6,
            BinaryOperator::Lt
            | BinaryOperator::LtEq
            | BinaryOperator::Gt
            | BinaryOperator::GtEq
            | BinaryOperator::InstanceOf
            | BinaryOperator::Has => 7,
            BinaryOperator::LeftShift | BinaryOperator::RightShift => 8,
            BinaryOperator::Add | BinaryOperator::Sub => 9,
            BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => 10,
        }
    }
}

/// A unary (single-operand) operator.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#arithmetic-operators>
#[derive(Debug, PartialEq)]
pub enum UnaryOperator {
    Pos,         // +
    Neg,         // -
    Not,         // !
    BitNot,      // ~
    PreInc,      // ++x
    PreDec,      // --x
    PostInc,     // x++
    PostDec,     // x--
    ResourceRef, // @Rez.Strings.foo
}

/// A compound assignment operator.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#assignment-operators>
#[derive(Debug, PartialEq)]
pub enum AssignOperator {
    Assign,           // =
    AddAssign,        // +=
    SubAssign,        // -=
    MulAssign,        // *=
    DivAssign,        // /=
    ModAssign,        // %=
    BitAndAssign,     // &=
    BitOrAssign,      // |=
    BitXorAssign,     // ^=
    LeftShiftAssign,  // <<=
    RightShiftAssign, // >>=
}
