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
