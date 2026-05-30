/// Source-text representation of each operator type.
///
/// These are free functions rather than methods because they carry no
/// formatter state — they are pure token-to-string mappings.
use monkey_c_parser::ast::{AssignOperator, BinaryOperator, UnaryOperator};

pub(crate) fn binary_op(op: &BinaryOperator) -> &'static str {
    match op {
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

/// Returns the prefix spelling of a unary operator.
///
/// Panics if called with a postfix operator (`PostInc` / `PostDec`) — callers
/// must handle those separately since they follow their operand.
pub(crate) fn unary_prefix_op(op: &UnaryOperator) -> &'static str {
    match op {
        UnaryOperator::Neg => "-",
        UnaryOperator::Not => "!",
        UnaryOperator::BitNot => "~",
        UnaryOperator::PreInc => "++",
        UnaryOperator::PreDec => "--",
        UnaryOperator::PostInc | UnaryOperator::PostDec => unreachable!(),
    }
}

/// Precedence-tier identifier used by the formatter to decide which adjacent
/// binary operators can collapse into one breakable chain. Two operators
/// share a chain when they have the same precedence — `+` and `-`, or
/// `*` / `/` / `%`, or the comparison operators — so a mixed expression like
/// `a + b - c + d` wraps at every `+`/`-` rather than nesting.
///
/// Different precedence tiers stay nested so the precedence structure
/// remains visible after wrapping: `a + b * c + d` keeps `b * c` as a tight
/// inner group instead of letting `*` be part of the outer `+` chain.
pub(crate) fn precedence_group(op: &BinaryOperator) -> u8 {
    match op {
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

pub(crate) fn assign_op(op: &AssignOperator) -> &'static str {
    match op {
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
