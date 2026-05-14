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
        BinaryOperator::And => "&&",
        BinaryOperator::Or => "||",
        BinaryOperator::BitAnd => "&",
        BinaryOperator::BitOr => "|",
        BinaryOperator::BitXor => "^",
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
    }
}
