use crate::token::Kind;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum PrefixOperator {
    Minus,
    Not,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::PrefixOperator::*;
        let repr = match self {
            Minus => Kind::Minus,
            Not => Kind::Bang,
        };
        write!(f, "{}", repr)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::InfixOperator::*;
        let repr = match self {
            Add => Kind::Plus,
            Subtract => Kind::Minus,
            Multiply => Kind::Asterisk,
            Divide => Kind::Slash,
            GreaterThan => Kind::GT,
            LessThan => Kind::LT,
            Equals => Kind::EQ,
            NotEquals => Kind::NotEQ,
        };
        write!(f, "{}", repr)
    }
}
