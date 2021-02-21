use crate::ast::{Expression, ExpressionKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    token: Token,
    elements: Vec<Expression>,
}

impl ArrayLiteral {
    pub fn new(token: Token, elements: Vec<Expression>) -> Expression {
        Expression::new(ExpressionKind::Array(ArrayLiteral { token, elements }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn elements(&self) -> &[Expression] {
        &self.elements
    }
}

impl Display for ArrayLiteral {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "[")?;
        if !self.elements.is_empty() {
            for elem in self.elements.iter().take(self.elements.len() - 1) {
                write!(f, "{}, ", elem)?;
            }
        }
        write!(f, "]")
    }
}
