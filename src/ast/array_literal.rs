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
        let elements = self
            .elements
            .iter()
            .map(|e| e.to_string())
            .collect::<Vec<String>>();
        write!(f, "[{}]", elements.join(", "))
    }
}
