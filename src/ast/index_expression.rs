use crate::ast::{Expression, ExpressionKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Clone, Debug)]
pub struct IndexExpression {
    token: Token,
    left: Box<Expression>,
    index: Box<Expression>,
}

impl IndexExpression {
    pub fn new_index_expression(token: Token, left: Expression, index: Expression) -> Expression {
        Expression::new(ExpressionKind::Index(IndexExpression {
            token,
            left: Box::new(left),
            index: Box::new(index),
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn index(&self) -> &Expression {
        &self.index
    }
}

impl Display for IndexExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}[{}])", self.left, self.index)
    }
}
