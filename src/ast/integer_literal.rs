use crate::ast::{Expression, ExpressionKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct IntegerLiteralExpression {
    token: Token,
    value: i64,
}

impl IntegerLiteralExpression {
    pub fn new_integer_literal_expression(token: Token, value: i64) -> Expression {
        Expression::new(ExpressionKind::IntegerLiteral(IntegerLiteralExpression {
            token,
            value,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> i64 {
        self.value
    }
}

impl Display for IntegerLiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
