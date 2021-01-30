use crate::ast::{Expression, ExpressionKind};
use crate::token::{Kind, Token};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct BooleanExpression {
    token: Token,
}

impl BooleanExpression {
    pub fn new_boolean_expression(token: Token) -> Expression {
        Expression::new(ExpressionKind::Boolean(BooleanExpression { token }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> bool {
        matches!(self.token.kind, Kind::True)
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.token().fmt(f)
    }
}
