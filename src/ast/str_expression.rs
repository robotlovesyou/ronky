use crate::ast::{Expression, ExpressionKind};
use crate::token::{Kind, Token};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct StrExpression {
    token: Token,
}

impl Display for StrExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token.kind)
    }
}

impl StrExpression {
    pub fn new_str_expression(token: Token) -> Expression {
        Expression::new(ExpressionKind::Str(StrExpression { token }))
    }
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> &str {
        match &self.token.kind {
            Kind::Str(repr) => &repr,
            other => panic!("Str token is a {:?} not a Kind::Str", other),
        }
    }
}
