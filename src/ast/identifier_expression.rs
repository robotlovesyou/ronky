use crate::ast::{Expression, ExpressionKind, Identifier};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct IdentifierExpression {
    identifier: Identifier,
}

impl IdentifierExpression {
    pub fn new_identifier_expression(identifier: Identifier) -> Expression {
        Expression::new(ExpressionKind::Identifier(IdentifierExpression {
            identifier,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.identifier.token
    }

    pub fn name(&self) -> &str {
        &self.identifier.name()
    }
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.identifier.token)
    }
}
