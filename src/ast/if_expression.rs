use crate::ast::{Expression, ExpressionKind, Statement};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: Statement,
    alternative: Option<Statement>,
}

impl IfExpression {
    pub fn new_if_expression(
        token: Token,
        condition: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Expression {
        Expression::new(ExpressionKind::If(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn consequence(&self) -> &Statement {
        &self.consequence
    }

    pub fn alternative(&self) -> &Option<Statement> {
        &self.alternative
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({}) {})",
            self.token, self.condition, self.consequence
        )
        .and_then(|_| {
            if let Some(ref alt) = self.alternative {
                write!(f, "else {}", alt)
            } else {
                Ok(())
            }
        })
    }
}
