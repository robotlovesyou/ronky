use crate::ast::{Expression, ExpressionKind, PrefixOperator};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct PrefixExpression {
    token: Token,
    operator: PrefixOperator,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new_prefix_expression(
        token: Token,
        operator: PrefixOperator,
        right_expression: Expression,
    ) -> Expression {
        Expression::new(ExpressionKind::Prefix(PrefixExpression {
            token,
            operator,
            right: Box::new(right_expression),
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn operator(&self) -> PrefixOperator {
        self.operator
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}
