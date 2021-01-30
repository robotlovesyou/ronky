use crate::ast::{Expression, ExpressionKind, InfixOperator};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct InfixExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: InfixOperator,
}

impl InfixExpression {
    pub fn new_infix_expression(
        left: Expression,
        right: Expression,
        operator: InfixOperator,
    ) -> Expression {
        Expression::new(ExpressionKind::Infix(InfixExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.left.token()
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn right(&self) -> &Expression {
        &self.right
    }

    pub fn operator(&self) -> InfixOperator {
        self.operator
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}
