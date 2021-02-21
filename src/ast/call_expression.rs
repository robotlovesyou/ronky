use crate::ast::{Expression, ExpressionKind};
use crate::display::display_parameter_list;
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct CallExpression {
    token: Token,
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new_call_expression(
        token: Token,
        function: Expression,
        arguments: Vec<Expression>,
    ) -> Expression {
        Expression::new(ExpressionKind::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn function(&self) -> &Expression {
        &self.function
    }

    pub fn arguments(&self) -> &[Expression] {
        &self.arguments
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.function)?;
        display_parameter_list(&self.arguments, f)
    }
}
