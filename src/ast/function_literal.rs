use crate::ast::{Expression, ExpressionKind, Identifier, Statement};
use crate::display::display_parameter_list;
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct FunctionLiteralExpression {
    token: Token,
    parameters: Vec<Identifier>,
    body: Statement,
}

impl FunctionLiteralExpression {
    pub fn new_function_literal_expression(
        token: Token,
        parameters: Vec<Identifier>,
        body: Statement,
    ) -> Expression {
        Expression::new(ExpressionKind::FunctionLiteral(FunctionLiteralExpression {
            token,
            parameters,
            body,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn parameters(&self) -> &[Identifier] {
        &self.parameters
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Display for FunctionLiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.token)?;
        display_parameter_list(&self.parameters, f)?;
        write!(f, "{}", self.body)
    }
}
