use crate::ast::{Expression, Statement, StatementKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    expression: Expression,
}

impl ExpressionStatement {
    pub fn new_expression_statement(expression: Expression) -> Statement {
        Statement::new(StatementKind::Expression(ExpressionStatement {
            expression,
        }))
    }

    pub fn expression(&self) -> &Expression {
        &self.expression
    }

    pub fn token(&self) -> &Token {
        self.expression.token()
    }
}

impl Display for ExpressionStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.expression.fmt(f)
    }
}
