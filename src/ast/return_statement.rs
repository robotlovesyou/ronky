use crate::ast::{Expression, Statement, StatementKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    token: Token,
    value: Expression,
}

impl ReturnStatement {
    pub fn new_return_statement(token: Token, value: Expression) -> Statement {
        Statement::new(StatementKind::Return(ReturnStatement { token, value }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {};", self.token, self.value)
    }
}
