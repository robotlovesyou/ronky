use crate::ast::{Expression, Identifier, Statement, StatementKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression,
}

impl LetStatement {
    pub fn new_let_statement(token: Token, name: Identifier, value: Expression) -> Statement {
        Statement::new(StatementKind::Let(LetStatement { token, name, value }))
    }

    pub fn name(&self) -> &Identifier {
        &self.name
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> &Expression {
        &self.value
    }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} {} = {}", self.token, self.name, self.value)
    }
}
