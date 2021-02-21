use crate::ast::{Statement, StatementKind};
use crate::token::Token;
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct BlockStatement {
    token: Token,
    statements: Vec<Statement>,
}

impl BlockStatement {
    pub fn new_block_statement(token: Token, statements: Vec<Statement>) -> Statement {
        Statement::new(StatementKind::Block(BlockStatement { token, statements }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

impl Display for BlockStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{\n")?;
        if !self.statements.is_empty() {
            for st in self.statements.iter() {
                write!(f, "\t{}\n", st)?;
            }
        }
        write!(f, "}}")
    }
}
