use crate::ast::{Node, Statement};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Clone)]
pub struct Program {
    statements: Vec<Statement>,
}

impl Program {
    pub fn statements(&self) -> &[Statement] {
        &self.statements
    }
}

impl Program {
    pub fn new(statements: Vec<Statement>) -> Program {
        Program { statements }
    }
}

impl Display for Program {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if !self.statements.is_empty() {
            for stmt in self.statements.iter().take(self.statements.len() - 1) {
                write!(f, "{}\n", stmt)?;
            }
            write!(f, "{}", self.statements.last().expect("no last statement"))?;
        }

        Ok(())
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .first()
            .map_or_else(|| "".to_string(), |statement| statement.token_literal())
    }
}
