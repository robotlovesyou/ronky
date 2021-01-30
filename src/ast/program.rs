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
        let mut buffer = Vec::new();
        for stmt in self.statements.iter() {
            buffer.push(format!("{}", stmt));
        }
        write!(f, "{}", buffer.join("\n"))
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self.statements
            .first()
            .map_or_else(|| "".to_string(), |statement| statement.token_literal())
    }
}
