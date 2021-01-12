use crate::token::{Token, Kind};

trait Node {
    fn token_literal(&self) -> String;
}

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
        Program{statements}
    }
}

impl Node for Program {
    fn token_literal(&self) -> String {
        self
            .statements
            .first()
            .map_or_else(
                || "".to_string(),
                |statement| statement.token_literal())
    }
}

pub struct Statement {
    kind: StatementKind
}

impl Statement {
    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match &self.kind {
            StatementKind::Let(ls) => ls.to_string()
        }
    }
}

pub enum StatementKind {
    Let(LetStatement)
}

pub struct Expression {

}

impl Node for Expression {
    fn token_literal(&self) -> String {
        unimplemented!()
    }
}

pub struct Identifier {
    token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Identifier {
        match token.kind {
            Kind::Ident(_) => Identifier{token},
            other => panic!("{:?} is not an Ident token", other),
        }
    }

    pub fn name(&self) -> &str {
        match &self.token.kind {
            Kind::Ident(name) => name.as_str(),
            _ => unreachable!(),
        }
    }
}

pub enum ExpressionKind {
}

pub struct LetStatement {
    token: Token,
    name: Identifier,
    //value: Expression
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> Statement {
        Statement{
            kind: StatementKind::Let(LetStatement{
                token,
                name,
            })
        }
    }
    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn name(&self) -> &Identifier {
        &self.name
    }

    // pub fn value(&self) -> &Expression {
    //     &self.value
    // }
}

impl std::string::ToString for LetStatement {
    fn to_string(&self) -> String {
        self.token.to_string()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[test]
    #[should_panic]
    fn panics_constructing_an_identifier_with_a_non_ident_token() {
        Identifier::new(Token::new(0, 0, Kind::RParen));
    }

    #[test]
    fn can_construct_an_identifier() {
        Identifier::new(Token::new(0, 0, Kind::Ident("abc".to_string())));
    }
}

