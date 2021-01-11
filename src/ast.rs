use crate::token::{Token, Kind};

trait Node {
    fn token_literal(&self) -> String;
}

pub struct Program {
    statements: Vec<Statement>,
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

struct Statement {
    kind: StatementKind
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        match &self.kind {
            StatementKind::Let(ls) => ls.to_string()
        }
    }
}

enum StatementKind {
    Let(LetStatement)
}

struct Expression {

}

impl Node for Expression {
    fn token_literal(&self) -> String {
        unimplemented!()
    }
}

struct Identifier {
    token: Token,
}

impl Identifier {
    fn new(token: Token) -> Identifier {
        match token.kind {
            Kind::Ident(_) => Identifier{token},
            other => panic!("{:?} is not an Ident token", other),
        }
    }

    fn name(&self) -> &str {
        match &self.token.kind {
            Kind::Ident(name) => name.as_str(),
            _ => unreachable!(),
        }
    }
}

enum ExpressionKind {
}

struct LetStatement {
    token: Token,
    name: Identifier,
    value: Expression
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

