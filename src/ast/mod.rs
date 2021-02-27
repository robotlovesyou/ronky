use crate::location::Location;
use crate::token::{Kind, Token};
use std::fmt::{self, Display, Formatter};

pub use array_literal::*;
pub use block_statement::*;
pub use boolean_expression::*;
pub use call_expression::*;
pub use expression_statement::*;
pub use function_literal::*;
pub use identifier_expression::*;
pub use if_expression::*;
pub use index_expression::*;
pub use infix_expression::*;
pub use integer_literal::*;
pub use let_statement::*;
pub use operator::*;
pub use prefix_expression::*;
pub use program::*;
pub use return_statement::*;
pub use str_expression::*;

mod array_literal;
mod block_statement;
mod boolean_expression;
mod call_expression;
mod expression_statement;
mod function_literal;
mod identifier_expression;
mod if_expression;
mod index_expression;
mod infix_expression;
mod integer_literal;
mod let_statement;
mod operator;
mod prefix_expression;
mod program;
mod return_statement;
mod str_expression;

trait Node: Display {
    fn token_literal(&self) -> String;
}

#[derive(Debug, Clone)]
pub struct Statement {
    kind: Box<StatementKind>,
}

impl Statement {
    pub fn new(kind: StatementKind) -> Statement {
        Statement {
            kind: Box::new(kind),
        }
    }

    pub fn token(&self) -> &Token {
        &self.kind.token()
    }

    pub fn kind(&self) -> &StatementKind {
        &self.kind
    }

    pub fn location(&self) -> Location {
        self.token().location
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

impl Node for Statement {
    fn token_literal(&self) -> String {
        self.token().to_string()
    }
}

#[derive(Debug, Clone)]
pub enum StatementKind {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
    Block(BlockStatement),
}

impl StatementKind {
    pub fn token(&self) -> &Token {
        use self::StatementKind::*;
        match self {
            Let(let_statement) => let_statement.token(),
            Return(return_statement) => return_statement.token(),
            Expression(expression_statement) => expression_statement.token(),
            Block(block_statement) => block_statement.token(),
        }
    }
}

impl Display for StatementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::StatementKind::*;
        match self {
            Let(let_statement) => let_statement.fmt(f),
            Return(return_statement) => return_statement.fmt(f),
            Expression(expression_statement) => expression_statement.fmt(f),
            Block(block_statement) => block_statement.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    kind: ExpressionKind,
}

impl Expression {
    pub fn new(kind: ExpressionKind) -> Expression {
        Expression { kind }
    }

    pub fn kind(&self) -> &ExpressionKind {
        &self.kind
    }

    pub fn token(&self) -> &Token {
        &self.kind.token()
    }

    pub fn location(&self) -> Location {
        self.token().location
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.kind.fmt(f)
    }
}

impl Node for Expression {
    fn token_literal(&self) -> String {
        unimplemented!()
    }
}

#[derive(Debug, Clone)]
pub enum ExpressionKind {
    Boolean(BooleanExpression),
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    FunctionLiteral(FunctionLiteralExpression),
    Call(CallExpression),
    Str(StrExpression),
    Array(ArrayLiteral),
    Index(IndexExpression),
}

impl ExpressionKind {
    pub fn token(&self) -> &Token {
        use self::ExpressionKind::*;
        match self {
            Identifier(kind) => kind.token(),
            IntegerLiteral(kind) => kind.token(),
            Prefix(kind) => kind.token(),
            Infix(kind) => kind.token(),
            Boolean(kind) => kind.token(),
            If(kind) => kind.token(),
            FunctionLiteral(kind) => kind.token(),
            Call(kind) => kind.token(),
            Str(kind) => kind.token(),
            Array(kind) => kind.token(),
            Index(kind) => kind.token(),
        }
    }
}

impl Display for ExpressionKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::ExpressionKind::*;
        match self {
            Identifier(kind) => kind.fmt(f),
            IntegerLiteral(kind) => kind.fmt(f),
            Prefix(kind) => kind.fmt(f),
            Infix(kind) => kind.fmt(f),
            Boolean(kind) => kind.fmt(f),
            If(kind) => kind.fmt(f),
            FunctionLiteral(kind) => kind.fmt(f),
            Call(kind) => kind.fmt(f),
            Str(kind) => kind.fmt(f),
            Array(kind) => kind.fmt(f),
            Index(kind) => kind.fmt(f),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Identifier {
    token: Token,
}

impl Identifier {
    pub fn new(token: Token) -> Identifier {
        match token.kind {
            Kind::Ident(_) => Identifier { token },
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

impl Display for Identifier {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.token.fmt(f)
    }
}
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[should_panic]
    fn panics_constructing_an_identifier_with_a_non_ident_token() {
        Identifier::new(Token::new(Location::default(), Kind::RParen));
    }

    #[test]
    fn can_construct_an_identifier() {
        Identifier::new(Token::new(
            Location::default(),
            Kind::Ident("abc".to_string()),
        ));
    }
}
