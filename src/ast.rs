use crate::token::{Kind, Token};
use std::fmt::{self, Display, Formatter};

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum PrefixOperator {
    Minus,
    Not,
}

impl Display for PrefixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::PrefixOperator::*;
        let repr = match self {
            Minus => Kind::Minus,
            Not => Kind::Bang,
        };
        write!(f, "{}", repr)
    }
}

#[derive(Debug, Eq, PartialEq, Copy, Clone)]
pub enum InfixOperator {
    Add,
    Subtract,
    Multiply,
    Divide,
    GreaterThan,
    LessThan,
    Equals,
    NotEquals,
}

impl Display for InfixOperator {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::InfixOperator::*;
        let repr = match self {
            Add => Kind::Plus,
            Subtract => Kind::Minus,
            Multiply => Kind::Asterisk,
            Divide => Kind::Slash,
            GreaterThan => Kind::GT,
            LessThan => Kind::LT,
            Equals => Kind::EQ,
            NotEquals => Kind::NotEQ,
        };
        write!(f, "{}", repr)
    }
}

trait Node: Display {
    fn token_literal(&self) -> String;
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct Statement {
    kind: StatementKind,
}

impl Statement {
    pub fn new(kind: StatementKind) -> Statement {
        Statement { kind }
    }
    pub fn token(&self) -> &Token {
        &self.kind.token()
    }

    pub fn kind(&self) -> &StatementKind {
        &self.kind
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

#[derive(Debug)]
pub enum StatementKind {
    Let(LetStatement),
    Return(ReturnStatement),
    Expression(ExpressionStatement),
}

impl StatementKind {
    pub fn token(&self) -> &Token {
        use self::StatementKind::*;
        match self {
            Let(let_statement) => let_statement.token(),
            Return(return_statement) => return_statement.token(),
            Expression(expression_statement) => expression_statement.token(),
        }
    }
}

impl Display for StatementKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        use self::StatementKind::*;
        match self {
            Let(ref let_statement) => let_statement.fmt(f),
            Return(ref return_statement) => return_statement.fmt(f),
            Expression(ref expression_statement) => expression_statement.fmt(f),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ExpressionKind {
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
}

impl ExpressionKind {
    pub fn token(&self) -> &Token {
        use self::ExpressionKind::*;
        match self {
            Identifier(identifier_expression) => identifier_expression.token(),
            IntegerLiteral(integer_literal) => integer_literal.token(),
            Prefix(prefix_expression) => prefix_expression.token(),
            Infix(infix_expression) => infix_expression.token(),
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
        }
    }
}

#[derive(Debug)]
pub struct IdentifierExpression {
    identifier: Identifier,
}

impl IdentifierExpression {
    pub fn new(identifier: Identifier) -> Expression {
        Expression::new(ExpressionKind::Identifier(IdentifierExpression {
            identifier,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.identifier.token
    }
}

impl Display for IdentifierExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.identifier.token)
    }
}

#[derive(Debug)]
pub struct IntegerLiteralExpression {
    token: Token,
    value: i64,
}

impl IntegerLiteralExpression {
    pub fn new(token: Token, value: i64) -> Expression {
        Expression::new(ExpressionKind::IntegerLiteral(IntegerLiteralExpression {
            token,
            value,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> i64 {
        self.value
    }
}

impl Display for IntegerLiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    token: Token,
    operator: PrefixOperator,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new(token: Token, operator: PrefixOperator, right_expression: Expression) -> Expression {
        Expression::new(ExpressionKind::Prefix(PrefixExpression {
            token,
            operator,
            right: Box::new(right_expression),
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn operator(&self) -> PrefixOperator {
        self.operator
    }

    pub fn right(&self) -> &Box<Expression> {
        &self.right
    }
}

impl Display for PrefixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}{})", self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct InfixExpression {
    left: Box<Expression>,
    right: Box<Expression>,
    operator: InfixOperator,
}

impl InfixExpression {
    pub fn new(left: Expression, right: Expression, operator: InfixOperator) -> Expression {
        Expression::new(ExpressionKind::Infix(InfixExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.left.token()
    }

    pub fn left(&self) -> &Box<Expression> {
        &self.left
    }

    pub fn right(&self) -> &Box<Expression> {
        &self.right
    }

    pub fn operator(&self) -> InfixOperator {
        self.operator
    }
}

impl Display for InfixExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator, self.right)
    }
}

#[derive(Debug)]
pub struct ExpressionStatement {
    expression: Expression,
}

impl ExpressionStatement {
    pub fn new(expression: Expression) -> Statement {
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

#[derive(Debug)]
pub struct LetStatement {
    token: Token,
    name: Identifier,
    //value: Expression
}

impl LetStatement {
    pub fn new(token: Token, name: Identifier) -> Statement {
        Statement::new(StatementKind::Let(LetStatement { token, name }))
    }

    pub fn name(&self) -> &Identifier {
        &self.name
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    // pub fn value(&self) -> &Expression {
    //     &self.value
    // }
}

impl Display for LetStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{} = //expression goes here", self.name)
    }
}

#[derive(Debug)]
pub struct ReturnStatement {
    token: Token, //value: Expression
}

impl ReturnStatement {
    pub fn new(token: Token) -> Statement {
        Statement::new(StatementKind::Return(ReturnStatement { token }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }
}

impl Display for ReturnStatement {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "//expression goes here")
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
