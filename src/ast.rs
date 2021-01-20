use crate::location::Location;
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
        let mut buffer = Vec::new();
        for stmt in self.statements.iter() {
            buffer.push(format!("\t{}", stmt));
        }
        write!(f, "{{\n{}\n}}", buffer.join("\n"))
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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
    Boolean(BooleanExpression),
    Identifier(IdentifierExpression),
    IntegerLiteral(IntegerLiteralExpression),
    Prefix(PrefixExpression),
    Infix(InfixExpression),
    If(IfExpression),
    FunctionLiteral(FunctionLiteralExpression),
    Call(CallExpression),
}

impl ExpressionKind {
    pub fn token(&self) -> &Token {
        use self::ExpressionKind::*;
        match self {
            Identifier(identifier_expression) => identifier_expression.token(),
            IntegerLiteral(integer_literal) => integer_literal.token(),
            Prefix(prefix_expression) => prefix_expression.token(),
            Infix(infix_expression) => infix_expression.token(),
            Boolean(boolean_expression) => boolean_expression.token(),
            If(if_expression) => if_expression.token(),
            FunctionLiteral(function_literal_expression) => function_literal_expression.token(),
            Call(call_expression) => call_expression.token(),
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
        }
    }
}

#[derive(Debug)]
pub struct IfExpression {
    token: Token,
    condition: Box<Expression>,
    consequence: Statement,
    alternative: Option<Statement>,
}

impl IfExpression {
    pub fn new_if_expression(
        token: Token,
        condition: Expression,
        consequence: Statement,
        alternative: Option<Statement>,
    ) -> Expression {
        Expression::new(ExpressionKind::If(IfExpression {
            token,
            condition: Box::new(condition),
            consequence,
            alternative,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn condition(&self) -> &Expression {
        &self.condition
    }

    pub fn consequence(&self) -> &Statement {
        &self.consequence
    }

    pub fn alternative(&self) -> &Option<Statement> {
        &self.alternative
    }
}

impl Display for IfExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{} ({}) {})",
            self.token, self.condition, self.consequence
        )
        .and_then(|_| {
            if let Some(ref alt) = self.alternative {
                write!(f, "else {}", alt)
            } else {
                Ok(())
            }
        })
    }
}

#[derive(Debug)]
pub struct FunctionLiteralExpression {
    token: Token,
    parameters: Vec<Identifier>,
    body: Statement,
}

impl FunctionLiteralExpression {
    pub fn new_function_literal_expression(
        token: Token,
        parameters: Vec<Identifier>,
        body: Statement,
    ) -> Expression {
        Expression::new(ExpressionKind::FunctionLiteral(FunctionLiteralExpression {
            token,
            parameters,
            body,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn parameters(&self) -> &[Identifier] {
        &self.parameters
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }
}

impl Display for FunctionLiteralExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let param_list = self
            .parameters
            .iter()
            .map(|p| p.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{} ({}){}", self.token, param_list, self.body)
    }
}

#[derive(Debug)]
pub struct IdentifierExpression {
    identifier: Identifier,
}

impl IdentifierExpression {
    pub fn new_identifier_expression(identifier: Identifier) -> Expression {
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
    pub fn new_integer_literal_expression(token: Token, value: i64) -> Expression {
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
pub struct BooleanExpression {
    token: Token,
}

impl BooleanExpression {
    pub fn new_boolean_expression(token: Token) -> Expression {
        Expression::new(ExpressionKind::Boolean(BooleanExpression { token }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn value(&self) -> bool {
        matches!(self.token.kind, Kind::True)
    }
}

impl Display for BooleanExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        self.token().fmt(f)
    }
}

#[derive(Debug)]
pub struct CallExpression {
    token: Token,
    function: Box<Expression>,
    arguments: Vec<Expression>,
}

impl CallExpression {
    pub fn new_call_expression(
        token: Token,
        function: Expression,
        arguments: Vec<Expression>,
    ) -> Expression {
        Expression::new(ExpressionKind::Call(CallExpression {
            token,
            function: Box::new(function),
            arguments,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.token
    }

    pub fn function(&self) -> &Expression {
        &self.function
    }

    pub fn arguments(&self) -> &[Expression] {
        &self.arguments
    }
}

impl Display for CallExpression {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let call_arguments = self
            .arguments()
            .iter()
            .map(|a| a.to_string())
            .collect::<Vec<String>>();
        write!(f, "{}({})", self.function, call_arguments.join(", "))
    }
}

#[derive(Debug)]
pub struct PrefixExpression {
    token: Token,
    operator: PrefixOperator,
    right: Box<Expression>,
}

impl PrefixExpression {
    pub fn new_prefix_expression(
        token: Token,
        operator: PrefixOperator,
        right_expression: Expression,
    ) -> Expression {
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

    pub fn right(&self) -> &Expression {
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
    pub fn new_infix_expression(
        left: Expression,
        right: Expression,
        operator: InfixOperator,
    ) -> Expression {
        Expression::new(ExpressionKind::Infix(InfixExpression {
            left: Box::new(left),
            right: Box::new(right),
            operator,
        }))
    }

    pub fn token(&self) -> &Token {
        &self.left.token()
    }

    pub fn left(&self) -> &Expression {
        &self.left
    }

    pub fn right(&self) -> &Expression {
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

#[derive(Debug)]
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

#[derive(Debug)]
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
