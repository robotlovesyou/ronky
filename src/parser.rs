use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use std::iter::Peekable;
use std::rc::Rc;
use std::result;

const PARSING_A_LET_STATEMENT: &str = "parsing a let statement";
const PARSING_A_PREFIX_EXPRESSION: &str = "parsing a prefix expression";
const PARSING_AN_INFIX_EXPRESSION: &str = "parsing a infix expression";
const PARSING_A_GROUPED_EXPRESSION: &str = "parsing a grouped expression";
const PARSING_AN_IF_EXPRESSION: &str = "parsing an if expression";
const PARSING_A_PARAMETER_LIST: &str = "parsing a parameter list";
const PARSING_A_FUNCTION_LITERAL: &str = "parsing a function literal";
const PARSING_A_RETURN_STATEMENT: &str = "parsing a return statement";

#[derive(Debug, Eq, PartialEq, Ord, PartialOrd)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

trait ErrorMessage {
    fn message(&self) -> String;
}

#[derive(Debug)]
pub struct ParseError {
    errors: Vec<Error>,
}

impl ParseError {
    fn new_parser_error(errors: Vec<Error>) -> Error {
        Error {
            kind: ErrorKind::Parse(ParseError { errors }),
        }
    }
}

impl ErrorMessage for &ParseError {
    fn message(&self) -> String {
        let mut messages = Vec::new();
        for err in self.errors.iter() {
            messages.push(format!("{}", err));
        }
        messages.join("\n\n")
    }
}

#[derive(Debug)]
pub struct StatementError {
    message: String,
}

impl ErrorMessage for &StatementError {
    fn message(&self) -> String {
        self.message.clone()
    }
}

impl StatementError {
    fn unexpected_token(expected: &str, got: &Token, action: &str) -> Error {
        Error {
            kind: ErrorKind::Statement(StatementError {
                message: format!(
                    "got an {} instead of an {} while {} at line {} column {}",
                    got.kind.tag(),
                    expected,
                    action,
                    got.line,
                    got.column,
                ),
            }),
        }
    }

    fn unexpected_eof(action: &str) -> Error {
        Error {
            kind: ErrorKind::Statement(StatementError {
                message: format!("reached the end of the source while {}", action),
            }),
        }
    }
}

#[derive(Debug)]
pub struct ExpressionError {
    message: String,
}

impl ErrorMessage for &ExpressionError {
    fn message(&self) -> String {
        self.message.clone()
    }
}

impl ExpressionError {
    pub fn no_parse_fn(token: &Token) -> Error {
        Error {
            kind: ErrorKind::Expression(ExpressionError {
                message: format!(
                    "unexpected '{}' at line {} column {}",
                    token, token.line, token.column
                ),
            }),
        }
    }
}

impl Display for dyn ErrorMessage {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.message())
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Parse(ParseError),
    Statement(StatementError),
    Expression(ExpressionError),
}

fn display_error_message(f: &mut Formatter<'_>, error_message: impl ErrorMessage) -> fmt::Result {
    write!(f, "{}", error_message.message())
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match self {
            ErrorKind::Parse(error) => display_error_message(f, error),
            ErrorKind::Statement(error) => display_error_message(f, error),
            ErrorKind::Expression(error) => display_error_message(f, error),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub type Result<T> = result::Result<T, Error>;

type ParseInfixFn = Rc<dyn Fn(&mut Parser, Expression, Token) -> Result<Expression>>;
type ParsePrefixFn = Rc<dyn Fn(&mut Parser, Token) -> Result<Expression>>;

pub struct Parser {
    lexer: Peekable<Lexer>,
    prefix_parse_fns: HashMap<Tag, ParsePrefixFn>,
    infix_parse_fns: HashMap<Tag, ParseInfixFn>,
}

fn operator_precedence(token: &Token) -> Precedence {
    use crate::token::Tag::*;
    use Precedence::*;

    match token.kind.tag() {
        Plus => Sum,
        Minus => Sum,
        Asterisk => Product,
        Slash => Product,
        LT => LessGreater,
        GT => LessGreater,
        EQ => Equals,
        NotEQ => Equals,
        LParen => Call,
        _ => Lowest,
    }
}

impl Parser {
    pub fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser {
            lexer: lexer.peekable(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };

        parser.register_prefix_fn(Tag::Ident, |parser: &mut Parser, token: Token| {
            parser.parse_identifier_expression(token)
        });

        parser.register_prefix_fn(Tag::Int, |parser: &mut Parser, token: Token| {
            parser.parse_integer_literal_expression(token)
        });

        parser.register_prefix_fn(Tag::Minus, |parser: &mut Parser, token: Token| {
            parser.parse_prefix_expression(token)
        });

        parser.register_prefix_fn(Tag::Bang, |parser: &mut Parser, token: Token| {
            parser.parse_prefix_expression(token)
        });

        parser.register_prefix_fn(Tag::True, |parser: &mut Parser, token: Token| {
            parser.parse_boolean_literal_expression(token)
        });

        parser.register_prefix_fn(Tag::False, |parser: &mut Parser, token: Token| {
            parser.parse_boolean_literal_expression(token)
        });

        parser.register_prefix_fn(Tag::LParen, |parser: &mut Parser, token: Token| {
            parser.parse_grouped_expression(token)
        });

        parser.register_prefix_fn(Tag::If, |parser: &mut Parser, token: Token| {
            parser.parse_if_expression(token)
        });

        parser.register_prefix_fn(Tag::Function, |parser: &mut Parser, token: Token| {
            parser.parse_function_literal(token)
        });

        parser.register_infix_fn(
            Tag::Plus,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::Minus,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::Asterisk,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::Slash,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::GT,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::LT,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::EQ,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::NotEQ,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_infix_expression(left, token)
            },
        );

        parser.register_infix_fn(
            Tag::LParen,
            |parser: &mut Parser, left: Expression, token: Token| {
                parser.parse_call_expression(left, token)
            },
        );

        parser
    }

    fn register_prefix_fn<F>(&mut self, tag: Tag, f: F)
    where
        F: 'static + Fn(&mut Parser, Token) -> Result<Expression>,
    {
        self.prefix_parse_fns.insert(tag, Rc::new(f));
    }

    fn register_infix_fn<F>(&mut self, tag: Tag, f: F)
    where
        F: 'static + Fn(&mut Parser, Expression, Token) -> Result<Expression>,
    {
        self.infix_parse_fns.insert(tag, Rc::new(f));
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();

        while let Some(token) = self.lexer.next() {
            let next_statement: Result<Statement> = self.parse_statement(token);

            if let Ok(statement) = next_statement {
                statements.push(statement);
            } else {
                errors.push(next_statement.err().expect("not an error"));
            }
        }

        if errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(ParseError::new_parser_error(errors))
        }
    }

    fn parse_statement(&mut self, token: Token) -> Result<Statement> {
        match token.kind {
            Kind::Let => self.parse_let_statement(token),
            Kind::Return => self.parse_return_statement(token),
            _ => self.parse_expression_statement(token),
        }
    }

    fn parse_expression_statement(&mut self, token: Token) -> Result<Statement> {
        self.parse_expression(token, Precedence::Lowest)
            .map(ExpressionStatement::new_expression_statement)
            .map(|expression| {
                self.optional_peek_consume(Tag::Semicolon);
                expression
            })
    }

    fn get_prefix_parse_fn(&mut self, token: &Token) -> Result<ParsePrefixFn> {
        if let Some(f) = self.prefix_parse_fns.get(&token.kind.tag()) {
            Ok(f.clone())
        } else {
            Err(ExpressionError::no_parse_fn(token))
        }
    }

    fn get_infix_parse_fn(&mut self, token: &Token) -> Result<ParseInfixFn> {
        if let Some(f) = self.infix_parse_fns.get(&token.kind.tag()) {
            Ok(f.clone())
        } else {
            Err(ExpressionError::no_parse_fn(token))
        }
    }

    fn parse_expression(&mut self, token: Token, precedence: Precedence) -> Result<Expression> {
        let prefix_f = self.get_prefix_parse_fn(&token)?;

        let mut left = prefix_f(self, token)?;
        while let Some(peek) = self.lexer.peek() {
            if peek.kind.tag() == Tag::Semicolon || precedence >= operator_precedence(peek) {
                break;
            }
            let next_token = self.lexer.next().expect("no token");
            let infix_f = self.get_infix_parse_fn(&next_token)?;

            left = infix_f(self, left, next_token)?;
        }

        Ok(left)
    }

    fn parse_identifier_expression(&mut self, token: Token) -> Result<Expression> {
        Ok(IdentifierExpression::new_identifier_expression(
            Identifier::new(token),
        ))
    }

    fn parse_integer_literal_expression(&mut self, token: Token) -> Result<Expression> {
        let value = match &token.kind {
            // Anything which does not parse as an i64 here would indicate a bug in the
            // lexer so ok to expect
            Kind::Int(repr) => repr.parse::<i64>().expect("not an integer literal"),
            other => panic!("got {:?} expecting an Int", other),
        };

        Ok(IntegerLiteralExpression::new_integer_literal_expression(
            token, value,
        ))
    }

    fn parse_boolean_literal_expression(&mut self, token: Token) -> Result<Expression> {
        Ok(BooleanExpression::new_boolean_expression(token))
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression> {
        let operator = match &token.kind.tag() {
            Tag::Bang => PrefixOperator::Not,
            Tag::Minus => PrefixOperator::Minus,
            _ => {
                return Err(StatementError::unexpected_token(
                    "a prefix operator",
                    &token,
                    PARSING_A_PREFIX_EXPRESSION,
                ))
            }
        };

        let next_token = self.expect_next(PARSING_A_PREFIX_EXPRESSION)?;
        let right = self.parse_expression(next_token, Precedence::Prefix)?;
        Ok(PrefixExpression::new_prefix_expression(
            token, operator, right,
        ))
    }

    fn parse_infix_expression(&mut self, left: Expression, token: Token) -> Result<Expression> {
        use crate::token::Tag::*;
        use InfixOperator::*;
        let operator = match token.kind.tag() {
            Plus => Add,
            Minus => Subtract,
            Asterisk => Multiply,
            Slash => Divide,
            LT => LessThan,
            GT => GreaterThan,
            EQ => Equals,
            NotEQ => NotEquals,
            _ => {
                return Err(StatementError::unexpected_token(
                    "an infix operator",
                    &token,
                    PARSING_AN_INFIX_EXPRESSION,
                ))
            }
        };

        let next_token = self.expect_next(PARSING_AN_INFIX_EXPRESSION)?;
        let right = self.parse_expression(next_token, operator_precedence(&token))?;
        Ok(InfixExpression::new_infix_expression(left, right, operator))
    }

    fn parse_grouped_expression(&mut self, _: Token) -> Result<Expression> {
        let next_token = self.expect_next(PARSING_A_GROUPED_EXPRESSION)?;
        self.parse_expression(next_token, Precedence::Lowest)
            .and_then(|ex| {
                self.expect_peek_consume(Tag::RParen, PARSING_A_GROUPED_EXPRESSION)
                    .map(|_| ex)
            })
    }

    fn parse_if_expression(&mut self, token: Token) -> Result<Expression> {
        self.expect_peek_consume(Tag::LParen, PARSING_AN_IF_EXPRESSION)?;

        let next_token = self.expect_next(PARSING_AN_IF_EXPRESSION)?;
        let condition = self.parse_expression(next_token, Precedence::Lowest)?;

        self.expect_peek_consume(Tag::RParen, PARSING_AN_IF_EXPRESSION)?;
        let next_token = self.expect_peek_consume(Tag::LBrace, PARSING_AN_IF_EXPRESSION)?;
        let consequence = self.parse_block_statement(next_token)?;

        let alternative = if self.optional_peek_consume(Tag::Else).is_some() {
            let next_token = self.expect_peek_consume(Tag::LBrace, PARSING_AN_IF_EXPRESSION)?;
            Some(self.parse_block_statement(next_token)?)
        } else {
            None
        };

        Ok(IfExpression::new_if_expression(
            token,
            condition,
            consequence,
            alternative,
        ))
    }

    fn parse_function_literal(&mut self, token: Token) -> Result<Expression> {
        self.expect_peek_consume(Tag::LParen, PARSING_A_FUNCTION_LITERAL)?;
        let parameters = self.parse_function_parameters()?;
        let next_token = self.expect_peek_consume(Tag::LBrace, PARSING_A_FUNCTION_LITERAL)?;
        let body = self.parse_block_statement(next_token)?;
        Ok(FunctionLiteralExpression::new_function_literal_expression(
            token, parameters, body,
        ))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Identifier>> {
        let mut identifiers = Vec::new();

        while let Some(next_token) = self.lexer.next() {
            if next_token.kind.tag() == Tag::RParen {
                break;
            }

            if next_token.kind.tag() != Tag::Ident {
                return Err(StatementError::unexpected_token(
                    "identifier",
                    &next_token,
                    PARSING_A_PARAMETER_LIST,
                ));
            }
            identifiers.push(Identifier::new(next_token));
            self.optional_peek_consume(Tag::Comma);
            // if let Some(maybe_comma) = self.lexer.peek() {
            //     if maybe_comma.kind.tag() != Tag::Comma && maybe_comma.kind.tag() != Tag::RParen {
            //         return Err(StatementError::unexpected_token(
            //             "comma or right paren",
            //             maybe_comma,
            //             PARSING_A_PARAMETER_LIST,
            //         ));
            //     }
            //     if maybe_comma.kind.tag() == Tag::Comma {
            //         self.expect_next()?;
            //     }
            // }
        }

        Ok(identifiers)
    }

    fn parse_block_statement(&mut self, token: Token) -> Result<Statement> {
        let mut statements = Vec::new();

        while let Some(next_token) = self.lexer.next() {
            if next_token.kind.tag() == Tag::RBrace {
                break;
            }
            statements.push(self.parse_statement(next_token)?);
        }
        Ok(BlockStatement::new_block_statement(token, statements))
    }

    fn parse_call_expression(&mut self, left: Expression, token: Token) -> Result<Expression> {
        let arguments = self.parse_call_arguments()?;
        Ok(CallExpression::new_call_expression(token, left, arguments))
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expression>> {
        let mut arguments = Vec::new();

        while let Some(token) = self.lexer.next() {
            if token.kind.tag() == Tag::RParen {
                break;
            }
            arguments.push(self.parse_expression(token, Precedence::Lowest)?);
            self.optional_peek_consume(Tag::Comma);
        }
        Ok(arguments)
    }

    fn consume_source_line(&mut self) {
        while let Some(next) = self.lexer.next() {
            if next.kind.tag() == Tag::Semicolon {
                break;
            }
        }
    }

    fn try_parse_let_statement(&mut self, token: Token) -> Result<Statement> {
        let ident_token = self.expect_peek_consume(Tag::Ident, PARSING_A_LET_STATEMENT)?;
        let identifier = Identifier::new(ident_token);

        self.expect_peek_consume(Tag::Assign, PARSING_A_LET_STATEMENT)?;
        let next_token = self.expect_next(PARSING_A_LET_STATEMENT)?;
        let value = self.parse_expression(next_token, Precedence::Lowest)?;
        self.expect_peek_consume(Tag::Semicolon, PARSING_A_LET_STATEMENT)?;
        Ok(LetStatement::new_let_statement(token, identifier, value))
    }

    fn parse_let_statement(&mut self, token: Token) -> Result<Statement> {
        match self.try_parse_let_statement(token) {
            Ok(let_statement) => Ok(let_statement),
            Err(e) => {
                self.consume_source_line();
                Err(e)
            }
        }
    }

    fn try_parse_return_statement(&mut self, token: Token) -> Result<Statement> {
        let next_token = self.expect_next(PARSING_A_RETURN_STATEMENT)?;
        let value = self.parse_expression(next_token, Precedence::Lowest)?;
        self.expect_peek_consume(Tag::Semicolon, PARSING_A_RETURN_STATEMENT)?;
        Ok(ReturnStatement::new_return_statement(token, value))
    }

    fn parse_return_statement(&mut self, token: Token) -> Result<Statement> {
        match self.try_parse_return_statement(token) {
            Ok(return_statement) => Ok(return_statement),
            Err(e) => {
                self.consume_source_line();
                Err(e)
            }
        }
    }

    fn expect_peek_consume(&mut self, tag: Tag, action: &str) -> Result<Token> {
        match self.lexer.peek() {
            Some(token) if token.kind.tag() == tag => Ok(self.lexer.next().expect("no token")),
            Some(token) => Err(StatementError::unexpected_token(
                &tag.to_string(),
                token,
                action,
            )),
            None => Err(StatementError::unexpected_eof(action)),
        }
    }

    fn optional_peek_consume(&mut self, tag: Tag) -> Option<Token> {
        let peeked = self.lexer.peek();
        match peeked {
            Some(token) if token.kind.tag() == tag => Some(self.lexer.next().expect("no token")),
            _ => None,
        }
    }

    fn expect_next(&mut self, action: &str) -> Result<Token> {
        match self.lexer.next() {
            None => Err(StatementError::unexpected_eof(action)),
            Some(token) => Ok(token),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::*;
    use indoc::indoc;

    #[derive(Debug, Copy, Clone)]
    enum Operand {
        Integer(i64),
        Identifier(&'static str),
        Boolean(bool),
    }

    fn test_integer_literal(integer_literal: &IntegerLiteralExpression, expected: i64) {
        assert_eq!(expected, integer_literal.value());
        assert_eq!(format!("{}", expected), integer_literal.to_string());
    }

    fn test_identifier(identifier_expression: &IdentifierExpression, expected: &str) {
        assert_eq!(expected, identifier_expression.to_string().as_str());
    }

    fn test_boolean_literal(boolean_expression: &BooleanExpression, expected: bool) {
        assert_eq!(boolean_expression.value(), expected);
    }

    fn test_literal_expression(expression: &Expression, expected: Operand) {
        match (expression.kind(), expected) {
            (ExpressionKind::IntegerLiteral(integer), Operand::Integer(expected_int)) => {
                test_integer_literal(integer, expected_int)
            }
            (ExpressionKind::Identifier(identifier), Operand::Identifier(expected_ident)) => {
                test_identifier(identifier, expected_ident)
            }
            (ExpressionKind::Boolean(boolean), Operand::Boolean(b)) => {
                test_boolean_literal(boolean, b)
            }
            (other_kind, other_operand) => panic!(
                "non matching expression kind {:?} and operand {:?}",
                other_kind, other_operand
            ),
        }
    }

    fn test_infix_expression(
        expression: &Expression,
        left: Operand,
        right: Operand,
        operator: InfixOperator,
    ) {
        match expression.kind() {
            ExpressionKind::Infix(infix) => {
                test_literal_expression(infix.left(), left);
                test_literal_expression(infix.right(), right);
                assert_eq!(operator, infix.operator());
            }
            other => panic!("got {:?} expecting an infix expression", other),
        }
    }

    fn test_block_statement<T: Fn(&BlockStatement) -> ()>(statement: &Statement, t: T) {
        match statement.kind() {
            StatementKind::Block(block_statement) => t(block_statement),
            other => panic!("got {:?} expecting a block statement", other),
        }
    }

    fn parser_from_source(source: &str) -> Parser {
        Parser::new(source.to_source().into_tokens())
    }

    #[test]
    fn can_parse_a_program_with_only_let_statements() -> Result<()> {
        let tests = vec![
            ("let x = 5;", "x", Operand::Integer(5)),
            ("let y = true;", "y", Operand::Boolean(true)),
            ("let foobar = y;", "foobar", Operand::Identifier("y")),
        ];

        for (source, identifier, value) in tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;
            assert_eq!(1, program.statements().len());
            match program.statements().first().unwrap().kind() {
                StatementKind::Let(let_statement) => {
                    assert_eq!(identifier, let_statement.name().name());
                    test_literal_expression(let_statement.value(), value);
                }
                other => panic!("got {:?} expecting a let statement", other),
            }
        }
        Ok(())
    }

    #[test]
    fn raises_an_error_when_parsing_an_invalid_let_statement() {
        let source = indoc! {"
        let x = 5;
        let y = 10;
        let 838383;
        "};

        let mut parser = parser_from_source(source);
        let result = parser.parse();
        assert!(result.is_err());
        match result.err().unwrap().kind {
            ErrorKind::Parse(parse_error) => assert!(!parse_error.errors.is_empty()),
            other => panic!("expected an ErrorKind::ParseError but got {:?}", other),
        }
    }

    #[test]
    fn can_parse_a_program_with_only_return_statements() -> Result<()> {
        let tests = vec![
            ("return 5;", Operand::Integer(5)),
            ("return true;", Operand::Boolean(true)),
            ("return foobar;", Operand::Identifier("foobar")),
        ];

        for (source, value) in tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;
            assert_eq!(1, program.statements().len());
            match program.statements().first().unwrap().kind() {
                StatementKind::Return(return_statement) => {
                    assert_eq!(Tag::Return, return_statement.token().kind.tag());
                    test_literal_expression(return_statement.value(), value);
                }
                other => panic!("got {:?} expecting a return statement", other),
            }
        }
        Ok(())
    }

    fn test_statement_as_expression_statement<T>(statement: &Statement, t: T)
    where
        T: Fn(&Expression) -> (),
    {
        match statement.kind() {
            StatementKind::Expression(expression_statement) => t(expression_statement.expression()),
            other => panic!("got {} when expecting an ExpressionStatement", other),
        }
    }

    #[test]
    fn can_parse_an_identifier_expression() -> Result<()> {
        let source = "foobar";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
            test_literal_expression(ex, Operand::Identifier("foobar"))
        });
        Ok(())
    }

    #[test]
    fn can_parse_an_integer_literal_expression() -> Result<()> {
        let source = "5;";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
            test_literal_expression(ex, Operand::Integer(5))
        });
        Ok(())
    }

    #[test]
    fn can_parse_prefix_expressions() -> Result<()> {
        let prefix_tests = vec![
            ("!5", PrefixOperator::Not, Operand::Integer(5)),
            ("-15", PrefixOperator::Minus, Operand::Integer(15)),
            ("!true;", PrefixOperator::Not, Operand::Boolean(true)),
            ("!false;", PrefixOperator::Not, Operand::Boolean(false)),
        ];

        for (source, operator, value) in prefix_tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
                match ex.kind() {
                    ExpressionKind::Prefix(prefix) => {
                        assert_eq!(operator, prefix.operator());
                        test_literal_expression(prefix.right(), value);
                    }
                    other => panic!("got {:?} expecting a prefix expression", other),
                }
            })
        }
        Ok(())
    }

    #[test]
    fn can_correctly_determine_operator_precedence() -> Result<()> {
        let precedence_tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            ("a + b - c", "((a + b) - c)"),
            ("a * b * c", "((a * b) * c)"),
            ("a * b / c", "((a * b) / c)"),
            ("a + b / c", "(a + (b / c))"),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)\n((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("(5 + 5) * 2 * (5 + 5)", "(((5 + 5) * 2) * (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            ("a + add(b * c) + d", "((a + add((b * c))) + d)"),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
        ];

        for (source, expected_output) in precedence_tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;
            assert_eq!(program.to_string(), expected_output);
        }
        Ok(())
    }

    #[test]
    fn can_parse_infix_expressions() -> Result<()> {
        let infix_expressions = vec![
            (
                "5 + 5;",
                Operand::Integer(5),
                InfixOperator::Add,
                Operand::Integer(5),
            ),
            (
                "5 - 5;",
                Operand::Integer(5),
                InfixOperator::Subtract,
                Operand::Integer(5),
            ),
            (
                "5 * 5;",
                Operand::Integer(5),
                InfixOperator::Multiply,
                Operand::Integer(5),
            ),
            (
                "5 / 5;",
                Operand::Integer(5),
                InfixOperator::Divide,
                Operand::Integer(5),
            ),
            (
                "5 > 5;",
                Operand::Integer(5),
                InfixOperator::GreaterThan,
                Operand::Integer(5),
            ),
            (
                "5 < 5;",
                Operand::Integer(5),
                InfixOperator::LessThan,
                Operand::Integer(5),
            ),
            (
                "5 == 5;",
                Operand::Integer(5),
                InfixOperator::Equals,
                Operand::Integer(5),
            ),
            (
                "5 != 5;",
                Operand::Integer(5),
                InfixOperator::NotEquals,
                Operand::Integer(5),
            ),
            (
                "foobar + barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::Add,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar - barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::Subtract,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar * barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::Multiply,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar / barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::Divide,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar > barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::GreaterThan,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar < barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::LessThan,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar == barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::Equals,
                Operand::Identifier("barfoo"),
            ),
            (
                "foobar != barfoo;",
                Operand::Identifier("foobar"),
                InfixOperator::NotEquals,
                Operand::Identifier("barfoo"),
            ),
            (
                "true == true",
                Operand::Boolean(true),
                InfixOperator::Equals,
                Operand::Boolean(true),
            ),
            (
                "true != false",
                Operand::Boolean(true),
                InfixOperator::NotEquals,
                Operand::Boolean(false),
            ),
            (
                "false == false",
                Operand::Boolean(false),
                InfixOperator::Equals,
                Operand::Boolean(false),
            ),
        ];

        for (source, left, operator, right) in infix_expressions {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
                test_infix_expression(ex, left, right, operator)
            });
        }
        Ok(())
    }

    #[test]
    fn can_parse_boolean_expressions() -> Result<()> {
        let tests = vec![("true;", true), ("false;", false)];

        for (source, value) in tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
                test_literal_expression(ex, Operand::Boolean(value))
            });
        }
        Ok(())
    }

    #[test]
    fn can_parse_if_expressions() -> Result<()> {
        let source = "if (x < y) {x}";
        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| match ex.kind() {
                ExpressionKind::If(if_expression) => {
                    test_infix_expression(
                        if_expression.condition(),
                        Operand::Identifier("x"),
                        Operand::Identifier("y"),
                        InfixOperator::LessThan,
                    );
                    test_block_statement(if_expression.consequence(), |block_statement| {
                        assert_eq!(1, block_statement.statements().len());
                        test_statement_as_expression_statement(
                            block_statement.statements().first().unwrap(),
                            |ex| test_literal_expression(ex, Operand::Identifier("x")),
                        );
                    });
                    assert!(if_expression.alternative().is_none());
                }
                other => panic!("got {:?} expecting an if expression", other),
            },
        );
        Ok(())
    }

    #[test]
    fn can_parse_if_else_expressions() -> Result<()> {
        let source = "if (a < b) {a} else {b}";
        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| match ex.kind() {
                ExpressionKind::If(if_expression) => {
                    test_infix_expression(
                        if_expression.condition(),
                        Operand::Identifier("a"),
                        Operand::Identifier("b"),
                        InfixOperator::LessThan,
                    );
                    test_block_statement(if_expression.consequence(), |block_statement| {
                        assert_eq!(1, block_statement.statements().len());
                        test_statement_as_expression_statement(
                            block_statement.statements().first().unwrap(),
                            |ex| test_literal_expression(ex, Operand::Identifier("a")),
                        );
                    });
                    test_block_statement(
                        if_expression
                            .alternative()
                            .as_ref()
                            .expect("no alternative"),
                        |block_statement| {
                            assert_eq!(1, block_statement.statements().len());
                            test_statement_as_expression_statement(
                                block_statement.statements().first().unwrap(),
                                |ex| test_literal_expression(ex, Operand::Identifier("b")),
                            );
                        },
                    );
                }
                other => panic!("got {:?} expecting an if expression", other),
            },
        );
        Ok(())
    }

    #[test]
    fn can_parse_a_function_literal() -> Result<()> {
        let source = "fn(x, y) {x + y;}";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| match ex.kind() {
                ExpressionKind::FunctionLiteral(function) => {
                    assert_eq!(2, function.parameters().len());
                    assert_eq!("x", function.parameters()[0].name());
                    assert_eq!("y", function.parameters()[1].name());
                    test_block_statement(function.body(), |block_statement| {
                        assert_eq!(1, block_statement.statements().len());
                        test_statement_as_expression_statement(
                            block_statement.statements().first().unwrap(),
                            |block_ex| {
                                test_infix_expression(
                                    block_ex,
                                    Operand::Identifier("x"),
                                    Operand::Identifier("y"),
                                    InfixOperator::Add,
                                );
                            },
                        );
                    });
                }
                other => panic!("got {:?} expecting a function literal", other),
            },
        );
        Ok(())
    }

    #[test]
    fn can_parse_function_parameter_lists() -> Result<()> {
        let tests = vec![
            ("fn () {}", vec![]),
            ("fn (x) {}", vec!["x"]),
            ("fn (x, y) {}", vec!["x", "y"]),
        ];

        for (source, parameters) in tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;
            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
                match ex.kind() {
                    ExpressionKind::FunctionLiteral(function) => {
                        let fn_params = function
                            .parameters()
                            .iter()
                            .map(|p| p.name())
                            .collect::<Vec<&str>>();
                        for (fp, p) in fn_params.iter().zip(parameters.iter()) {
                            assert_eq!(fp, p);
                        }
                    }
                    other => panic!("got {:?} expecting a function literal expression", other),
                }
            })
        }
        Ok(())
    }

    #[test]
    fn can_parse_call_expression() -> Result<()> {
        let source = "add(1, 2 * 3, 4 + 5);";
        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| match ex.kind() {
                ExpressionKind::Call(call_expression) => {
                    test_literal_expression(call_expression.function(), Operand::Identifier("add"));
                    assert_eq!(3, call_expression.arguments().len());
                    test_literal_expression(
                        call_expression.arguments().get(0).unwrap(),
                        Operand::Integer(1),
                    );
                    test_infix_expression(
                        call_expression.arguments().get(1).unwrap(),
                        Operand::Integer(2),
                        Operand::Integer(3),
                        InfixOperator::Multiply,
                    );
                    test_infix_expression(
                        call_expression.arguments().get(2).unwrap(),
                        Operand::Integer(4),
                        Operand::Integer(5),
                        InfixOperator::Add,
                    )
                }
                other => panic!("got {:?} expecting a call_expression", other),
            },
        );
        Ok(())
    }
}
