use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use std::iter::Peekable;
use std::rc::Rc;
use std::result;

const PARSING_A_LET_STATEMENT: &'static str = "parsing a let statement";
const PARSING_A_PROGRAM: &'static str = "parsing a program";
const PARSING_A_PREFIX_EXPRESSION: &'static str = "parsing a prefix expression";

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

#[derive(Debug)]
pub struct ParseError {
    errors: Vec<Box<Error>>,
}

impl ParseError {
    fn new(errors: Vec<Error>) -> Error {
        Error {
            kind: ErrorKind::Parse(ParseError {
                errors: errors.into_iter().map(|e| Box::new(e)).collect(),
            }),
        }
    }
}

#[derive(Debug)]
pub struct StatementError {
    message: String,
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

    fn unexpected_eof(expected: &str, action: &str) -> Error {
        Error {
            kind: ErrorKind::Statement(StatementError {
                message: format!(
                    "reached the end of the source when expecting a {} while {}",
                    expected, action
                ),
            }),
        }
    }
}

#[derive(Debug)]
enum ErrorKind {
    Parse(ParseError),
    Statement(StatementError),
}

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

pub type Result<T> = result::Result<T, Error>;

struct Parser {
    lexer: Peekable<Lexer>,
    prefix_parse_fns: HashMap<Tag, Rc<dyn Fn(&mut Parser, Token) -> Result<Expression>>>,
    infix_parse_fns: HashMap<Tag, Rc<dyn Fn(&mut Parser, Expression, Token) -> Result<Expression>>>,
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
        _ => Lowest,
    }
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
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
            let next_statement: Result<Statement> = match token.kind {
                Kind::Let => self.parse_let_statement(token),
                Kind::Return => self.parse_return_statement(token),
                _ => self.parse_expression_statement(token),
            };

            if let Ok(statement) = next_statement {
                statements.push(statement);
            } else {
                errors.push(next_statement.err().expect("not an error"));
            }
        }

        if errors.is_empty() {
            Ok(Program::new(statements))
        } else {
            Err(ParseError::new(errors))
        }
    }

    fn parse_statement(&mut self, token: Token) -> Result<Statement> {
        match token.kind {
            Kind::Let => self.parse_let_statement(token),
            other => unimplemented!(),
        }
    }

    fn parse_expression_statement(&mut self, token: Token) -> Result<Statement> {
        self.parse_expression(token, Precedence::Lowest)
            .map(|expression| ExpressionStatement::new(expression))
            .map(|expression| {
                self.optional_peek_consume(Tag::Semicolon);
                expression
            })
    }

    fn parse_expression(&mut self, token: Token, precedence: Precedence) -> Result<Expression> {
        let prefix_f = self
            .prefix_parse_fns
            .get(&token.kind.tag())
            //TODO don't panic here, raise an error
            .expect("no prefix parse fn")
            .clone();

        let mut left = prefix_f(self, token)?;
        while let Some(peek) = self.lexer.peek() {
            if peek.kind.tag() == Tag::Semicolon || precedence >= operator_precedence(peek) {
                break;
            }
            let next_token = self.lexer.next().expect("no token");
            let infix_f = self
                .infix_parse_fns
                .get(&next_token.kind.tag())
                //TODO don't panic here, raise an error
                .expect("no infix parse fn")
                .clone();

            left = infix_f(self, left, next_token)?;
        }

        Ok(left)
    }

    fn parse_identifier_expression(&mut self, token: Token) -> Result<Expression> {
        Ok(IdentifierExpression::new(Identifier::new(token)))
    }

    fn parse_integer_literal_expression(&mut self, token: Token) -> Result<Expression> {
        let value = match &token.kind {
            // Anything which does not parse as an i64 here would indicate a bug in the
            // lexer so ok to expect
            Kind::Int(repr) => repr.parse::<i64>().expect("not an integer literal"),
            other => panic!("got {:?} expecting an Int"),
        };

        Ok(IntegerLiteralExpression::new(token, value))
    }

    fn parse_prefix_expression(&mut self, token: Token) -> Result<Expression> {
        let operator = match token.kind {
            Kind::Bang => PrefixOperator::Not,
            Kind::Minus => PrefixOperator::Minus,
            //TODO raise an error here, don't panic
            other => panic!("{:?} is not a prefix operator"),
        };

        let next_token = self.expect_next()?;
        let right = self.parse_expression(next_token, Precedence::Prefix)?;
        Ok(PrefixExpression::new(token, operator, right))
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
            //TODO raise an error here, don't panic
            other => panic!("{:?} is not an infix operator", other),
        };

        let next_token = self.expect_next()?;
        let right = self.parse_expression(next_token, operator_precedence(&token))?;
        Ok(InfixExpression::new(left, right, operator))
    }

    fn consume_source_line(&mut self) {
        while let Some(next) = self.lexer.next() {
            if next.kind.tag() == Tag::Semicolon {
                break;
            }
        }
    }

    fn parse_let_statement(&mut self, token: Token) -> Result<Statement> {
        let elements = self
            .expect_peek_consume(Tag::Ident, PARSING_A_LET_STATEMENT)
            .and_then(|ident_token| {
                self.expect_peek_consume(Tag::Assign, PARSING_A_LET_STATEMENT)
                    .map(|_| ident_token)
            });

        match elements {
            Ok(ident_token) => {
                // TODO: This should not be consuming the source line but parsing the expression into 'elements' above
                self.consume_source_line();
                Ok(LetStatement::new(token, Identifier::new(ident_token)))
            }
            Err(e) => {
                self.consume_source_line();
                Err(e)
            }
        }
    }

    fn parse_return_statement(&mut self, token: Token) -> Result<Statement> {
        //TODO: this should not be consuming the source line but parsing the expression
        self.consume_source_line();
        Ok(ReturnStatement::new(token))
    }

    fn expect_peek_consume(&mut self, tag: Tag, action: &str) -> Result<Token> {
        match self.lexer.peek() {
            Some(token) if token.kind.tag() == tag => Ok(self.lexer.next().expect("no token")),
            Some(token) => Err(StatementError::unexpected_token(
                &tag.to_string(),
                token,
                action,
            )),
            None => Err(StatementError::unexpected_eof(&tag.to_string(), action)),
        }
    }

    fn optional_peek_consume(&mut self, tag: Tag) -> Option<Token> {
        match self.lexer.peek() {
            Some(token) if token.kind.tag() == tag => Some(self.lexer.next().expect("no token")),
            _ => None,
        }
    }

    fn expect_next(&mut self) -> Result<Token> {
        match self.lexer.next() {
            None => Err(StatementError::unexpected_eof(
                "an expression",
                PARSING_A_PREFIX_EXPRESSION,
            )),
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

    fn test_literal_expression(expression: &Expression, expected: Operand) {
        match (expression.kind(), expected) {
            (ExpressionKind::IntegerLiteral(integer), Operand::Integer(expected_int)) =>
                test_integer_literal(integer, expected_int),
            (ExpressionKind::Identifier(identifier), Operand::Identifier(expected_ident)) =>
                test_identifier(identifier, expected_ident),
            (other_kind, other_operand) =>
                panic!("non matching expression kind {:?} and operand {:?}", other_kind, other_operand),
        }
    }

    fn test_infix_expression(expression: &Expression, left: Operand, right: Operand, operator: InfixOperator) {
        match expression.kind() {
            ExpressionKind::Infix(infix) => {
                test_literal_expression(infix.left(), left);
                test_literal_expression(infix.right(), right);
                assert_eq!(operator, infix.operator());
            }
            other => panic!("got {:?} expecting an infix expression", other),
        }
    }

    fn test_let_statement(statement: &Statement, name: &str) {
        assert!(matches!(statement.kind(), StatementKind::Let(_)));

        match statement.kind() {
            StatementKind::Let(ref let_statement) => {
                assert_eq!(name, let_statement.name().name())
            }
            other => panic!("got a {:?} expecting a let statement", other),
        }
    }

    fn parser_from_source(source: &str) -> Parser {
        Parser::new(source.to_source().into_tokens())
    }

    #[test]
    fn can_parse_a_program_with_only_let_statements() -> Result<()> {
        let source: &str = indoc! {"
        let x = 5;
        let y = 10;
        let foobar = 838383;
        "};

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;

        assert_eq!(3, program.statements().len());

        let test_names = vec!["x", "y", "foobar"];

        program
            .statements()
            .iter()
            .zip(test_names)
            .for_each(|(stmt, name)| test_let_statement(stmt, name));

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
        let source = indoc! {"
        return 5;
        return 10;
        return 993322;
        "};

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(3, program.statements().len());

        program
            .statements()
            .iter()
            .for_each(|stmt| assert!(matches!(stmt.kind(), StatementKind::Return(_))));
        Ok(())
    }

    fn test_statement_as_expression_statement<T>(statement: &Statement, t: T)
        where
            T: Fn(&Expression) -> (),
    {
        match statement.kind() {
            StatementKind::Expression(expression_statement) => {
                t(expression_statement.expression())
            }
            other => panic!("got {} when expecting an ExpressionStatement", other),
        }
    }

    #[test]
    fn can_parse_an_identifier_expression() -> Result<()> {
        let source = "foobar";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| test_literal_expression(ex, Operand::Identifier("foobar")),
        );

        //     match ek {
        //     ExpressionKind::Identifier(identifier) => {
        //         assert_eq!("foobar", identifier.to_string())
        //     }
        //     other => panic!("got {:?} expecting an identifier expression", other),
        // },

        Ok(())
    }

    #[test]
    fn can_parse_an_integer_literal_expression() -> Result<()> {
        let source = "5;";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ex| test_literal_expression(ex, Operand::Integer(5)),
        );
        Ok(())
    }

    #[test]
    fn can_parse_prefix_expressions() -> Result<()> {
        let prefix_tests = vec![
            ("!5", PrefixOperator::Not, 5),
            ("-15", PrefixOperator::Minus, 15),
        ];

        for (source, operator, value) in prefix_tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ex| {
                match ex.kind() {
                    ExpressionKind::Prefix(prefix) => {
                        assert_eq!(operator, prefix.operator());
                        test_literal_expression(prefix.right().as_ref(), Operand::Integer(value));
                    }
                    other => panic!("got {:?} expecting a prefix expression"),
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
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
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
            ("5 + 5;", Operand::Integer(5), InfixOperator::Add, Operand::Integer(5)),
            ("5 - 5;", Operand::Integer(5), InfixOperator::Subtract, Operand::Integer(5)),
            ("5 * 5;", Operand::Integer(5), InfixOperator::Multiply, Operand::Integer(5)),
            ("5 / 5;", Operand::Integer(5), InfixOperator::Divide, Operand::Integer(5)),
            ("5 > 5;", Operand::Integer(5), InfixOperator::GreaterThan, Operand::Integer(5)),
            ("5 < 5;", Operand::Integer(5), InfixOperator::LessThan, Operand::Integer(5)),
            ("5 == 5;", Operand::Integer(5), InfixOperator::Equals, Operand::Integer(5)),
            ("5 != 5;", Operand::Integer(5), InfixOperator::NotEquals, Operand::Integer(5)),
            ("foobar + barfoo;", Operand::Identifier("foobar"), InfixOperator::Add, Operand::Identifier("barfoo")),
            ("foobar - barfoo;", Operand::Identifier("foobar"), InfixOperator::Subtract, Operand::Identifier("barfoo")),
            ("foobar * barfoo;", Operand::Identifier("foobar"), InfixOperator::Multiply, Operand::Identifier("barfoo")),
            ("foobar / barfoo;", Operand::Identifier("foobar"), InfixOperator::Divide, Operand::Identifier("barfoo")),
            ("foobar > barfoo;", Operand::Identifier("foobar"), InfixOperator::GreaterThan, Operand::Identifier("barfoo")),
            ("foobar < barfoo;", Operand::Identifier("foobar"), InfixOperator::LessThan, Operand::Identifier("barfoo")),
            ("foobar == barfoo;", Operand::Identifier("foobar"), InfixOperator::Equals, Operand::Identifier("barfoo")),
            ("foobar != barfoo;", Operand::Identifier("foobar"), InfixOperator::NotEquals, Operand::Identifier("barfoo")),
        ];

        for (source, left, operator, right) in infix_expressions {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(
                program.statements().first().unwrap(),
                |ex| test_infix_expression(ex, left, right, operator)
            );
        }
        Ok(())
    }
}
