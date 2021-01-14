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
    infix_parse_fns: HashMap<Tag, Rc<dyn Fn(&mut Parser, Token) -> Result<Expression>>>,
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
        F: 'static + Fn(&mut Parser, Token) -> Result<Expression>,
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
            .expect("no prefix parse fn")
            .clone();

        prefix_f(self, token)
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
            Kind::Bang => Operator::Not,
            Kind::Minus => Operator::Minus,
            other => panic!("{:?} is not a prefix operator"),
        };

        let next_token = self.expect_next()?;
        let right = self.parse_expression(next_token, Precedence::Prefix)?;
        Ok(PrefixExpression::new(token, operator, right))
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
        T: Fn(&ExpressionKind) -> (),
    {
        match statement.kind() {
            StatementKind::Expression(expression_statement) => {
                t(expression_statement.expression().kind())
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
            |ek| match ek {
                ExpressionKind::Identifier(identifier) => {
                    assert_eq!("foobar", identifier.to_string())
                }
                other => panic!("got {:?} expecting an identifier expression", other),
            },
        );
        Ok(())
    }

    fn test_integer_literal(integer_literal: &IntegerLiteralExpression, expected: i64) {
        assert_eq!(expected, integer_literal.value());
        assert_eq!(format!("{}", expected), integer_literal.to_string());
    }

    #[test]
    fn can_parse_an_integer_literal_expression() -> Result<()> {
        let source = "5;";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        test_statement_as_expression_statement(
            program.statements().first().unwrap(),
            |ek| match ek {
                ExpressionKind::IntegerLiteral(integer) => {
                    test_integer_literal(integer, 5);
                }
                other => panic!("got {:?} expecting an integer expression", other),
            },
        );
        Ok(())
    }

    #[test]
    fn can_parse_prefix_expressions() -> Result<()> {
        let prefix_tests = vec![("!5", Operator::Not, 5), ("-15", Operator::Minus, 15)];

        for (source, operator, value) in prefix_tests {
            let mut parser = parser_from_source(source);
            let program = parser.parse()?;

            assert_eq!(1, program.statements().len());
            test_statement_as_expression_statement(program.statements().first().unwrap(), |ek| {
                match ek {
                    ExpressionKind::Prefix(prefix) => {
                        assert_eq!(operator, prefix.operator());
                        match prefix.right().as_ref().kind() {
                            ExpressionKind::IntegerLiteral(integer) => {
                                test_integer_literal(integer, value);
                            }
                            other => panic!("got {:?} expecing an integer literal"),
                        }
                    }
                    other => panic!("got {:?} expecting an integer expression"),
                }
            })
        }

        Ok(())
    }
}
