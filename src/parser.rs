use std::result;
use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::iter::Peekable;

const PARSING_A_LET_STATEMENT: &'static str = "parsing a let statement";
const PARSING_A_PROGRAM: &'static str = "parsing a program";

#[derive(Debug)]
pub struct ParseError {
    errors: Vec<Box<Error>>
}

impl ParseError {
    fn new(errors: Vec<Error>) -> Error {
        Error{
            kind: ErrorKind::Parse(ParseError {
                errors: errors.into_iter().map(|e| Box::new(e)).collect(),
            })
        }
    }
}

#[derive(Debug)]
pub struct StatementError {
    message: String,
}

impl StatementError {
    fn unexpected_token(expected: &str, got: &Token, action: &str) -> Error {
        Error{
            kind: ErrorKind::Statement(StatementError {
                message: format!(
                    "got an {} instead of an {} while {} at line {} column {}",
                    got.kind.tag(),
                    expected,
                    action,
                    got.line,
                    got.column,
                )
            })
        }
    }

    fn unexpected_eof(expected: &str, action: &str) -> Error {
        Error {
            kind: ErrorKind::Statement(StatementError {
                message: format!(
                    "reached the end of the source when expecting a {} while {}",
                    expected,
                    action
                )
            })
        }
    }
}

#[derive(Debug)]
enum ErrorKind{
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
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        Parser{lexer: lexer.peekable()}
    }

    pub fn parse(&mut self) -> Result<Program> {
        let mut statements: Vec<Statement> = Vec::new();
        let mut errors: Vec<Error> = Vec::new();

        while let Some(token) = self.lexer.next() {
            let next_statement: Result<Statement> = match token.kind {
                Kind::Let => self.parse_let_statement(token),
                _ => Err(
                    StatementError::unexpected_token(
                        "top level declaration",
                        &token,
                        PARSING_A_PROGRAM
                    )
                )
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

    fn parse_statement(&mut self, tkn: Token) -> Result<Statement> {
        match tkn.kind {
            Kind::Let => self.parse_let_statement(tkn),
            other => unimplemented!(),
        }
    }

    fn parse_let_statement(&mut self, tkn: Token) -> Result<Statement> {
        let ident = self.expect_peek_consume(Tag::Ident, PARSING_A_LET_STATEMENT)?;
        self.expect_peek_consume(Tag::Assign, PARSING_A_LET_STATEMENT)?;
        while let Some(next) = self.lexer.next() {
            if next.kind.tag() == Tag::Semicolon {
                break;
            }
        }

        Ok(LetStatement::new(tkn, Identifier::new(ident)))
    }

    fn expect_peek_consume(&mut self, tag: Tag, action: &str) -> Result<Token> {
        match self.lexer.peek() {
            Some(token) if token.kind.tag() == tag => Ok(
                self.lexer.next().expect("no token")
            ),
            Some(token) => Err(
                StatementError::unexpected_token(
                    &tag.to_string(),
                    token,
                    action
                )
            ),
            None => Err(
                StatementError::unexpected_eof(
                    &tag.to_string(),
                    action
                )
            )
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::source::*;
    use indoc::indoc;

    fn test_let_statement(statement: &Statement, name: &str) {
        assert!(matches!(statement.kind(),  StatementKind::Let(_)));

        match statement.kind() {
            StatementKind::Let(ref let_statement) => {
                assert_eq!(name, let_statement.name().name())
            },
        }
    }

    fn parser_from_source(source: &str) -> Parser {
        Parser::new(source.to_source().into_tokens())
    }
    
    #[test]
    fn can_parse_a_program_with_only_let_statements() -> Result<()> {
        let source: &str = indoc!{"
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
            .for_each(
                |(stmt, name)| test_let_statement(stmt, name)
            );

        Ok(())
    }

    #[test]
    fn raises_an_error_when_parsing_an_invalid_let_statement() {
        let source: &str = indoc!{"
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
}