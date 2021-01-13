use std::result;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::iter::Peekable;
use std::rc::Rc;

const PARSING_A_LET_STATEMENT: &'static str = "parsing a let statement";
const PARSING_A_PROGRAM: &'static str = "parsing a program";

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
    prefix_parse_fns: HashMap<Tag, Rc<PrefixParseFn>>,
    infix_parse_fns: HashMap<Tag, Rc<InfixParseFn>>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        let mut parser = Parser{
            lexer: lexer.peekable(),
            prefix_parse_fns: HashMap::new(),
            infix_parse_fns: HashMap::new(),
        };
        parser.register_prefix_fn(Tag::Ident, Rc::new(|parser, token| parser.parse_identifier_expression(token)));
        parser
    }

    fn register_prefix_fn(&mut self, tag: Tag, f: Rc<PrefixParseFn>) {
        self.prefix_parse_fns.insert(tag, f);
    }

    fn register_infix_fn(&mut self, tag: Tag, f: Rc<InfixParseFn>) {
        self.infix_parse_fns.insert(tag, f);
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

    fn parse_return_statement(&mut self, tkn: Token) -> Result<Statement> {
        //TODO: this should not be consuming the source line but parsing the expression
        self.consume_source_line();
        Ok(ReturnStatement::new(tkn))
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

    fn optional_peek_consume(&mut self, tag: Tag) -> Option<Token> {
        match self.lexer.peek() {
            Some(token) if token.kind.tag() == tag  =>
                Some(self.lexer.next().expect("no token")),
            _ => None,
        }
    }
}

type PrefixParseFn = dyn Fn(&mut Parser, Token) -> Result<Expression>;
type InfixParseFn = dyn Fn(&mut Parser, Token, Expression) -> Result<Expression>;

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
            other => panic!("got a {:?} expecting a let statement", other)
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
        let source = indoc!{"
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
        let source = indoc!{"
        return 5;
        return 10;
        return 993322;
        "};

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(3, program.statements().len());

        program.statements().iter().for_each(|stmt| assert!(matches!(stmt.kind(), StatementKind::Return(_))));
        Ok(())
    }

    #[test]
    fn can_parse_an_identifier_expression() -> Result<()> {
        let source = "foobar";

        let mut parser = parser_from_source(source);
        let program = parser.parse()?;
        assert_eq!(1, program.statements().len());
        assert!(matches!(program.statements().first().unwrap().kind(), StatementKind::Expression(_)));
        if let StatementKind::Expression(expression_statement) = program.statements().first().unwrap().kind() {
            assert!(matches!(expression_statement.expression().kind(), ExpressionKind::Identifier(_)));
            assert_eq!("foobar", expression_statement.expression().token().to_string());
        }
        Ok(())
    }
}