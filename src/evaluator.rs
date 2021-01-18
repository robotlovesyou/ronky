use std::result;

use crate::ast::{Program, Statement, StatementKind, ExpressionStatement, Expression, ExpressionKind, IntegerLiteralExpression, BooleanExpression};
use crate::parser;
use crate::object::{Object, ObjectKind, Integer, Inspectable, Boolean};

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind
}

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error {
            kind
        }
    }
}

#[derive(Debug)]
pub struct ParserError {
    message: String
}

impl ParserError {
    fn new_parser_error(e: parser::Error) -> Error {
        Error::new(ErrorKind::Parser(
            ParserError {
                message: format!("{}", e)
            }
        ))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Parser(ParserError)
}

type Result<T> = result::Result<T, Error>;

pub trait Evaluable {
    fn evaluate(&self) -> Result<Object>;
}

impl Evaluable for Program {
    fn evaluate(&self) -> Result<Object> {
        //TODO: This is a terrible hack
        self.statements().last().unwrap().evaluate()
    }
}

impl Evaluable for &Statement {
    fn evaluate(&self) -> Result<Object> {
        match self.kind() {
            StatementKind::Let(_) => unimplemented!(),
            StatementKind::Return(_) => unimplemented!(),
            StatementKind::Expression(kind) => kind.evaluate(),
            StatementKind::Block(_) => unimplemented!(),
        }
    }
}

impl Evaluable for &ExpressionStatement {
    fn evaluate(&self) -> Result<Object> {
        self.expression().evaluate()
    }
}

impl Evaluable for &Expression {
    fn evaluate(&self) -> Result<Object> {
        match self.kind() {
            ExpressionKind::Boolean(kind) => kind.evaluate(),
            ExpressionKind::Identifier(_) => unimplemented!(),
            ExpressionKind::IntegerLiteral(kind) => kind.evaluate(),
            ExpressionKind::Prefix(_) => unimplemented!(),
            ExpressionKind::Infix(_) => unimplemented!(),
            ExpressionKind::If(_) => unimplemented!(),
            ExpressionKind::FunctionLiteral(_) => unimplemented!(),
            ExpressionKind::Call(_) => unimplemented!(),
        }
    }
}

impl Evaluable for &IntegerLiteralExpression {
    fn evaluate(&self) -> Result<Object> {
        Ok(Integer::new_integer_object(self.value()))
    }
}

impl Evaluable for &BooleanExpression {
    fn evaluate(&self) -> Result<Object> {
        Ok(Boolean::new_boolean_object(self.value()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Parser;
    use crate::source::ToSource;
    use crate::lexer::IntoTokens;

    #[derive(Debug)]
    enum Value {
        Integer(i64),
        Boolean(bool),
    }

    impl From<parser::Error> for Error {
        fn from(e: parser::Error) -> Self {
            ParserError::new_parser_error(e)
        }
    }

    fn program_from_source(source: &str) -> Result<Program> {
        let mut parser = Parser::new(source.to_source().into_tokens());
        let program = parser.parse()?;
        Ok(program)
    }

    fn test_value(object: Object, expected_value: Value) {
        match (object.kind(), expected_value) {
            (ObjectKind::Integer(integer), Value::Integer(expected)) => assert_eq!(&expected, integer.value()),
            (ObjectKind::Boolean(boolean), Value::Boolean(expected)) => assert_eq!(&expected, boolean.value()),
            other => panic!("{:?} is not a valid option")
        }
    }

    fn test_evaluated_value(tests: Vec<(&str, Value)>) -> Result<()> {
        for (source, expected) in tests {
            let program = program_from_source(source)?;
            let evaluated = program.evaluate()?;
            test_value(evaluated, expected);
        }
        Ok(())
    }
    
    #[test]
    fn can_evaluate_integer_expression() -> Result<()> {
        let tests = vec![
            ("5", Value::Integer(5i64)),
            ("10", Value::Integer(10)),
        ];

        test_evaluated_value(tests)
    }

    #[test]
    fn can_evaluate_bool_expression() -> Result<()> {
        let tests = vec![
            ("true", Value::Boolean(true)),
            ("false", Value::Boolean(false))
        ];

        test_evaluated_value(tests)
    }
}