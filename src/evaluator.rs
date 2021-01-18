use std::result;

use crate::ast::{Program, Statement, StatementKind, ExpressionStatement, Expression, ExpressionKind, IntegerLiteralExpression, BooleanExpression, PrefixExpression, PrefixOperator};
use crate::parser;
use crate::object::{Object, ObjectKind, Integer, Inspectable, Boolean, Null};

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

pub struct Environment {

}

pub trait Evaluable {
    fn evaluate(&self, env: &mut Environment) -> Result<Object>;
}

impl Evaluable for Program {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        //TODO: This is a terrible hack
        self.statements().last().unwrap().evaluate(env)
    }
}

impl Evaluable for &Statement {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        match self.kind() {
            StatementKind::Let(_) => unimplemented!(),
            StatementKind::Return(_) => unimplemented!(),
            StatementKind::Expression(kind) => kind.evaluate(env),
            StatementKind::Block(_) => unimplemented!(),
        }
    }
}

impl Evaluable for &ExpressionStatement {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        self.expression().evaluate(env)
    }
}

impl Evaluable for &Expression {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        match self.kind() {
            ExpressionKind::Boolean(kind) => kind.evaluate(env),
            ExpressionKind::Identifier(_) => unimplemented!(),
            ExpressionKind::IntegerLiteral(kind) => kind.evaluate(env),
            ExpressionKind::Prefix(kind) => kind.evaluate(env),
            ExpressionKind::Infix(_) => unimplemented!(),
            ExpressionKind::If(_) => unimplemented!(),
            ExpressionKind::FunctionLiteral(_) => unimplemented!(),
            ExpressionKind::Call(_) => unimplemented!(),
        }
    }
}

impl Evaluable for &IntegerLiteralExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        Ok(Integer::new_integer_object(self.value()))
    }
}

impl Evaluable for &BooleanExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        Ok(Boolean::new_boolean_object(self.value()))
    }
}

impl Evaluable for &PrefixExpression {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        let value = self.right().evaluate(env)?;
        match self.operator() {
            PrefixOperator::Minus => Ok(eval_prefix_neg_expression(value)),
            PrefixOperator::Not => Ok(eval_prefix_not_expression(value))
        }

    }
}

fn eval_prefix_not_expression(expression: Object) -> Object {
    match expression.kind() {
        ObjectKind::Integer(integer) => Boolean::new_boolean_object(integer.value == 0),
        ObjectKind::Boolean(boolean) => Boolean::new_boolean_object(!(*boolean.value())),
        ObjectKind::Null(_) => Boolean::new_boolean_object(false),
    }
}

fn eval_prefix_neg_expression(expression: Object) -> Object {
    match expression.kind() {
        ObjectKind::Integer(integer) => Integer::new_integer_object(-(*integer.value())),
        _ => Null::new_null_object()
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

    fn test_value(object: Object, expected_value: Value, source: &str) {
        match (object.kind(), expected_value) {
            (ObjectKind::Integer(integer), Value::Integer(expected)) => assert_eq!(&expected, integer.value(), "{}", source),
            (ObjectKind::Boolean(boolean), Value::Boolean(expected)) => assert_eq!(&expected, boolean.value(), "{}", source),
            other => panic!("{:?} is not a valid option with source {}", other, source)
        }
    }

    fn test_evaluated_value(tests: Vec<(&str, Value)>) -> Result<()> {
        for (source, expected) in tests {
            let program = program_from_source(source)?;
            let mut env = Environment{};
            let evaluated = program.evaluate(&mut env)?;
            test_value(evaluated, expected, source);
        }
        Ok(())
    }
    
    #[test]
    fn can_evaluate_integer_expression() -> Result<()> {
        let tests = vec![
            ("5", Value::Integer(5i64)),
            ("10", Value::Integer(10)),
            ("-5", Value::Integer(-5)),
            ("-10", Value::Integer(-10))
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

    #[test]
    fn can_evaluate_bang_expression() -> Result<()> {
        let tests = vec![
            ("!true", Value::Boolean(false)),
            ("!false", Value::Boolean(true)),
            ("!5", Value::Boolean(false)),
            ("!!true", Value::Boolean(true)),
            ("!!false", Value::Boolean(false)),
            ("!!5", Value::Boolean(true)),
        ];

        test_evaluated_value(tests)
    }
}