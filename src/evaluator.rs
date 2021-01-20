use std::result;

use crate::ast::{Program, Statement, StatementKind, ExpressionStatement, Expression, ExpressionKind, IntegerLiteralExpression, BooleanExpression, PrefixExpression, PrefixOperator, InfixOperator, IfExpression};
use crate::parser;
use crate::object::{Object, ObjectKind, Integer, Inspectable, Boolean, Null};
use std::borrow::Borrow;

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

impl Evaluable for &[Statement] {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        // TODO: This is a terrible hack
        if self.is_empty() {
            Ok(Null::new_null_object())
        } else {
            self.last().unwrap().evaluate(env)
        }
    }
}

impl Evaluable for &Statement {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        match self.kind() {
            StatementKind::Let(_) => unimplemented!(),
            StatementKind::Return(_) => unimplemented!(),
            StatementKind::Expression(kind) => kind.evaluate(env),
            StatementKind::Block(kind) => kind.statements().evaluate(env)
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
            ExpressionKind::Boolean(kind) => evaluate_bool_literal_expresson(kind),
            ExpressionKind::Identifier(_) => unimplemented!(),
            ExpressionKind::IntegerLiteral(kind) => evaluate_integer_literal_expression(kind),
            ExpressionKind::Prefix(kind) => {
                let right = kind.right().evaluate(env)?;
                evaluate_prefix_expression(kind.operator(), &right)
            }
            ExpressionKind::Infix(kind) => {
                let left = kind.left().evaluate(env)?;
                let right = kind.right().evaluate(env)?;
                evaluate_infix_expression(kind.operator(), &left, &right)
            },
            ExpressionKind::If(kind) => evaluate_if_expression(kind, env),
            ExpressionKind::FunctionLiteral(_) => unimplemented!(),
            ExpressionKind::Call(_) => unimplemented!(),
        }
    }
}

fn evaluate_integer_literal_expression(expression: &IntegerLiteralExpression) -> Result<Object> {
    Ok(Integer::new_integer_object(expression.value()))
}

fn evaluate_bool_literal_expresson(expression: &BooleanExpression) -> Result<Object> {
    Ok(Boolean::new_boolean_object(expression.value()))
}

fn evaluate_prefix_expression(operator: PrefixOperator, right: &Object) -> Result<Object> {
    match operator {
        PrefixOperator::Minus => Ok(eval_prefix_neg_expression(right)),
        PrefixOperator::Not => Ok(eval_prefix_not_expression(right))
    }
}

fn eval_prefix_not_expression(value: &Object) -> Object {
    match value.kind() {
        ObjectKind::Integer(integer) => Boolean::new_boolean_object(integer.value == 0),
        ObjectKind::Boolean(boolean) => Boolean::new_boolean_object(!(*boolean.value())),
        ObjectKind::Null(_) => Boolean::new_boolean_object(false),
    }
}

fn eval_prefix_neg_expression(value: &Object) -> Object {
    match value.kind() {
        ObjectKind::Integer(integer) => Integer::new_integer_object(-(*integer.value())),
        _ => Null::new_null_object()
    }
}

fn evaluate_infix_expression(operator: InfixOperator, left: &Object, right: &Object) -> Result<Object> {
    match (left.kind(), right.kind()) {
        (ObjectKind::Integer(a), ObjectKind::Integer(b)) => evaluate_integer_infix_expression(operator, a, b),
        (ObjectKind::Boolean(a), ObjectKind::Boolean(b)) => evaluate_boolean_infix_expression(operator, a, b),
        // TODO: this should raise an error, not panic
        other => panic!("cannot operate on {:?}", other),
    }
}

fn evaluate_integer_infix_expression(operator: InfixOperator, left: &Integer, right: &Integer) -> Result<Object> {
    Ok(match operator {
        InfixOperator::Add => Integer::new_integer_object(left.value() + right.value()),
        InfixOperator::Subtract => Integer::new_integer_object(left.value() - right.value()),
        InfixOperator::Multiply => Integer::new_integer_object(left.value() * right.value()),
        InfixOperator::Divide => Integer::new_integer_object(left.value() / right.value()),
        InfixOperator::GreaterThan => Boolean::new_boolean_object(left.value() > right.value()),
        InfixOperator::LessThan => Boolean::new_boolean_object(left.value() < right.value()),
        InfixOperator::Equals => Boolean::new_boolean_object(left.value() == right.value()),
        InfixOperator::NotEquals => Boolean::new_boolean_object(left.value() != right.value())
    })
}

fn evaluate_boolean_infix_expression(operator: InfixOperator, left: &Boolean, right: &Boolean) -> Result<Object> {
    Ok(match operator {
        InfixOperator::Equals => Boolean::new_boolean_object(left.value() == right.value()),
        InfixOperator::NotEquals => Boolean::new_boolean_object(left.value() != right.value()),
        // TODO: this should raise an error, not panic
        other => panic!("{:?} is not a valid boolean operation", other)
    })
}

fn evaluate_if_expression(expression: &IfExpression, env: &mut Environment) -> Result<Object> {
    let condition = expression.condition().evaluate(env)?;
    if is_truthy(&condition) {
        expression.consequence().evaluate(env)
    } else {
        expression.alternative().as_ref().map_or_else(
            || Ok(Null::new_null_object()),
            |alternative| alternative.evaluate(env)
        )
    }
}

fn is_truthy(object: &Object) -> bool {
    match object.kind() {
        ObjectKind::Integer(kind) => *kind.value() != 0,
        ObjectKind::Boolean(kind) => *kind.value(),
        ObjectKind::Null(_) => false
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
        Null
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
            (ObjectKind::Null(_), Value::Null) => {}, // do nothing, these are always equal
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
            ("-10", Value::Integer(-10)),
            ("5 + 5 + 5 + 5 - 10", Value::Integer(10)),
            ("2 * 2 * 2 * 2 * 2", Value::Integer(32)),
            ("-50 + 100 + -50", Value::Integer(0)),
            ("5 * 2 + 10", Value::Integer(20)),
            ("5 + 2 * 10", Value::Integer(25)),
            ("20 + 2 * -10", Value::Integer(0)),
            ("50 / 2 * 2 + 10", Value::Integer(60)),
            ("2 * (5 + 10)", Value::Integer(30)),
            ("3 * 3 * 3 + 10", Value::Integer(37)),
            ("3 * (3 * 3) + 10", Value::Integer(37)),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Value::Integer(50)),
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
            ("1 < 2", Value::Boolean(true)),
            ("1 > 2", Value::Boolean(false)),
            ("1 < 1", Value::Boolean(false)),
            ("1 > 1", Value::Boolean(false)),
            ("1 == 1", Value::Boolean(true)),
            ("1 != 1", Value::Boolean(false)),
            ("1 == 2", Value::Boolean(false)),
            ("1 != 2", Value::Boolean(true)),
            ("true == true", Value::Boolean(true)),
            ("false == false", Value::Boolean(true)),
            ("true == false", Value::Boolean(false)),
            ("true != false", Value::Boolean(true)),
            ("false != true", Value::Boolean(true)),
            ("(1 < 2) == true", Value::Boolean(true)),
            ("(1 < 2) == false", Value::Boolean(false)),
            ("(1 > 2) == true", Value::Boolean(false)),
            ("(1 > 2) == false", Value::Boolean(true)),
        ];

        test_evaluated_value(tests)
    }
    
    #[test]
    fn can_parse_if_else_expression() -> Result<()> {
        let tests = vec![
            ("if (true) { 10 }", Value::Integer(10)),
            ("if (false) { 10 }", Value::Null),
            ("if (1) { 10 }", Value::Integer(10)),
            ("if (1 < 2) { 10 }", Value::Integer(10)),
            ("if (1 > 2) { 10 }", Value::Null),
            ("if (1 > 2) { 10 } else { 20 }", Value::Integer(20)),
            ("if (1 < 2) { 10 } else { 20 }", Value::Integer(10)),
        ];

        test_evaluated_value(tests)
    }
}