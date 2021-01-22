use crate::ast::{
    BooleanExpression, Expression, ExpressionKind, ExpressionStatement, IfExpression,
    InfixOperator, IntegerLiteralExpression, PrefixExpression, PrefixOperator, Program, Statement,
    StatementKind,
};
use crate::environment::Environment;
use crate::location::Location;
use crate::object::{
    self, Boolean, Inspectable, Integer, Null, ObjRef, Object, ObjectKind, Return,
};
use crate::{environment, parser};

use std::borrow::Borrow;
use std::collections::HashMap;
use std::fmt::{self, Debug, Display, Formatter};
use std::os::macos::raw::stat;
use std::rc::Rc;
use std::result;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
}

impl Error {
    pub fn new(kind: ErrorKind) -> Error {
        Error { kind }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

#[derive(Debug)]
pub struct ParserError {
    message: String,
}

impl ParserError {
    fn new_parser_error(e: parser::Error) -> Error {
        Error::new(ErrorKind::Parser(ParserError {
            message: format!("parser error: {}", e),
        }))
    }
}

impl Display for ParserError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.message)
    }
}

#[derive(Debug)]
pub struct EvaluationError {
    message: String,
}

impl EvaluationError {
    fn new_evaluation_error(message: String) -> Error {
        Error::new(ErrorKind::Evaluation(EvaluationError { message }))
    }
}

impl Display for EvaluationError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error: {}", self.message)
    }
}

impl From<object::Error> for Error {
    fn from(e: object::Error) -> Self {
        EvaluationError::new_evaluation_error(format!("{}", e))
    }
}

impl From<environment::Error> for Error {
    fn from(e: environment::Error) -> Self {
        EvaluationError::new_evaluation_error(format!("{}", e))
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    Parser(ParserError),
    Evaluation(EvaluationError),
}

impl Display for ErrorKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            ErrorKind::Parser(kind) => write!(f, "{}", kind),
            ErrorKind::Evaluation(kind) => write!(f, "{}", kind),
        }
    }
}

type Result<T> = result::Result<T, Error>;

pub trait Evaluable {
    fn evaluate(&self, env: &mut Environment) -> Result<Object>;
}

impl Evaluable for Program {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Null::new_null_object(Location::default());

        'evaluation: for statement in self.statements().iter() {
            env.set_location(statement.location());
            result = statement.evaluate(env)?;

            if let ObjectKind::Return(_) = result.kind() {
                if let ObjectKind::Return(ret) = result.try_kind_owned()? {
                    result = ret.consume();
                    break 'evaluation;
                }
                unreachable!();
            }
        }
        Ok(result)
    }
}

impl Evaluable for &[Statement] {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        let mut result = Null::new_null_object(env.location());

        'evaluation: for statement in self.iter() {
            env.set_location(statement.location());
            result = statement.evaluate(env)?;
            if matches!(result.kind(), ObjectKind::Return(_)) {
                break 'evaluation;
            }
        }
        Ok(result)
    }
}

impl Evaluable for &Statement {
    fn evaluate(&self, env: &mut Environment) -> Result<Object> {
        match self.kind() {
            StatementKind::Let(kind) => {
                let location = self.location();
                let value = kind.value().evaluate(env)?;
                env.set(kind.name().name().to_string(), value)?;
                Ok(Null::new_null_object(location))
            }
            StatementKind::Return(kind) => {
                let location = self.location();
                let value = kind.value().evaluate(env)?;
                Ok(Return::new_return_object(value, location))
            }
            StatementKind::Expression(kind) => kind.evaluate(env),
            StatementKind::Block(kind) => kind.statements().evaluate(env),
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
        env.set_location(self.location());
        match self.kind() {
            ExpressionKind::Boolean(kind) => evaluate_bool_literal_expresson(kind, env.location()),

            ExpressionKind::Identifier(kind) => Ok(env.get(kind.name(), env.location())?),

            ExpressionKind::IntegerLiteral(kind) => {
                evaluate_integer_literal_expression(kind, env.location())
            }

            ExpressionKind::Prefix(kind) => {
                let location = env.location();
                let right = kind.right().evaluate(env)?;
                evaluate_prefix_expression(kind.operator(), &right, location)
            }

            ExpressionKind::Infix(kind) => {
                let location = env.location();
                let left = kind.left().evaluate(env)?;
                let right = kind.right().evaluate(env)?;
                evaluate_infix_expression(kind.operator(), &left, &right, location)
            }

            ExpressionKind::If(kind) => evaluate_if_expression(kind, env),

            ExpressionKind::FunctionLiteral(_) => unimplemented!(),
            ExpressionKind::Call(_) => unimplemented!(),
        }
    }
}

fn evaluate_integer_literal_expression(
    expression: &IntegerLiteralExpression,
    location: Location,
) -> Result<Object> {
    Ok(Integer::new_integer_object(expression.value(), location))
}

fn evaluate_bool_literal_expresson(
    expression: &BooleanExpression,
    location: Location,
) -> Result<Object> {
    Ok(Boolean::new_boolean_object(expression.value(), location))
}

fn evaluate_prefix_expression(
    operator: PrefixOperator,
    right: &Object,
    location: Location,
) -> Result<Object> {
    match operator {
        PrefixOperator::Minus => eval_prefix_neg_expression(right, location),
        PrefixOperator::Not => eval_prefix_not_expression(right, location),
    }
}

fn eval_prefix_not_expression(value: &Object, location: Location) -> Result<Object> {
    match value.kind() {
        ObjectKind::Integer(integer) => {
            Ok(Boolean::new_boolean_object(*integer.value() == 0, location))
        }
        ObjectKind::Boolean(boolean) => {
            Ok(Boolean::new_boolean_object(!(*boolean.value()), location))
        }
        ObjectKind::Null(_) => Ok(Boolean::new_boolean_object(false, location)),
        _ => Err(EvaluationError::new_evaluation_error(format!(
            "unknown operator: {} {} at {}",
            PrefixOperator::Not,
            value.kind().name(),
            location,
        ))),
    }
}

fn eval_prefix_neg_expression(value: &Object, location: Location) -> Result<Object> {
    match value.kind() {
        ObjectKind::Integer(integer) => {
            Ok(Integer::new_integer_object(-(*integer.value()), location))
        }
        _ => Err(EvaluationError::new_evaluation_error(format!(
            "unknown operator: {} {} at {}",
            PrefixOperator::Minus,
            value.kind().name(),
            location,
        ))),
    }
}

fn evaluate_infix_expression(
    operator: InfixOperator,
    left: &Object,
    right: &Object,
    location: Location,
) -> Result<Object> {
    match (left.kind(), right.kind()) {
        (ObjectKind::Integer(a), ObjectKind::Integer(b)) => {
            evaluate_integer_infix_expression(operator, a, b, location)
        }
        (ObjectKind::Boolean(a), ObjectKind::Boolean(b)) => {
            evaluate_boolean_infix_expression(operator, a, b, location)
        }
        _ => Err(EvaluationError::new_evaluation_error(format!(
            "type mismatch: {} {} {} at {}",
            left.kind().name(),
            operator,
            right.kind().name(),
            location,
        ))),
    }
}

fn evaluate_integer_infix_expression(
    operator: InfixOperator,
    left: &Integer,
    right: &Integer,
    location: Location,
) -> Result<Object> {
    Ok(match operator {
        InfixOperator::Add => Integer::new_integer_object(left.value() + right.value(), location),
        InfixOperator::Subtract => {
            Integer::new_integer_object(left.value() - right.value(), location)
        }
        InfixOperator::Multiply => {
            Integer::new_integer_object(left.value() * right.value(), location)
        }
        InfixOperator::Divide => {
            Integer::new_integer_object(left.value() / right.value(), location)
        }
        InfixOperator::GreaterThan => {
            Boolean::new_boolean_object(left.value() > right.value(), location)
        }
        InfixOperator::LessThan => {
            Boolean::new_boolean_object(left.value() < right.value(), location)
        }
        InfixOperator::Equals => {
            Boolean::new_boolean_object(left.value() == right.value(), location)
        }
        InfixOperator::NotEquals => {
            Boolean::new_boolean_object(left.value() != right.value(), location)
        }
    })
}

fn evaluate_boolean_infix_expression(
    operator: InfixOperator,
    left: &Boolean,
    right: &Boolean,
    location: Location,
) -> Result<Object> {
    match operator {
        InfixOperator::Equals => Ok(Boolean::new_boolean_object(
            left.value() == right.value(),
            location,
        )),
        InfixOperator::NotEquals => Ok(Boolean::new_boolean_object(
            left.value() != right.value(),
            location,
        )),
        _ => Err(EvaluationError::new_evaluation_error(format!(
            "unknown operator: Boolean {} Boolean at {}",
            operator, location
        ))),
    }
}

fn evaluate_if_expression(expression: &IfExpression, env: &mut Environment) -> Result<Object> {
    let condition = expression.condition().evaluate(env)?;
    if is_truthy(&condition, expression.condition().location())? {
        expression.consequence().evaluate(env)
    } else {
        if let Some(alternative) = expression.alternative() {
            alternative.evaluate(env)
        } else {
            Ok(Null::new_null_object(env.location()))
        }
    }
}

fn is_truthy(object: &Object, location: Location) -> Result<bool> {
    match object.kind() {
        ObjectKind::Integer(kind) => Ok(*kind.value() != 0),
        ObjectKind::Boolean(kind) => Ok(*kind.value()),
        ObjectKind::Null(_) => Ok(false),
        _ => Err(EvaluationError::new_evaluation_error(format!(
            "type mismatch; {} cannot be evaluated for truthiness at {}",
            object.kind().name(),
            location
        ))),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::IntoTokens;
    use crate::parser::Parser;
    use crate::source::ToSource;

    use indoc::indoc;

    #[derive(Debug)]
    enum Value {
        Integer(i64),
        Boolean(bool),
        Null,
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
            (ObjectKind::Integer(integer), Value::Integer(expected)) => {
                assert_eq!(&expected, integer.value(), "{}", source)
            }
            (ObjectKind::Boolean(boolean), Value::Boolean(expected)) => {
                assert_eq!(&expected, boolean.value(), "{}", source)
            }
            (ObjectKind::Null(_), Value::Null) => {} // do nothing, these are always equal
            other => panic!("{:?} is not a valid option with source {}", other, source),
        }
    }

    fn test_evaluated_value(tests: Vec<(&str, Value)>) -> Result<()> {
        for (source, expected) in tests {
            let program = program_from_source(source)?;
            let mut env = Environment::default();
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
            ("false", Value::Boolean(false)),
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

    #[test]
    fn can_evaluate_return_statements() -> Result<()> {
        let nested_1 = indoc! {"
        if (10 > 1) {
          if (10 > 1) {
            return 10;
          }
          return 1;
        }"};

        let tests = vec![
            ("return 10;", Value::Integer(10)),
            ("return 10; 9;", Value::Integer(10)),
            ("return 2 * 5; 9;", Value::Integer(10)),
            ("9; return 2 * 5; 9;", Value::Integer(10)),
            ("if (10 > 1) { return 10; }", Value::Integer(10)),
            (nested_1, Value::Integer(10)),
        ];

        test_evaluated_value(tests)
    }

    #[test]
    fn returns_expected_error_message() -> Result<()> {
        let complex_1 = indoc! {"
        if (10 > 1) {
                if (10 > 1) {
                    return true + false;
                }

                return 1;
            }
        "};

        let tests = vec![
            (
                "5 + true;",
                "Error: type mismatch: Integer + Boolean at line: 1 column: 1",
            ),
            (
                "5 + true; 5;",
                "Error: type mismatch: Integer + Boolean at line: 1 column: 1",
            ),
            (
                "-true",
                "Error: unknown operator: - Boolean at line: 1 column: 1",
            ),
            (
                "true + false;",
                "Error: unknown operator: Boolean + Boolean at line: 1 column: 1",
            ),
            (
                "true + false + true + false;",
                "Error: unknown operator: Boolean + Boolean at line: 1 column: 1",
            ),
            (
                "5; true + false; 5",
                "Error: unknown operator: Boolean + Boolean at line: 1 column: 4",
            ),
            (
                "if (10 > 1) { true + false; }",
                "Error: unknown operator: Boolean + Boolean at line: 1 column: 15",
            ),
            (
                complex_1,
                "Error: unknown operator: Boolean + Boolean at line: 3 column: 20",
            ),
        ];

        for (source, error) in tests {
            let program = program_from_source(source)?;
            let mut env = Environment::default();
            let result = program.evaluate(&mut env);
            match result {
                Ok(_) => panic!("program should fail to evaluate: `{}`", source),
                Err(e) => assert_eq!(error, format!("{}", e).as_str()),
            }
        }

        Ok(())
    }

    #[test]
    fn can_parse_let_statements_and_identifiers() -> Result<()> {
        let tests = vec![
            ("let a = 5; a;", Value::Integer(5)),
            ("let a = 5 * 5; a;", Value::Integer(25)),
            ("let a = 5; let b = a; b;", Value::Integer(5)),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Value::Integer(15),
            ),
        ];

        test_evaluated_value(tests)
    }
}
