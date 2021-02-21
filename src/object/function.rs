use crate::ast::{Identifier, Statement};
use crate::environment::Environment;
use crate::location::Location;
use crate::object::{Error, Integer, Object, ObjectKind, Result};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub enum Function {
    User(UserFunction),
    Builtin(Builtin),
}

impl Display for Function {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self {
            Function::User(kind) => std::fmt::Display::fmt(&kind, f),
            Function::Builtin(kind) => std::fmt::Display::fmt(&kind, f),
        }
    }
}

#[derive(Debug)]
pub enum Builtin {
    Len,
}

impl Builtin {
    pub fn new_len(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::Len)),
            location,
        )
    }
    pub fn apply(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        match self {
            Builtin::Len => self.apply_len(arguments, location),
        }
    }

    fn apply_len(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        if arguments.len() != 1 {
            Err(Error::new(format!(
                "wrong number of arguments. got {}, want 1 at {}",
                arguments.len(),
                location
            )))
        } else {
            match &arguments[0].kind {
                ObjectKind::Str(s) => Ok(Integer::new_integer_object(
                    s.value().len() as i64,
                    location,
                )),
                other => Err(Error::new(format!(
                    "argument to `len` not supported, got {} at {}",
                    other.name(),
                    location
                ))),
            }
        }
    }
}

impl Display for Builtin {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "builtin function")
    }
}

#[derive(Debug)]
pub struct UserFunction {
    parameters: Vec<Identifier>,
    body: Statement,
    env: Environment,
}

impl UserFunction {
    pub fn new_user_function_object(
        parameters: Vec<Identifier>,
        body: Statement,
        env: Environment,
    ) -> Object {
        let location = env.location();

        Object::new(
            ObjectKind::Function(Function::User(UserFunction {
                parameters,
                body,
                env,
            })),
            location,
        )
    }

    pub fn parameters(&self) -> &[Identifier] {
        &self.parameters
    }

    pub fn body(&self) -> &Statement {
        &self.body
    }

    pub fn env(&self) -> &Environment {
        &self.env
    }
}

impl Display for UserFunction {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let parameters = self
            .parameters
            .iter()
            .map(|p| format!("{}", p))
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn ({}) {}", parameters, self.body)
    }
}
