use crate::ast::{Identifier, Statement};
use crate::environment::Environment;
use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind, Result};
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

impl Inspectable<Function> for Function {
    fn value(&self) -> &Function {
        &self
    }

    fn inspect(&self) -> String {
        format!("{}", self)
    }
}

#[derive(Debug)]
pub enum Builtin {
    Len,
}

impl Builtin {
    pub fn apply(arguments: Vec<Object>, location: Location) -> Result<Object> {
        unimplemented!()
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
