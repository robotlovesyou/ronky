use crate::ast::{Identifier, Statement};
use crate::display::display_parameter_list;
use crate::environment::Environment;
use crate::location::Location;
use crate::object::{Error, Integer, Null, Object, ObjectKind, Result};
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
    First,
    Last,
    Rest,
    Push,
}

fn with_n_arguments<F>(n: usize, arguments: Vec<Object>, location: Location, f: F) -> Result<Object>
where
    F: Fn(Vec<Object>, Location) -> Result<Object>,
{
    if arguments.len() != n {
        Err(Error::new(format!(
            "wrong number of arguments. got {}, want 1 at {}",
            arguments.len(),
            location
        )))
    } else {
        f(arguments, location)
    }
}

impl Builtin {
    pub fn new_len(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::Len)),
            location,
        )
    }

    pub fn new_first(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::First)),
            location,
        )
    }

    pub fn new_last(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::Last)),
            location,
        )
    }

    pub fn new_rest(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::Rest)),
            location,
        )
    }

    pub fn new_push(location: Location) -> Object {
        Object::new(
            ObjectKind::Function(Function::Builtin(Builtin::Push)),
            location,
        )
    }

    pub fn apply(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        match self {
            Builtin::Len => self.apply_len(arguments, location),
            Builtin::First => self.apply_first(arguments, location),
            Builtin::Last => self.apply_last(arguments, location),
            Builtin::Rest => self.apply_rest(arguments, location),
            Builtin::Push => self.apply_push(arguments, location),
        }
    }

    fn apply_len(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        with_n_arguments(
            1,
            arguments,
            location,
            |a: Vec<Object>, l: Location| match a[0].kind() {
                ObjectKind::Str(s) => Ok(Integer::new_integer_object(s.value().len() as i64, l)),
                ObjectKind::Array(array) => Ok(Integer::new_integer_object(array.len() as i64, l)),
                other => Err(Error::new(format!(
                    "argument to `len` not supported, got {} at {}",
                    other.name(),
                    l
                ))),
            },
        )
    }

    fn apply_first(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        with_n_arguments(
            1,
            arguments,
            location,
            |a: Vec<Object>, l: Location| match a[0].kind() {
                ObjectKind::Array(array) => {
                    if array.len() > 0 {
                        array.at(0)
                    } else {
                        Ok(Null::new_null_object(l))
                    }
                }
                other => Err(Error::new(format!(
                    "argument to `first` must be Array, got {} at {}",
                    other.name(),
                    l
                ))),
            },
        )
    }

    fn apply_last(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        with_n_arguments(
            1,
            arguments,
            location,
            |a: Vec<Object>, l: Location| match a[0].kind() {
                ObjectKind::Array(array) => {
                    if array.len() > 0 {
                        array.at(array.len() - 1)
                    } else {
                        Ok(Null::new_null_object(l))
                    }
                }
                other => Err(Error::new(format!(
                    "argument to `last` must be Array, got {} at {}",
                    other.name(),
                    l
                ))),
            },
        )
    }

    fn apply_rest(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        with_n_arguments(
            1,
            arguments,
            location,
            |a: Vec<Object>, l: Location| match a[0].kind() {
                ObjectKind::Array(array) => Ok(array.tail(l)),
                other => Err(Error::new(format!(
                    "argument to `rest` must be Array, got {} at {}",
                    other.name(),
                    l
                ))),
            },
        )
    }

    fn apply_push(&self, arguments: Vec<Object>, location: Location) -> Result<Object> {
        with_n_arguments(2, arguments, location, |mut a: Vec<Object>, l: Location| {
            let object = a.pop().expect("no object to push");
            let target = a.pop().expect("no target to push to");
            match target.kind() {
                ObjectKind::Array(array) => Ok(array.push(object, l)),
                other => Err(Error::new(format!(
                    "first argument to `push` must be Array, got {} at {}",
                    other.name(),
                    l
                ))),
            }
        })
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
        write!(f, "fn ")?;
        display_parameter_list(&self.parameters, f)?;
        write!(f, "){}", self.body)
    }
}
