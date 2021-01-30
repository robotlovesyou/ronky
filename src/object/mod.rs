use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use crate::ast::{Identifier, Statement};
use crate::environment::Environment;
use crate::location::Location;
use std::mem;

pub use boolean::*;
pub use function::*;
pub use integer::*;
pub use null::*;
pub use obj_ref::*;
pub use return_obj::*;
pub use str_obj::*;

mod boolean;
mod function;
mod integer;
mod null;
mod obj_ref;
mod return_obj;
mod str_obj;

const LEN_FN_STR: &str = "len(arg){}";

#[derive(Debug)]
pub struct Error {
    message: String,
}

#[allow(dead_code)]
impl Error {
    fn new(message: String) -> Error {
        Error { message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Object {
    kind: ObjectKind,
    location: Location,
}

impl Object {
    pub fn new(kind: ObjectKind, location: Location) -> Object {
        Object { kind, location }
    }

    pub fn kind(&self) -> &ObjectKind {
        match &self.kind {
            ObjectKind::ObjRef(kind) => kind.ptr.kind(),
            _ => &self.kind,
        }
    }

    pub fn location(&self) -> Location {
        self.location
    }

    /// Takes ownership of the Object.kind value, replacing the internal with Null.
    /// Used with Return objects to allow the wrapped value to be extracted
    pub fn try_kind_owned(&mut self) -> Result<ObjectKind> {
        match &self.kind {
            // panic is appropriate here since this would indicate a logic error in the interpreter
            ObjectKind::ObjRef(_) => panic!("cannot take ownership of an object ref"),
            _ => Ok(mem::replace(&mut self.kind, ObjectKind::Null(Null))),
        }
    }

    pub fn inspect(&self) -> String {
        match &self.kind {
            ObjectKind::Integer(kind) => kind.inspect(),
            ObjectKind::Boolean(kind) => kind.inspect(),
            ObjectKind::Null(kind) => kind.inspect(),
            ObjectKind::Return(kind) => kind.inspect(),
            ObjectKind::ObjRef(kind) => kind.inspect(),
            ObjectKind::Function(kind) => kind.inspect(),
            ObjectKind::Str(kind) => kind.inspect(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ObjectKind::Integer(kind) => kind.fmt(f),
            ObjectKind::Boolean(kind) => kind.fmt(f),
            ObjectKind::Null(kind) => kind.fmt(f),
            ObjectKind::Return(kind) => kind.fmt(f),
            ObjectKind::ObjRef(kind) => kind.fmt(f),
            ObjectKind::Function(kind) => std::fmt::Display::fmt(&kind, f),
            ObjectKind::Str(kind) => std::fmt::Display::fmt(&kind, f),
        }
    }
}

pub trait Inspectable<T: ?Sized>
where
    T: Display,
{
    fn value(&self) -> &T;

    fn inspect(&self) -> String {
        format!("{}", self.value())
    }
}

#[derive(Debug)]
pub enum ObjectKind {
    Integer(Integer),
    Boolean(Boolean),
    Null(Null),
    Return(Return),
    ObjRef(ObjRef),
    Function(Function),
    Str(Str),
}

impl ObjectKind {
    pub fn name(&self) -> &'static str {
        match &self {
            ObjectKind::Integer(_) => "Integer",
            ObjectKind::Boolean(_) => "Boolean",
            ObjectKind::Null(_) => "Null",
            ObjectKind::Return(_) => "Return",
            ObjectKind::ObjRef(_) => "ObjRef",
            ObjectKind::Function(_) => "Function",
            ObjectKind::Str(_) => "String",
        }
    }
}
