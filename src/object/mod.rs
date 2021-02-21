use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use crate::ast::{Identifier, Statement};
use crate::environment::Environment;
use crate::location::Location;
use std::mem;

pub use array_obj::*;
pub use boolean::*;
pub use function::*;
pub use integer::*;
pub use null::*;
pub use obj_ref::*;
pub use return_obj::*;
pub use str_obj::*;

mod array_obj;
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
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ObjectKind::Integer(kind) => write!(f, "{}", kind),
            ObjectKind::Boolean(kind) => write!(f, "{}", kind),
            ObjectKind::Null(kind) => write!(f, "{}", kind),
            ObjectKind::Return(kind) => write!(f, "{}", kind),
            ObjectKind::ObjRef(kind) => write!(f, "{}", kind),
            ObjectKind::Function(kind) => write!(f, "{}", kind),
            ObjectKind::Str(kind) => write!(f, "{}", kind),
            ObjectKind::Array(kind) => write!(f, "{}", kind),
        }
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
    Array(Array),
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
            ObjectKind::Array(_) => "Array",
        }
    }
}
