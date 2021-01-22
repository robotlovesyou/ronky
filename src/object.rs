use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use crate::location::Location;
use lazy_static::lazy_static;
use std::mem;

const TRUE: bool = true;
const FALSE: bool = false;
const NULL: NullValue = NullValue;

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl Error {
    fn new(message: String) -> Error {
        Error { message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Object Error: {}", self.message)
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
        // TODO this should fail if trying to take ownership of a ref kind
        Ok(mem::replace(&mut self.kind, ObjectKind::Null(Null)))
    }

    pub fn inspect(&self) -> String {
        match &self.kind {
            ObjectKind::Integer(integer) => integer.inspect(),
            ObjectKind::Boolean(boolean) => boolean.inspect(),
            ObjectKind::Null(null) => null.inspect(),
            ObjectKind::Return(rtrn) => rtrn.inspect(),
            ObjectKind::ObjRef(obj_ref) => obj_ref.inspect(),
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
        }
    }
}

pub trait Inspectable<T>
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
}

impl ObjectKind {
    pub fn name(&self) -> &'static str {
        match &self {
            ObjectKind::Integer(_) => "Integer",
            ObjectKind::Boolean(_) => "Boolean",
            ObjectKind::Null(_) => "Null",
            ObjectKind::Return(_) => "Return",
            ObjectKind::ObjRef(_) => "ObjRef",
        }
    }
}

#[derive(Debug)]
pub struct Integer {
    value: i64,
}

impl Integer {
    pub fn new_integer_object(value: i64, location: Location) -> Object {
        Object::new(ObjectKind::Integer(Integer { value }), location)
    }
}

impl Inspectable<i64> for Integer {
    fn value(&self) -> &i64 {
        &self.value
    }
}

#[derive(Debug)]
pub enum Boolean {
    True,
    False,
}

impl Boolean {
    pub fn new_boolean_object(value: bool, location: Location) -> Object {
        let kind = match value {
            true => Boolean::True,
            false => Boolean::False,
        };
        Object::new(ObjectKind::Boolean(kind), location)
    }
}

impl Inspectable<bool> for Boolean {
    fn value(&self) -> &bool {
        match self {
            Boolean::True => &TRUE,
            Boolean::False => &FALSE,
        }
    }
}

#[derive(Debug)]
pub struct NullValue;

impl Display for NullValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}

#[derive(Debug)]
pub struct Null;

impl Null {
    pub fn new_null_object(location: Location) -> Object {
        Object::new(ObjectKind::Null(Null), location)
    }
}

impl Inspectable<NullValue> for Null {
    fn value(&self) -> &NullValue {
        &NULL
    }
}

#[derive(Debug)]
pub struct Return {
    value: Box<Object>,
}

impl Return {
    pub fn new_return_object(value: Object, location: Location) -> Object {
        Object::new(
            ObjectKind::Return(Return {
                value: Box::new(value),
            }),
            location,
        )
    }

    /// Consume this return value and extract the wrapped Object.
    pub fn consume(mut self) -> Object {
        *self.value
    }
}

impl Inspectable<Object> for Return {
    fn value(&self) -> &Object {
        &self.value()
    }
}

#[derive(Debug)]
pub struct ObjRef {
    ptr: Rc<Object>,
}

impl ObjRef {
    pub fn new_obj_ref(obj_ref: Rc<Object>, location: Location) -> Object {
        Object::new(ObjectKind::ObjRef(ObjRef { ptr: obj_ref }), location)
    }
}

impl Inspectable<Object> for ObjRef {
    fn value(&self) -> &Object {
        &self.ptr
    }

    fn inspect(&self) -> String {
        self.ptr.inspect()
    }
}
