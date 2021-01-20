use std::fmt::{self, Debug, Display, Formatter};
use std::rc::Rc;

use lazy_static::lazy_static;
use std::mem;

const TRUE: bool = true;
const FALSE: bool = false;
const NULL: NullValue = NullValue;

#[derive(Debug)]
pub struct Object {
    kind: ObjectKind,
    line: usize,
    column: usize,
}

impl Object {
    pub fn new(kind: ObjectKind, line: usize, column: usize) -> Object {
        Object { kind, line, column }
    }

    pub fn kind(&self) -> &ObjectKind {
        &self.kind
    }

    pub fn line(&self) -> usize {
        self.line
    }

    pub fn column(&self) -> usize {
        self.column
    }

    /// Takes ownership of the Object.kind value, replacing the internal with Null.
    /// Used with Return objects to allow the wrapped value to be extracted
    pub fn kind_owned(&mut self) -> ObjectKind {
        mem::replace(&mut self.kind, ObjectKind::Null(Null))
    }

    pub fn inspect(&self) -> String {
        match &self.kind {
            ObjectKind::Integer(integer) => integer.inspect(),
            ObjectKind::Boolean(boolean) => boolean.inspect(),
            ObjectKind::Null(null) => null.inspect(),
            ObjectKind::Return(rtrn) => rtrn.inspect(),
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
}

#[derive(Debug)]
pub struct Integer {
    value: i64,
}

impl Integer {
    pub fn new_integer_object(value: i64, line: usize, column: usize) -> Object {
        Object::new(ObjectKind::Integer(Integer { value }), line, column)
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
    pub fn new_boolean_object(value: bool, line: usize, column: usize) -> Object {
        let kind = match value {
            true => Boolean::True,
            false => Boolean::False,
        };
        Object::new(ObjectKind::Boolean(kind), line, column)
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
    pub fn new_null_object(line: usize, column: usize) -> Object {
        Object::new(ObjectKind::Null(Null), line, column)
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
    pub fn new_return_object(value: Object, line: usize, column: usize) -> Object {
        Object::new(ObjectKind::Return(Return {
            value: Box::new(value),
        }), line, column)
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
