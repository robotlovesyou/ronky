use std::fmt::{self, Display, Formatter};

const TRUE: bool = true;
const FALSE: bool = false;

pub struct Object {
    kind: ObjectKind
}

impl Object {
    pub fn new(kind: ObjectKind) -> Object {
        Object{kind}
    }

    pub fn kind(&self) -> &ObjectKind {
        &self.kind
    }

    pub fn inspect(&self) -> String {
        match &self.kind {
            ObjectKind::Integer(integer) => integer.inspect(),
            ObjectKind::Boolean(boolean) => boolean.inspect(),
            ObjectKind::Null(null) => null.inspect(),
        }
    }
}

pub trait Inspectable<T> where T: Display {
    fn value(&self) -> &T;

    fn inspect(&self) -> String {
        format!("{}", self.value())
    }
}

pub enum ObjectKind {
    Integer(Integer),
    Boolean(Boolean),
    Null(&'static Null),
}

pub struct Integer {
    pub value: i64
}

impl Integer {
    pub fn new_integer_object(value: i64) -> Object {
        Object::new(ObjectKind::Integer(Integer {
            value
        }))
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
    pub fn new_boolean_object(value: bool) -> Object {
        let kind = match value {
            true => Boolean::True,
            false => Boolean::False,
        };
        Object::new(ObjectKind::Boolean(kind))
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

pub struct NullValue;

impl Display for NullValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "null")
    }
}

pub struct Null {
    value: NullValue,
}

impl Inspectable<NullValue> for Null {
    fn value(&self) -> &NullValue {
        &self.value
    }
}
