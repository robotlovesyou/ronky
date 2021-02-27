use crate::location::Location;
use crate::object::{Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

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

impl Display for Null {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "NULL")
    }
}
