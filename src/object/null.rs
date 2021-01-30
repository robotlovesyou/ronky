use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

const NULL: NullValue = NullValue;

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
