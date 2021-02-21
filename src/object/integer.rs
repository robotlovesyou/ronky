use crate::location::Location;
use crate::object::{Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct Integer {
    value: i64,
}

impl Integer {
    pub fn new_integer_object(value: i64, location: Location) -> Object {
        Object::new(ObjectKind::Integer(Integer { value }), location)
    }

    pub fn value(&self) -> i64 {
        self.value
    }
}

impl Display for Integer {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value)
    }
}
