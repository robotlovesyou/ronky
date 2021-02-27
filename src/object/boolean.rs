use crate::location::Location;
use crate::object::{Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

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

    pub fn value(&self) -> bool {
        matches!(self, Boolean::True)
    }
}

impl Display for Boolean {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.value())
    }
}
