use crate::location::Location;
use crate::object::{Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct Str {
    value: String,
}

impl Str {
    pub fn new_str_object(value: String, location: Location) -> Object {
        Object::new(ObjectKind::Str(Str { value }), location)
    }

    pub fn value(&self) -> &str {
        &self.value
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "\"{}\"", self.value)
    }
}
