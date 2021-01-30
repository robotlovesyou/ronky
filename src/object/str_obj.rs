use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

#[derive(Debug)]
pub struct Str {
    value: String,
}

impl Str {
    pub fn new_str_object(value: String, location: Location) -> Object {
        Object::new(ObjectKind::Str(Str { value }), location)
    }
}

impl Display for Str {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.inspect())
    }
}

impl Inspectable<str> for Str {
    fn value(&self) -> &str {
        &self.value
    }

    fn inspect(&self) -> String {
        format!("\"{}\"", self.value)
    }
}
