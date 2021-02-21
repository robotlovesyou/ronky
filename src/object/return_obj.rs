use crate::location::Location;
use crate::object::{Object, ObjectKind};
use std::fmt::{self, Display, Formatter};

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
    pub fn consume(self) -> Object {
        *self.value
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "RETURN")
    }
}
