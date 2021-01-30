use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind};

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

impl Inspectable<Object> for Return {
    fn value(&self) -> &Object {
        &self.value
    }
}
