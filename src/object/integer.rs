use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind};

#[derive(Debug)]
pub struct Integer {
    value: i64,
}

impl Integer {
    pub fn new_integer_object(value: i64, location: Location) -> Object {
        Object::new(ObjectKind::Integer(Integer { value }), location)
    }
}

impl Inspectable<i64> for Integer {
    fn value(&self) -> &i64 {
        &self.value
    }
}
