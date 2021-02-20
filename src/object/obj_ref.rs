use crate::location::Location;
use crate::object::{Inspectable, Object, ObjectKind};
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct ObjRef {
    pub ptr: Rc<Object>,
}

impl ObjRef {
    pub fn new_obj_ref(obj_ref: Rc<Object>, location: Location) -> Object {
        Object::new(ObjectKind::ObjRef(ObjRef { ptr: obj_ref }), location)
    }
}

impl Inspectable<Object> for ObjRef {
    fn value(&self) -> &Object {
        &self.ptr
    }

    fn inspect(&self) -> String {
        self.ptr.inspect()
    }
}
