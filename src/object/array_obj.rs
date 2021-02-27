use crate::location::Location;
use crate::object::{Error, Null, ObjRef, Object, ObjectKind, Result};
use std::fmt::{self, Display, Formatter};
use std::rc::Rc;

#[derive(Debug)]
pub struct Array {
    elements: Vec<Rc<Object>>,
}

impl Array {
    pub fn new_array_obj(elements: Vec<Object>, location: Location) -> Object {
        let ref_elements = elements.into_iter().map(Rc::new).collect();

        Object::new(
            ObjectKind::Array(Array {
                elements: ref_elements,
            }),
            location,
        )
    }

    pub fn len(&self) -> i64 {
        self.elements.len() as i64
    }

    // In ronky arrays are indexed with Integers which can be negative
    pub fn at(&self, idx: i64) -> Result<Object> {
        if idx < self.elements.len() as i64 && idx >= 0 {
            Ok(ObjRef::new_obj_ref(
                self.elements[idx as usize].clone(),
                self.elements[idx as usize].location(),
            ))
        } else {
            Err(Error::new(format!("index {} is out of range", idx)))
        }
    }

    pub fn tail(&self, location: Location) -> Object {
        if self.elements.len() > 1 {
            let ref_elements: Vec<Rc<Object>> = self.elements[1..].to_vec();
            Object::new(
                ObjectKind::Array(Array {
                    elements: ref_elements,
                }),
                location,
            )
        } else {
            Null::new_null_object(location)
        }
    }

    pub fn push(&self, item: Object, location: Location) -> Object {
        let mut ref_elements: Vec<Rc<Object>> = self.elements.to_vec();
        ref_elements.push(Rc::new(item));
        Object::new(
            ObjectKind::Array(Array {
                elements: ref_elements,
            }),
            location,
        )
    }
}

impl Display for Array {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        if self.elements.is_empty() {
            write!(f, "[]")
        } else {
            write!(f, "[")?;
            for el in self.elements.iter().take(self.elements.len() - 1) {
                let ref_el = ObjRef::new_obj_ref(el.clone(), el.location());
                write!(f, "{},", ref_el)?;
            }
            let last = self.elements.last().expect("elements is empty");
            let last_ref = ObjRef::new_obj_ref(last.clone(), last.location());
            write!(f, "{}]", last_ref)
        }
    }
}
