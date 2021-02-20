use crate::location::Location;
use crate::object::{Error, Inspectable, ObjRef, Object, ObjectKind, Result};
use std::fmt::{self, Display, Formatter};
use std::ops::{Index, IndexMut};
use std::rc::Rc;

#[derive(Debug)]
pub struct Array {
    elements: Vec<Rc<Object>>,
}

impl Array {
    pub fn new_array_obj(elements: Vec<Object>, location: Location) -> Object {
        let ref_elements = elements.into_iter().map(|e| Rc::new(e)).collect();

        Object::new(
            ObjectKind::Array(Array {
                elements: ref_elements,
            }),
            location,
        )
    }

    pub fn len(&self) -> usize {
        self.elements.len()
    }

    pub fn at(&self, idx: usize) -> Result<Object> {
        if idx <= self.elements.len() {
            Ok(ObjRef::new_obj_ref(
                self.elements[idx].clone(),
                self.elements[idx].location(),
            ))
        } else {
            Err(Error::new(format!("index {} is out of range", idx)))
        }
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

impl Inspectable<Array> for Array {
    fn value(&self) -> &Array {
        &self
    }

    fn inspect(&self) -> String {
        format!("{}", self)
    }
}
