use crate::location::Location;
use crate::object::{ObjRef, Object};
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::rc::Rc;
use std::{fmt, result};

#[derive(Debug)]
pub struct Error {
    message: String,
}

impl Error {
    pub fn new(message: String) -> Error {
        Error { message }
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

type Result<T> = result::Result<T, Error>;

#[derive(Debug)]
struct EnvironmentState {
    parent: Option<Environment>,
    location: Location,
    bindings: HashMap<String, Rc<Object>>,
}

impl Default for EnvironmentState {
    fn default() -> Self {
        EnvironmentState {
            parent: None,
            location: Location::default(),
            bindings: HashMap::new(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Environment {
    state: Rc<RefCell<EnvironmentState>>,
}

impl Default for Environment {
    fn default() -> Self {
        Environment {
            state: Rc::new(RefCell::new(EnvironmentState::default())),
        }
    }
}

impl Environment {
    pub fn set_location(&mut self, location: Location) {
        self.state.borrow_mut().location = location;
    }

    pub fn location(&self) -> Location {
        self.state.borrow().location
    }

    pub fn get(&self, name: &str, location: Location) -> Result<Object> {
        match self.state.borrow().bindings.get(name) {
            None => match &self.state.borrow().parent {
                None => Err(Error::new(format!(
                    "name not found: {} at {}",
                    name, location
                ))),
                Some(parent) => parent.get(name, location),
            },
            Some(obj) => Ok(ObjRef::new_obj_ref(obj.clone(), location)),
        }
    }

    pub fn set(&mut self, name: String, object: Object) -> Result<Object> {
        let location = object.location();
        let obj_ref = Rc::new(object);
        self.state
            .borrow_mut()
            .bindings
            .insert(name, obj_ref.clone());
        Ok(ObjRef::new_obj_ref(obj_ref, location))
    }

    pub fn closed(&self) -> Environment {
        Environment {
            state: Rc::new(RefCell::new(EnvironmentState {
                parent: Some(self.clone()),
                location: self.state.borrow().location,
                bindings: HashMap::new(),
            })),
        }
    }
}
