use std::fmt::{self, Display, Formatter};

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Location(pub usize, pub usize);

impl Default for Location {
    fn default() -> Self {
        Location(0, 0)
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "line: {} column: {}", self.line(), self.column())
    }
}

impl Location {
    pub fn new(line: usize, column: usize) -> Location {
        Location(line, column)
    }
    pub fn line(&self) -> usize {
        self.0
    }

    pub fn column(&self) -> usize {
        self.1
    }
}