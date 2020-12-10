#[derive(Copy, Clone)]
pub struct SourceChar {
    pub repr: char,
    pub line: usize,
    pub column: usize,
}

impl SourceChar {
    pub fn new(line: usize, column: usize, repr: char) -> SourceChar {
        SourceChar{line, column, repr}
    }
}

pub struct SourceChars {
    chars: Vec<char>,
    next_line: usize,
    next_column: usize,
}

impl SourceChars {
    pub fn new(source: &str) -> SourceChars {
        SourceChars {
            chars: source.chars().rev().collect(),
            next_line: 1,
            next_column: 1,
        }
    }
}

impl Iterator for SourceChars {
    type Item = SourceChar;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(c) = self.chars.pop() {
            let next = match c {
                '\n' => {
                    self.next_line += 1;
                    self.next_column = 1;
                    None
                },
                repr => {
                    let sc = Some(SourceChar::new(self.next_line, self.next_column, repr));
                    self.next_column += 1;
                    sc
                }
            };
            if next.is_some() {
                return next;
            }
        }
        None
    }
}

pub trait ToSource {
    fn to_source(&self) -> SourceChars;
}

impl ToSource for &str {
    fn to_source(&self) -> SourceChars {
        SourceChars::new(self)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    use indoc::indoc;

    const TEST_SOURCE: &'static str = indoc!{"\
    ab
    cd"};

    #[test]
    fn produces_expected_characters() {
        let mut iter = TEST_SOURCE.to_source();
        assert!(matches!(iter.next(), Some(SourceChar{line: 1, column: 1, repr: 'a'})));
        assert!(matches!(iter.next(), Some(SourceChar{line: 1, column: 2, repr: 'b'})));
        assert!(matches!(iter.next(), Some(SourceChar{line: 2, column: 1, repr: 'c'})));
        assert!(matches!(iter.next(), Some(SourceChar{line: 2, column: 2, repr: 'd'})));
    }
}