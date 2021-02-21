use std::fmt::{self, Display, Formatter};

pub fn display_parameter_list<T>(params: &[T], f: &mut Formatter) -> fmt::Result
where
    T: Display,
{
    write!(f, "(")?;
    if !params.is_empty() {
        for param in params.iter().take(params.len() - 1) {
            write!(f, "{}, ", param)?;
        }
        write!(f, "{}", params.last().expect("no final parameter"))?;
    }
    write!(f, ")")
}
