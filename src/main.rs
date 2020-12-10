mod source;
mod lexer;
mod repl;
mod token;

use std::io;

fn main() {
    repl::start(io::stdin(), &mut io::stdout());
}
