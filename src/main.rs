mod ast;
mod lexer;
mod parser;
mod repl;
mod source;
mod token;

use std::io;

fn main() {
    repl::start(io::stdin(), &mut io::stdout());
}
