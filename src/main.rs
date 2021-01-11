mod lexer;
mod repl;
mod source;
mod token;
mod ast;
mod parser;

use std::io;

fn main() {
    repl::start(io::stdin(), &mut io::stdout());
}
