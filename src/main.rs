mod ast;
mod evaluator;
mod lexer;
mod location;
mod object;
mod parser;
mod repl;
mod source;
mod token;

use std::io;

fn main() {
    repl::start(io::stdin(), &mut io::stdout(), &mut io::stderr());
}
