mod ast;
mod lexer;
mod parser;
mod repl;
mod source;
mod token;
mod object;
mod evaluator;

use std::io;

fn main() {
    repl::start(io::stdin(), &mut io::stdout(), &mut io::stderr());
}
