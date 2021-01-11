use std::result;
use crate::ast::*;
use crate::lexer::*;
use crate::token::*;
use std::iter::Peekable;

pub struct Error {

}

pub type Result<T> = result::Result<T, Error>;


struct Parser {
    lexer: Peekable<Lexer>,
}

impl Parser {
    fn new(lexer: Lexer) -> Parser {
        Parser{lexer: lexer.peekable()}
    }

    pub fn parse() -> Result<Program> {
        unimplemented!()
    }
}