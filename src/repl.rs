use std::io::Write;
use std::{error, io};

use crate::ast::Program;
use crate::evaluator::{Environment, Evaluable};
use crate::lexer::IntoTokens;
use crate::parser::{Error, Parser};
use crate::source::ToSource;
use core::result;

const PROMPT: &str = ">> ";

pub fn start(stdin: io::Stdin, stdout: &mut io::Stdout, stderr: &mut io::Stderr) {
    let mut line = String::new();
    loop {
        write(stdout, PROMPT);
        match read(&stdin, &mut line) {
            Ok(_) => {
                let mut parser = Parser::new(line.as_str().to_source().into_tokens());
                match parser.parse() {
                    Ok(program) => {
                        let mut env = Environment {};
                        match program.evaluate(&mut env) {
                            Ok(object) => write(stdout, format!("{}\n", object.inspect()).as_str()),
                            Err(e) => write(stderr, format!("{:?}", e).as_str()),
                        }
                    }
                    //write(stdout, format!("{}\n", program).as_str()),
                    Err(e) => write(stderr, format!("{}", e).as_str()),
                }
                line.clear();
            }
            Err(e) => {
                write(stdout, format!("{:?}", e).as_str());
                return;
            }
        }
    }
}

fn write(out: &mut impl io::Write, s: &str) {
    out.write_all(s.as_bytes()).unwrap();
    out.flush().unwrap();
}

fn read(stdin: &io::Stdin, line: &mut String) -> result::Result<(), Box<dyn error::Error>> {
    match stdin.read_line(line) {
        Ok(_) => Ok(()),
        Err(e) => Err(Box::new(e)),
    }
}
