use std::io::Write;
use std::{error, io};

use crate::lexer::IntoTokens;
use crate::source::ToSource;
use core::result;

const PROMPT: &str = ">> ";

pub fn start(stdin: io::Stdin, stdout: &mut io::Stdout) {
    let mut line = String::new();
    loop {
        write(stdout, PROMPT);
        match read(&stdin, &mut line) {
            Ok(_) => {
                let tokens = line.as_str().to_source().into_tokens();
                for token in tokens {
                    write(stdout, format!("{:?}\n", token).as_str());
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

fn write(stdout: &mut io::Stdout, s: &str) {
    stdout.write_all(s.as_bytes()).unwrap();
    stdout.flush().unwrap();
}

fn read(stdin: &io::Stdin, line: &mut String) -> result::Result<(), Box<dyn error::Error>> {
    match stdin.read_line(line) {
        Ok(_) => Ok(()),
        Err(e) => Err(Box::new(e)),
    }
}
