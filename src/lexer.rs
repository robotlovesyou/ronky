#[cfg(test)]
use crate::source::{SourceChar, SourceChars, ToSource};
use crate::token::{self, TokenKind, Token};
use std::iter::Peekable;
use crate::token::TokenKind::ILLEGAL;

pub struct Lexer {
    source: Peekable<SourceChars>,
}

impl Lexer {
    pub fn new(source: SourceChars) -> Lexer {
        Lexer{source: source.peekable()}
    }

    fn peek_check(&mut self, c: char) -> bool {
        self.source.peek().map_or(false, |pc| pc.repr == c)
    }

    fn if_peek_else<T, F>(&mut self, c: char, if_true: T, if_false: F) -> Option<Token> where
    T: Fn() -> Option<Token>,
    F: Fn() -> Option<Token>
    {
        if self.peek_check(c) {
            self.source.next();
            if_true()
        } else {
            if_false()
        }
    }

    fn read_identifier(&mut self, first: &SourceChar) -> Option<Token> {
        let mut ident: String = format!("{}", first.repr);
        while let Some(sc) = self.source.peek() {
            match sc.repr {
                c if is_letter(c) => {
                    ident.push(c);
                    self.source.next();
                    ()
                },
                _ => break,
            }
        }
        new_token(first, token::keyword_to_token(&ident))
    }

    fn read_number(&mut self, first: &SourceChar) -> Option<Token> {
        let mut number: String = format!("{}", first.repr);
        while let Some(sc) = self.source.peek() {
            match sc.repr {
                c if is_digit(c) => {
                    number.push(c);
                    self.source.next();
                    ()
                },
                _ => break
            }
        }
        new_token(first, TokenKind::INT(number))
    }
}

fn new_token(sc: &SourceChar, kind: TokenKind) -> Option<Token> {
    Some(Token::new(sc.line, sc.column, kind))
}

fn is_letter(c: char) -> bool {
    match c {
        'a'..='z' | 'A'..='Z' | '_' => true,
        _ => false
    }
}

fn is_digit(c: char) -> bool {
    match c {
        '0'..='9' => true,
        _ => false
    }
}



impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(sc) = self.source.next() {
            let next = match sc.repr {
                ' ' | '\t' => None,
                '=' => self.if_peek_else('=',
                                      || new_token(&sc, TokenKind::EQ),
                                      || new_token(&sc, TokenKind::ASSIGN)
                    ),
                '+' => new_token(&sc, TokenKind::PLUS),
                '-' => new_token(&sc, TokenKind::MINUS),
                '!' => self.if_peek_else('=',
                        || new_token(&sc, TokenKind::NOT_EQ),
                            || new_token(&sc, TokenKind::BANG)
                        ),
                '/' => new_token(&sc, TokenKind::SLASH),
                '*' => new_token(&sc, TokenKind::ASTERISK),
                '<' => new_token(&sc, TokenKind::LT),
                '>' => new_token(&sc, TokenKind::GT),
                ';' => new_token(&sc, TokenKind::SEMICOLON),
                ',' => new_token(&sc, TokenKind::COMMA),
                '{' => new_token(&sc, TokenKind::LBRACE),
                '}' => new_token(&sc, TokenKind::RBRACE),
                '(' => new_token(&sc, TokenKind::LPAREN),
                ')' => new_token(&sc, TokenKind::RPAREN),
                c if is_letter(c) => self.read_identifier(&sc),
                c if is_digit(c) => self.read_number(&sc),
                illegal => new_token(&sc, ILLEGAL(illegal)),
            };
            if next.is_some() {
                return next;
            }
        }
        None
    }
}

pub trait IntoTokens {
    fn into_tokens(self) -> Lexer;
}

impl IntoTokens for SourceChars {
    fn into_tokens(self) -> Lexer {
        Lexer::new(self)
    }
}

mod tests {
    use super::*;

    use crate::token::{Token, TokenKind};
    use crate::source::{self, SourceChar, SourceChars, ToSource};

    use indoc::indoc;
    use lazy_static::lazy_static;

    const INPUT: &'static str = indoc!{"\
    let five = 5;
    let ten = 10;

    let add = fn(x, y) {
        x + y;
    };

    let result = add(five, ten);
    !-/*5;
    5 < 10 > 5;

    if (5 < 10) {
	    return true;
    } else {
	    return false;
    }

    10 == 10;
    10 != 9;
    "};

    fn expected_tokens() -> Vec<Token> {
        vec![
            Token::new(1, 1, TokenKind::LET),
            Token::new(1, 5, TokenKind::IDENT("five".to_string())),
            Token::new(1, 10, TokenKind::ASSIGN),
            Token::new(1, 12, TokenKind::INT("5".to_string())),
            Token::new(1, 13, TokenKind::SEMICOLON),
            Token::new(2, 1, TokenKind::LET),
            Token::new(2, 5, TokenKind::IDENT("ten".to_string())),
            Token::new(2, 9, TokenKind::ASSIGN),
            Token::new(2, 11, TokenKind::INT("10".to_string())),
            Token::new(2, 13, TokenKind::SEMICOLON),
        ]
    }

    // {token.LET, "let"},
    // {token.IDENT, "five"},
    // {token.ASSIGN, "="},
    // {token.INT, "5"},
    // {token.SEMICOLON, ";"},
    // {token.LET, "let"},
    // {token.IDENT, "ten"},
    // {token.ASSIGN, "="},
    // {token.INT, "10"},
    // {token.SEMICOLON, ";"},
    // {token.LET, "let"},
    // {token.IDENT, "add"},
    // {token.ASSIGN, "="},
    // {token.FUNCTION, "fn"},
    // {token.LPAREN, "("},
    // {token.IDENT, "x"},
    // {token.COMMA, ","},
    // {token.IDENT, "y"},
    // {token.RPAREN, ")"},
    // {token.LBRACE, "{"},
    // {token.IDENT, "x"},
    // {token.PLUS, "+"},
    // {token.IDENT, "y"},
    // {token.SEMICOLON, ";"},
    // {token.RBRACE, "}"},
    // {token.SEMICOLON, ";"},
    // {token.LET, "let"},
    // {token.IDENT, "result"},
    // {token.ASSIGN, "="},
    // {token.IDENT, "add"},
    // {token.LPAREN, "("},
    // {token.IDENT, "five"},
    // {token.COMMA, ","},
    // {token.IDENT, "ten"},
    // {token.RPAREN, ")"},
    // {token.SEMICOLON, ";"},
    // {token.BANG, "!"},
    // {token.MINUS, "-"},
    // {token.SLASH, "/"},
    // {token.ASTERISK, "*"},
    // {token.INT, "5"},
    // {token.SEMICOLON, ";"},
    // {token.INT, "5"},
    // {token.LT, "<"},
    // {token.INT, "10"},
    // {token.GT, ">"},
    // {token.INT, "5"},
    // {token.SEMICOLON, ";"},
    // {token.IF, "if"},
    // {token.LPAREN, "("},
    // {token.INT, "5"},
    // {token.LT, "<"},
    // {token.INT, "10"},
    // {token.RPAREN, ")"},
    // {token.LBRACE, "{"},
    // {token.RETURN, "return"},
    // {token.TRUE, "true"},
    // {token.SEMICOLON, ";"},
    // {token.RBRACE, "}"},
    // {token.ELSE, "else"},
    // {token.LBRACE, "{"},
    // {token.RETURN, "return"},
    // {token.FALSE, "false"},
    // {token.SEMICOLON, ";"},
    // {token.RBRACE, "}"},
    // {token.INT, "10"},
    // {token.EQ, "=="},
    // {token.INT, "10"},
    // {token.SEMICOLON, ";"},
    // {token.INT, "10"},
    // {token.NOT_EQ, "!="},
    // {token.INT, "9"},
    // {token.SEMICOLON, ";"},
    // {token.EOF, ""},

    #[test]
    fn produces_the_expected_tokens() {
        let mut lexer = INPUT.to_source().into_tokens();
        for token in expected_tokens().into_iter() {
            assert_eq!(token, lexer.next().expect("no token in source"));
        }
        panic!("you still need to add all the other tokens!");
    }
}