use crate::source::{SourceChar, SourceChars};
use crate::token::Kind::Illegal;
use crate::token::{self, Kind, Token};
use std::iter::Peekable;

pub struct Lexer {
    source: Peekable<SourceChars>,
}

impl Lexer {
    pub fn new(source: SourceChars) -> Lexer {
        Lexer {
            source: source.peekable(),
        }
    }

    fn peek_check(&mut self, c: char) -> bool {
        self.source.peek().map_or(false, |pc| pc.repr == c)
    }

    fn if_peek_else<T, F>(&mut self, c: char, if_true: T, if_false: F) -> Option<Token>
    where
        T: Fn() -> Option<Token>,
        F: Fn() -> Option<Token>,
    {
        if self.peek_check(c) {
            self.source.next();
            if_true()
        } else {
            if_false()
        }
    }

    fn read_identifier(&mut self, first: &SourceChar) -> Token {
        let mut ident: String = format!("{}", first.repr);
        while let Some(sc) = self.source.peek() {
            match sc.repr {
                c if is_letter(c) => {
                    ident.push(c);
                    self.source.next();
                }
                _ => break,
            }
        }
        new_token(first, token::keyword_to_kind(&ident))
    }

    fn read_number(&mut self, first: &SourceChar) -> Token {
        let mut number: String = format!("{}", first.repr);
        while let Some(sc) = self.source.peek() {
            match sc.repr {
                c if is_digit(c) => {
                    number.push(c);
                    self.source.next();
                }
                _ => break,
            }
        }
        new_token(first, Kind::Int(number))
    }

    fn read_string(&mut self, first: &SourceChar) -> Token {
        let mut value = String::new();
        while let Some(sc) = self.source.next() {
            if sc.repr == '"' {
                break;
            }
            value.push(sc.repr);
        }
        new_token(first, Kind::Str(value))
    }
}

fn new_token(sc: &SourceChar, kind: Kind) -> Token {
    Token::new(sc.location, kind)
}

fn is_letter(c: char) -> bool {
    matches!(c, 'a'..='z' | 'A'..='Z' | '_')
}

fn is_digit(c: char) -> bool {
    matches!(c, '0'..='9')
}

impl Iterator for Lexer {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(sc) = self.source.next() {
            let next = match sc.repr {
                ' ' | '\t' => None,
                '=' => self.if_peek_else(
                    '=',
                    || Some(new_token(&sc, Kind::EQ)),
                    || Some(new_token(&sc, Kind::Assign)),
                ),
                '+' => Some(new_token(&sc, Kind::Plus)),
                '-' => Some(new_token(&sc, Kind::Minus)),
                '!' => self.if_peek_else(
                    '=',
                    || Some(new_token(&sc, Kind::NotEQ)),
                    || Some(new_token(&sc, Kind::Bang)),
                ),
                '/' => Some(new_token(&sc, Kind::Slash)),
                '*' => Some(new_token(&sc, Kind::Asterisk)),
                '<' => Some(new_token(&sc, Kind::LT)),
                '>' => Some(new_token(&sc, Kind::GT)),
                ';' => Some(new_token(&sc, Kind::Semicolon)),
                ',' => Some(new_token(&sc, Kind::Comma)),
                '{' => Some(new_token(&sc, Kind::LBrace)),
                '}' => Some(new_token(&sc, Kind::RBrace)),
                '(' => Some(new_token(&sc, Kind::LParen)),
                ')' => Some(new_token(&sc, Kind::RParen)),
                '[' => Some(new_token(&sc, Kind::LBracket)),
                ']' => Some(new_token(&sc, Kind::RBracket)),
                '"' => Some(self.read_string(&sc)),
                c if is_letter(c) => Some(self.read_identifier(&sc)),
                c if is_digit(c) => Some(self.read_number(&sc)),
                illegal => Some(new_token(&sc, Illegal(illegal))),
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

#[cfg(test)]
mod tests {
    use super::*;

    use crate::source::ToSource;
    use crate::token::{Kind, Token};

    use crate::location::Location;
    use indoc::indoc;

    const INPUT: &'static str = indoc! {"\
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
    \"foobar\"
    \"foo bar\"
    [1,2];
    "};

    fn expected_tokens() -> Vec<Token> {
        vec![
            Token::new(Location::new(1, 1), Kind::Let),
            Token::new(Location::new(1, 5), Kind::Ident("five".to_string())),
            Token::new(Location::new(1, 10), Kind::Assign),
            Token::new(Location::new(1, 12), Kind::Int("5".to_string())),
            Token::new(Location::new(1, 13), Kind::Semicolon),
            Token::new(Location::new(2, 1), Kind::Let),
            Token::new(Location::new(2, 5), Kind::Ident("ten".to_string())),
            Token::new(Location::new(2, 9), Kind::Assign),
            Token::new(Location::new(2, 11), Kind::Int("10".to_string())),
            Token::new(Location::new(2, 13), Kind::Semicolon),
            Token::new(Location::new(4, 1), Kind::Let),
            Token::new(Location::new(4, 5), Kind::Ident("add".to_string())),
            Token::new(Location::new(4, 9), Kind::Assign),
            Token::new(Location::new(4, 11), Kind::Function),
            Token::new(Location::new(4, 13), Kind::LParen),
            Token::new(Location::new(4, 14), Kind::Ident("x".to_string())),
            Token::new(Location::new(4, 15), Kind::Comma),
            Token::new(Location::new(4, 17), Kind::Ident("y".to_string())),
            Token::new(Location::new(4, 18), Kind::RParen),
            Token::new(Location::new(4, 20), Kind::LBrace),
            Token::new(Location::new(5, 5), Kind::Ident("x".to_string())),
            Token::new(Location::new(5, 7), Kind::Plus),
            Token::new(Location::new(5, 9), Kind::Ident("y".to_string())),
            Token::new(Location::new(5, 10), Kind::Semicolon),
            Token::new(Location::new(6, 1), Kind::RBrace),
            Token::new(Location::new(6, 2), Kind::Semicolon),
            Token::new(Location::new(8, 1), Kind::Let),
            Token::new(Location::new(8, 5), Kind::Ident("result".to_string())),
            Token::new(Location::new(8, 12), Kind::Assign),
            Token::new(Location::new(8, 14), Kind::Ident("add".to_string())),
            Token::new(Location::new(8, 17), Kind::LParen),
            Token::new(Location::new(8, 18), Kind::Ident("five".to_string())),
            Token::new(Location::new(8, 22), Kind::Comma),
            Token::new(Location::new(8, 24), Kind::Ident("ten".to_string())),
            Token::new(Location::new(8, 27), Kind::RParen),
            Token::new(Location::new(8, 28), Kind::Semicolon),
            Token::new(Location::new(9, 1), Kind::Bang),
            Token::new(Location::new(9, 2), Kind::Minus),
            Token::new(Location::new(9, 3), Kind::Slash),
            Token::new(Location::new(9, 4), Kind::Asterisk),
            Token::new(Location::new(9, 5), Kind::Int("5".to_string())),
            Token::new(Location::new(9, 6), Kind::Semicolon),
            Token::new(Location::new(10, 1), Kind::Int("5".to_string())),
            Token::new(Location::new(10, 3), Kind::LT),
            Token::new(Location::new(10, 5), Kind::Int("10".to_string())),
            Token::new(Location::new(10, 8), Kind::GT),
            Token::new(Location::new(10, 10), Kind::Int("5".to_string())),
            Token::new(Location::new(10, 11), Kind::Semicolon),
            Token::new(Location::new(12, 1), Kind::If),
            Token::new(Location::new(12, 4), Kind::LParen),
            Token::new(Location::new(12, 5), Kind::Int("5".to_string())),
            Token::new(Location::new(12, 7), Kind::LT),
            Token::new(Location::new(12, 9), Kind::Int("10".to_string())),
            Token::new(Location::new(12, 11), Kind::RParen),
            Token::new(Location::new(12, 13), Kind::LBrace),
            Token::new(Location::new(13, 5), Kind::Return),
            Token::new(Location::new(13, 12), Kind::True),
            Token::new(Location::new(13, 16), Kind::Semicolon),
            Token::new(Location::new(14, 1), Kind::RBrace),
            Token::new(Location::new(14, 3), Kind::Else),
            Token::new(Location::new(14, 8), Kind::LBrace),
            Token::new(Location::new(15, 5), Kind::Return),
            Token::new(Location::new(15, 12), Kind::False),
            Token::new(Location::new(15, 17), Kind::Semicolon),
            Token::new(Location::new(16, 1), Kind::RBrace),
            Token::new(Location::new(18, 1), Kind::Int("10".to_string())),
            Token::new(Location::new(18, 4), Kind::EQ),
            Token::new(Location::new(18, 7), Kind::Int("10".to_string())),
            Token::new(Location::new(18, 9), Kind::Semicolon),
            Token::new(Location::new(19, 1), Kind::Int("10".to_string())),
            Token::new(Location::new(19, 4), Kind::NotEQ),
            Token::new(Location::new(19, 7), Kind::Int("9".to_string())),
            Token::new(Location::new(19, 8), Kind::Semicolon),
            Token::new(Location::new(20, 1), Kind::Str("foobar".to_string())),
            Token::new(Location::new(21, 1), Kind::Str("foo bar".to_string())),
            Token::new(Location::new(22, 1), Kind::LBracket),
            Token::new(Location::new(22, 2), Kind::Int("1".to_string())),
            Token::new(Location::new(22, 3), Kind::Comma),
            Token::new(Location::new(22, 4), Kind::Int("2".to_string())),
            Token::new(Location::new(22, 5), Kind::RBracket),
            Token::new(Location::new(22, 6), Kind::Semicolon),
        ]
    }

    #[test]
    fn produces_the_expected_tokens() {
        let mut lexer = INPUT.to_source().into_tokens();
        for token in expected_tokens().into_iter() {
            assert_eq!(token, lexer.next().expect("no token in source"));
        }
    }
}
