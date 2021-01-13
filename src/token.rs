use std::fmt::{self, Display, Formatter};

#[derive(Eq, PartialEq, Debug)]
pub enum Kind {
    Illegal(char),

    Ident(String),
    Int(String),

    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    LT,
    GT,

    EQ,
    NotEQ,

    Comma,
    Semicolon,

    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl Kind {
    pub fn tag(&self) -> Tag {
        match &self {
            Kind::Illegal(_) => Tag::Illegal,
            Kind::Ident(_) => Tag::Ident,
            Kind::Int(_) => Tag::Int,
            Kind::Assign => Tag::Assign,
            Kind::Plus => Tag::Plus,
            Kind::Minus => Tag::Minus,
            Kind::Bang => Tag::Bang,
            Kind::Asterisk => Tag::Asterisk,
            Kind::Slash => Tag::Slash,
            Kind::LT => Tag::LT,
            Kind::GT => Tag::GT,
            Kind::EQ => Tag::EQ,
            Kind::NotEQ => Tag::NotEQ,
            Kind::Comma => Tag::Comma,
            Kind::Semicolon => Tag::Semicolon,
            Kind::LParen => Tag::LParen,
            Kind::RParen => Tag::RParen,
            Kind::LBrace => Tag::LBrace,
            Kind::RBrace => Tag::RBrace,
            Kind::Function => Tag::Function,
            Kind::Let => Tag::Let,
            Kind::True => Tag::True,
            Kind::False => Tag::False,
            Kind::If => Tag::If,
            Kind::Else => Tag::Else,
            Kind::Return => Tag::Return
        }
    }
}

// impl std::string::ToString for Kind {
//     fn to_string(&self) -> String {
//         self.tag().to_string()
//     }
// }

impl fmt::Display for Kind {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let display = match self {
            Kind::Illegal(ref illegal) => illegal.to_string(),
            Kind::Ident(ref ident) => ident.to_string(),
            Kind::Int(repr) => repr.to_string(),
            Kind::Assign => "=".to_string(),
            Kind::Plus => "+".to_string(),
            Kind::Minus => "-".to_string(),
            Kind::Bang => "!".to_string(),
            Kind::Asterisk => "*".to_string(),
            Kind::Slash => "/".to_string(),
            Kind::LT => "<".to_string(),
            Kind::GT => ">".to_string(),
            Kind::EQ => "==".to_string(),
            Kind::NotEQ => "!=".to_string(),
            Kind::Comma => ",".to_string(),
            Kind::Semicolon => ";".to_string(),
            Kind::LParen => "(".to_string(),
            Kind::RParen => ")".to_string(),
            Kind::LBrace => "{".to_string(),
            Kind::RBrace => "}".to_string(),
            Kind::Function => "function".to_string(),
            Kind::Let => "let".to_string(),
            Kind::True => "true".to_string(),
            Kind::False => "false".to_string(),
            Kind::If => "if".to_string(),
            Kind::Else => "else".to_string(),
            Kind::Return => "return".to_string(),
        };
        write!(f, "{}", display)
    }
}

#[derive(Debug, Eq, PartialEq, Hash)]
pub enum Tag {
    Illegal,
    Ident,
    Int,
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,
    LT,
    GT,
    EQ,
    NotEQ,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Return,
}

impl std::fmt::Display for Tag {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let display = match self {
            Tag::Illegal => "illegal",
            Tag::Ident => "identifier",
            Tag::Int => "integer",
            Tag::Assign => "assignment",
            Tag::Plus => "plus",
            Tag::Minus => "minus",
            Tag::Bang => "bang",
            Tag::Asterisk => "asterisk",
            Tag::Slash => "slash",
            Tag::LT => "less than",
            Tag::GT => "greater than",
            Tag::EQ => "equal",
            Tag::NotEQ => "not equal",
            Tag::Comma => "comma",
            Tag::Semicolon => "semicolon",
            Tag::LParen => "left paren",
            Tag::RParen => "right paren",
            Tag::LBrace => "left brace",
            Tag::RBrace => "right brace",
            Tag::Function => "function",
            Tag::Let => "let",
            Tag::True => "true",
            Tag::False => "false",
            Tag::If => "if",
            Tag::Else => "else",
            Tag::Return => "return",
        };
        write!(f, "{}", display)
    }
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub kind: Kind,
}

impl Token {
    pub fn new(line: usize, column: usize, kind: Kind) -> Token {
        Token { line, column, kind }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.kind)
    }
}

pub fn keyword_to_kind(keyword: &str) -> Kind {
    match keyword {
        "fn" => Kind::Function,
        "let" => Kind::Let,
        "true" => Kind::True,
        "false" => Kind::False,
        "if" => Kind::If,
        "else" => Kind::Else,
        "return" => Kind::Return,
        ident => Kind::Ident(ident.to_string()),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn each_token_returns_the_correct_tag() {
        let kinds_and_tags = vec![
            (Kind::Illegal('a'), Tag::Illegal),
            (Kind::Ident("abc".to_string()), Tag::Ident),
            (Kind::Int("123".to_string()), Tag::Int),
            (Kind::Assign, Tag::Assign),
            (Kind::Plus, Tag::Plus),
            (Kind::Minus, Tag::Minus),
            (Kind::Bang, Tag::Bang),
            (Kind::Asterisk, Tag::Asterisk),
            (Kind::Slash, Tag::Slash),
            (Kind::LT, Tag::LT),
            (Kind::GT, Tag::GT),
            (Kind::EQ, Tag::EQ),
            (Kind::NotEQ, Tag::NotEQ),
            (Kind::Comma, Tag::Comma),
            (Kind::Semicolon, Tag::Semicolon),
            (Kind::LParen, Tag::LParen),
            (Kind::RParen, Tag::RParen),
            (Kind::LBrace, Tag::LBrace),
            (Kind::RBrace, Tag::RBrace),
            (Kind::Function, Tag::Function),
            (Kind::Let, Tag::Let),
            (Kind::True, Tag::True),
            (Kind::False, Tag::False),
            (Kind::If, Tag::If),
            (Kind::Else, Tag::Else),
            (Kind::Return, Tag::Return),
        ];
        
        for (kind, tag) in kinds_and_tags.iter() {
            assert_eq!(*tag, kind.tag())
        }
    }
}
