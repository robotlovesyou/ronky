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
        use self::Kind::*;
        match &self {
            Illegal(_) => Tag::Illegal,
            Ident(_) => Tag::Ident,
            Int(_) => Tag::Int,
            Assign => Tag::Assign,
            Plus => Tag::Plus,
            Minus => Tag::Minus,
            Bang => Tag::Bang,
            Asterisk => Tag::Asterisk,
            Slash => Tag::Slash,
            LT => Tag::LT,
            GT => Tag::GT,
            EQ => Tag::EQ,
            NotEQ => Tag::NotEQ,
            Comma => Tag::Comma,
            Semicolon => Tag::Semicolon,
            LParen => Tag::LParen,
            RParen => Tag::RParen,
            LBrace => Tag::LBrace,
            RBrace => Tag::RBrace,
            Function => Tag::Function,
            Let => Tag::Let,
            True => Tag::True,
            False => Tag::False,
            If => Tag::If,
            Else => Tag::Else,
            Return => Tag::Return,
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
        use self::Kind::*;
        let display = match self {
            Illegal(ref illegal) => illegal.to_string(),
            Ident(ref ident) => ident.to_string(),
            Int(repr) => repr.to_string(),
            Assign => "=".to_string(),
            Plus => "+".to_string(),
            Minus => "-".to_string(),
            Bang => "!".to_string(),
            Asterisk => "*".to_string(),
            Slash => "/".to_string(),
            LT => "<".to_string(),
            GT => ">".to_string(),
            EQ => "==".to_string(),
            NotEQ => "!=".to_string(),
            Comma => ",".to_string(),
            Semicolon => ";".to_string(),
            LParen => "(".to_string(),
            RParen => ")".to_string(),
            LBrace => "{".to_string(),
            RBrace => "}".to_string(),
            Function => "function".to_string(),
            Let => "let".to_string(),
            True => "true".to_string(),
            False => "false".to_string(),
            If => "if".to_string(),
            Else => "else".to_string(),
            Return => "return".to_string(),
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
        use self::Tag::*;
        let display = match self {
            Illegal => "illegal",
            Ident => "identifier",
            Int => "integer",
            Assign => "assignment",
            Plus => "plus",
            Minus => "minus",
            Bang => "bang",
            Asterisk => "asterisk",
            Slash => "slash",
            LT => "less than",
            GT => "greater than",
            EQ => "equal",
            NotEQ => "not equal",
            Comma => "comma",
            Semicolon => "semicolon",
            LParen => "left paren",
            RParen => "right paren",
            LBrace => "left brace",
            RBrace => "right brace",
            Function => "function",
            Let => "let",
            True => "true",
            False => "false",
            If => "if",
            Else => "else",
            Return => "return",
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
