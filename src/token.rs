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
