#[derive(Eq, PartialEq, Debug)]
pub enum TokenKind {
    Illegal(char),
    EOF,

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
    Return
}

#[derive(Debug, Eq, PartialEq)]
pub struct Token {
    pub line: usize,
    pub column: usize,
    pub kind: TokenKind,
}


impl Token {
    pub fn new(line: usize, column: usize, kind: TokenKind) -> Token {
        Token {
            line,
            column,
            kind,
        }
    }
}

pub fn keyword_to_token(keyword: &str) -> TokenKind {
    match keyword {
        "fn" => TokenKind::Function,
        "let" => TokenKind::Let,
        "true" => TokenKind::True,
        "false" => TokenKind::False,
        "if" => TokenKind::If,
        "else" => TokenKind::Else,
        "return" => TokenKind::Return,
        ident => TokenKind::Ident(ident.to_string())
    }
}