#[derive(Eq, PartialEq, Debug)]
pub enum TokenKind {
    ILLEGAL(char),
    EOF,

    IDENT(String),
    INT(String),

    ASSIGN,
    PLUS,
    MINUS,
    BANG,
    ASTERISK,
    SLASH,

    LT,
    GT,

    EQ,
    NOT_EQ,

    COMMA,
    SEMICOLON,

    LPAREN,
    RPAREN,
    LBRACE,
    RBRACE,

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN
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
        "fn" => TokenKind::FUNCTION,
        "let" => TokenKind::LET,
        "true" => TokenKind::TRUE,
        "false" => TokenKind::FALSE,
        "if" => TokenKind::IF,
        "else" => TokenKind::ELSE,
        "return" => TokenKind::RETURN,
        ident => TokenKind::IDENT(ident.to_string())
    }
}