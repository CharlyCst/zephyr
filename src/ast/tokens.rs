use crate::error::Location;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single character
    LeftPar,
    RightPar,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Percent,
    Bang,
    Equal,
    Greater,
    Less,
    And,
    Or,

    // Two characters
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    AndAnd,
    OrOr,

    // Literals
    Identifier(String),
    IntegerLit(u64),
    FloatLit(f64),
    BooleanLit(bool),
    StringLit(String),

    // Keywords
    As,
    Else,
    Expose,
    False,
    Fun,
    If,
    Let,
    Package,
    Pub,
    Return,
    True,
    Use,
    Var,
    While,

    // Other
    SemiColon,
    EOF,
}

pub struct Token {
    pub t: TokenType,
    pub loc: Location,
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} ", self.t)
    }
}
