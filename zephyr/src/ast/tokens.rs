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
    Colon,
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
    Hat,

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
    False,
    True,

    // Keywords
    Abstract,
    As,
    Else,
    Expose,
    From,
    Fun,
    If,
    Impl,
    Import,
    Let,
    Module,
    Pub,
    Return,
    Runtime,
    Standalone,
    Struct,
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
