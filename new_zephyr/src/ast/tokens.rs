//! # Tokens

use crate::diagnostics::Location;

#[derive(Debug)]
pub struct Token {
    pub t: TokenType,
    pub loc: Location,
}

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
    ColonColon,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    AndAnd,
    OrOr,

    // Literals
    Identifier(String),
    IntegerLit(u64),
    FloatLit(f64),
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

