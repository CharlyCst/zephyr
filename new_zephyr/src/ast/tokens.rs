//! # Tokens

#![allow(dead_code)]

use std::fmt;

#[derive(Debug)]
pub struct Token {
    pub t: TokenType,
    pub start: u32,
    pub end: u32,
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
    Identifier,
    NumberLit,
    StringLit,
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
    CommentString,
    SemiColon,
    EOF,
    Error,
    NewLine,
    Whitespace,
}

pub struct TokenStream<'a> {
    tokens: Vec<Token>,
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self { tokens, source }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn source(&self) -> &str {
        self.source
    }
}

impl<'a> fmt::Display for TokenStream<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = self.source;
        write!(f, "|")?;
        for token in &self.tokens {
            if let TokenType::SemiColon = token.t {
                write!(f, ";|")?;
            } else {
                let start = token.start as usize;
                let end = token.end as usize;
                write!(f, "{}|", &source[start..end])?;
            }
        }
        Ok(())
    }
}
