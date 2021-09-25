//! # Tokens

#![allow(dead_code)]

use std::fmt;

pub use super::syntax::SyntaxKind;
use crate::diagnostics::Location;

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub t: SyntaxKind,
    pub start: u32,
    pub end: u32,
}

impl Token {
    pub fn loc(&self) -> Location {
        Location {
            start: self.start,
            end: self.end,
        }
    }

    fn eof() -> Token {
        Token {
            t: SyntaxKind::EOF,
            start: 0,
            end: 0,
        }
    }
}

/// The list of tokens
pub mod token_kind {
    pub use super::SyntaxKind::{
        // Keywords
        Abstract,
        And,
        AndAnd,
        As,
        Bang,

        // Two characters
        BangEqual,
        Blank,

        Colon,
        ColonColon,
        Comma,

        // Other
        Dot,
        Else,
        Equal,
        EqualEqual,
        Error,
        Expose,
        False,
        From,
        Fun,
        Greater,
        GreaterEqual,
        Hat,

        // Literals
        Identifier,
        If,
        Impl,
        Import,
        LeftBrace,

        // Single character
        LeftPar,
        Less,
        LessEqual,
        Let,
        Minus,
        Module,
        NumberLit,
        Or,
        OrOr,
        Percent,
        Plus,
        Pub,
        Return,
        RightBrace,
        RightPar,
        Runtime,
        SemiColon,
        Slash,
        Standalone,
        Star,
        StringLit,
        Struct,
        True,
        Use,
        Var,
        While,
        // Special
        EOF,
    };
}

pub struct TokenStream<'a> {
    tokens: Vec<Token>,
    cursor: usize,
    is_at_end: bool,
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            tokens,
            source,
            cursor: 0,
            is_at_end: false,
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn source(&self) -> &str {
        self.source
    }

    pub fn is_at_end(&self) -> bool {
        self.is_at_end
    }

    pub fn advance(&mut self) -> Token {
        let idx = self.cursor;
        if idx >= self.tokens.len() {
            self.is_at_end = true;
            Token::eof()
        } else {
            self.cursor += 1;
            self.tokens[idx]
        }
    }

    pub fn peek(&mut self) -> Token {
        self.tokens.get(self.cursor).cloned().unwrap_or_else(|| {
            self.is_at_end = true;
            Token::eof()
        })
    }

    pub fn peekpeek(&mut self) -> Token {
        self.tokens
            .get(self.cursor + 1)
            .cloned()
            .unwrap_or_else(|| {
                self.is_at_end = true;
                Token::eof()
            })
    }

    pub fn text(&self, start: u32, end: u32) -> &'a str {
        &self.source[start as usize..end as usize]
    }
}

impl<'a> fmt::Debug for TokenStream<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = self.source;
        write!(f, "|")?;
        for token in &self.tokens {
            if let token_kind::SemiColon = token.t {
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

impl<'a> fmt::Display for TokenStream<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let source = self.source;
        for token in &self.tokens {
            if token.t != token_kind::SemiColon {
                let start = token.start as usize;
                let end = token.end as usize;
                write!(f, "{}", &source[start..end])?;
            }
        }
        Ok(())
    }
}
