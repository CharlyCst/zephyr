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
        Colon,
        ColonColon,
        Comma,

        // Other
        CommentString,
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
        NewLine,
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
        Whitespace,
        EOF,
    };
}

pub struct TokenStream<'a> {
    tokens: Vec<Token>,
    cursor: usize,
    source: &'a str,
}

impl<'a> TokenStream<'a> {
    pub fn new(tokens: Vec<Token>, source: &'a str) -> Self {
        Self {
            tokens,
            source,
            cursor: 0,
        }
    }

    pub fn tokens(&self) -> &Vec<Token> {
        &self.tokens
    }

    pub fn source(&self) -> &str {
        self.source
    }

    pub fn advance(&mut self) -> Option<Token> {
        let idx = self.cursor;
        if idx >= self.tokens.len() {
            None
        } else {
            self.cursor += 1;
            Some(self.tokens[idx])
        }
    }

    pub fn peek(&self) -> Option<Token> {
        self.tokens.get(self.cursor).cloned()
    }

    pub fn peekpeek(&self) -> Option<Token> {
        self.tokens.get(self.cursor + 1).cloned()
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
