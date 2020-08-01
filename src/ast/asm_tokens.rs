use crate::error::Location;
use std::collections::HashMap;
use std::fmt;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single character
    LeftPar,
    RightPar,
    LeftBrace,
    RightBrace,
    Comma,
    // Literals
    Identifier(String),
    StringLit(String),
    NumberLit(u64),
    Opcode(Opcode),

    // Keywords
    Expose,
    Fun,
    Pub,
    Package,
    As,

    // Other
    SemiColon,
    EOF,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    Drop,
    Return,
    I32Const,
    I64Const,
}

pub struct Token {
    pub t: TokenType,
    pub loc: Location,
}

/// Return an HashMap populated with all Zephyr asm keywords and opcodes.
pub fn get_keyword_map() -> HashMap<String, TokenType> {
    [
        // Keywords
        (String::from("as"), TokenType::As),
        (String::from("expose"), TokenType::Expose),
        (String::from("fun"), TokenType::Fun),
        (String::from("pub"), TokenType::Pub),
        (String::from("package"), TokenType::Package),
        // Opcodes
        (String::from("drop"), to_token(Opcode::Drop)),
        (String::from("return"), to_token(Opcode::Return)),
        (String::from("i32.const"), to_token(Opcode::I32Const)),
        (String::from("i64.const"), to_token(Opcode::I64Const)),
    ]
    .iter()
    .cloned()
    .collect()
}

fn to_token(op: Opcode) -> TokenType {
    TokenType::Opcode(op)
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.t {
            // Single Character
            TokenType::LeftPar => write!(f, "("),
            TokenType::RightPar => write!(f, ")"),
            TokenType::LeftBrace => write!(f, "{{"),
            TokenType::RightBrace => write!(f, "}}"),
            TokenType::Comma => write!(f, ","),
            TokenType::SemiColon => write!(f, ";"),
            TokenType::EOF => write!(f, "EOF"),
            // Keywords
            TokenType::As => write!(f, "as"),
            TokenType::Expose => write!(f, "expose"),
            TokenType::Fun => write!(f, "fun"),
            TokenType::Pub => write!(f, "pub"),
            TokenType::Package => write!(f, "package"),
            // Literals
            TokenType::Identifier(ref ident) => write!(f, "'{}'", ident),
            TokenType::StringLit(ref s) => write!(f, "\"{}\"", s),
            TokenType::NumberLit(ref n) => write!(f, "{}", n),
            // Opcodes
            TokenType::Opcode(opcode) => write!(f, "{}", opcode),
        }
    }
}

impl fmt::Display for Opcode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Opcode::Drop => write!(f, "drop"),
            Opcode::Return => write!(f, "return"),
            Opcode::I32Const => write!(f, "i32.const"),
            Opcode::I64Const => write!(f, "i64.const"),
        }
    }
}
