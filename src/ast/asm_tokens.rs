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
    // Parametric
    Drop,
    // Control
    Return,
    // Numeric
    I32Const,
    I64Const,
    //Local
    LocalGet,
    LocalSet,
    // Memory
    MemorySize,
    MemoryGrow,
    I32Load,
    I32Store,
    I64Load,
    I64Store,
    F32Load,
    F32Store,
    F64Load,
    F64Store,
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
        (String::from("local.get"), to_token(Opcode::LocalGet)),
        (String::from("local.set"), to_token(Opcode::LocalSet)),
        (String::from("memory.size"), to_token(Opcode::MemorySize)),
        (String::from("memory.grow"), to_token(Opcode::MemoryGrow)),
        (String::from("i32.load"), to_token(Opcode::I32Load)),
        (String::from("i32.store"), to_token(Opcode::I32Store)),
        (String::from("i64.load"), to_token(Opcode::I64Load)),
        (String::from("i64.store"), to_token(Opcode::I64Store)),
        (String::from("f32.load"), to_token(Opcode::F32Load)),
        (String::from("f32.store"), to_token(Opcode::F32Store)),
        (String::from("f64.load"), to_token(Opcode::F64Load)),
        (String::from("f64.store"), to_token(Opcode::F64Store)),

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
            Opcode::LocalGet => write!(f, "local.get"),
            Opcode::LocalSet => write!(f, "local.set"),
            Opcode::MemorySize => write!(f, "memory.size"),
            Opcode::MemoryGrow=> write!(f, "memory.grow"),
            Opcode::I32Load=> write!(f, "i32.load"),
            Opcode::I32Store=> write!(f, "i32.store"),
            Opcode::I64Load=> write!(f, "i64.load"),
            Opcode::I64Store=> write!(f, "i64.store"),
            Opcode::F32Load=> write!(f, "f32.load"),
            Opcode::F32Store=> write!(f, "f32.store"),
            Opcode::F64Load=> write!(f, "f64.load"),
            Opcode::F64Store=> write!(f, "f64.store"),
        }
    }
}
