//! AST Diagnostics

use crate::diagnostics::Error;

#[allow(dead_code)]
pub enum ScanError {
    BadNumber,
    NoFloatHexa,
    MalformedHexa,
    NoFloatBinary,
    MalformedBinary,
    MultilineString,
    UnpairedDoubleQuote,
    BadEscapeSequence(char),
    UnexpectedCharacter(char),
}

impl Error for ScanError {
    fn message(&self) -> String {
        match self {
            Self::BadNumber => "Failed to parse number".into(),
            Self::NoFloatHexa => "Hexadecimal numbers can't be floating points".into(),
            Self::MalformedHexa => "Malformed hexadecimal number".into(),
            Self::NoFloatBinary => "Binary numbers can't be floiating points".into(),
            Self::MalformedBinary => "Malformed binary number".into(),
            Self::MultilineString => "String literal can not span multiple lines".into(),
            Self::UnpairedDoubleQuote => "Could not find matching quote '\"'".into(),
            Self::BadEscapeSequence(c) => format!("Invalid escape sequence: '\\{}'", c),
            Self::UnexpectedCharacter(c) => format!("Unexpected character: '{}'", c),
        }
    }
}

#[allow(dead_code)]
pub enum ParseError {
    Unknown,
    BadModDecl,
    ModDeclMissIdent,
    MissingSemicolon,
}

impl Error for ParseError {
    fn message(&self) -> String {
        match self {
            Self::Unknown => "A parsing error occured".into(),
            Self::BadModDecl => "Invalid module declaration".into(),
            Self::ModDeclMissIdent => "Expected a module identifier".into(),
            Self::MissingSemicolon => "Expected a statement ender, ty adding a line break".into(),
        }
    }
}
