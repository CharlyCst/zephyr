//! AST Diagnostics

use crate::diagnostics::Error;

pub enum ScanError {
    BadNumber,
    NoFloatHexa,
    NoFloatBinary,
    MultilineString,
    BadEscapeSequence(u8),
    UnexpectedCharacter(u8),
}

impl Error for ScanError {
    fn message(&self) -> String {
        match self {
            Self::BadNumber => "Failed to parse number".into(),
            Self::NoFloatHexa => "Hexadecimal numbers can't be floating points".into(),
            Self::NoFloatBinary => "Binary numbers can't be floiating points".into(),
            Self::MultilineString => "String literal can not span multiple lines".into(),
            Self::BadEscapeSequence(c) => format!("Invalid escape sequence: '\\{}'", c),
            Self::UnexpectedCharacter(c) => format!("Unexpected character: '{}'", c),
        }
    }
}
