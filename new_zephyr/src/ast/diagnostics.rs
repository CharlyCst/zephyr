//! AST Diagnostics

use crate::diagnostics::Error;

pub enum ScanError {
    BadNumber,
    NoFloatHexa,
    NoFloatBinary,
    UnexpectedCharacter(u8),
}

impl Error for ScanError {
    fn message(&self) -> String {
        match self {
            Self::BadNumber => "Failed to parse number".into(),
            Self::NoFloatHexa => "Hexadecimal numbers can't be floating points".into(),
            Self::NoFloatBinary => "Binary numbers can't be floiating points".into(),
            Self::UnexpectedCharacter(c) => format!("Unexpected character: '{}'", c),
        }
    }
}
