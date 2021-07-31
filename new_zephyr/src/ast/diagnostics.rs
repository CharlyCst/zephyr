//! AST Diagnostics

use crate::diagnostics::Error;

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
