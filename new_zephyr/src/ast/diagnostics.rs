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
    Internal,
    BadModDecl,
    ModDeclMissIdent,
    MissingSemicolon,
    ExpectModPath,
    ExpectPath,
    ExpectUseDecl,
    ExpectStructDecl,
    ExpectIdent,
    ExpectColon,
    ExpectLeftPar,
    ExpectLeftBrace,
    ExpectRightBrace,
    ExpectFunKeyword,
    ExpectImportKeyword,
    MissingClosingPar,
}

impl Error for ParseError {
    fn message(&self) -> String {
        match self {
            Self::Unknown => "A parsing error occured".into(),
            Self::Internal => "An internal error occured during parsing".into(),
            Self::BadModDecl => "Invalid module declaration".into(),
            Self::ModDeclMissIdent => "Expected a module identifier".into(),
            Self::MissingSemicolon => "Expected a statement ender, ty adding a line break".into(),
            Self::ExpectModPath => "Expect a module path".into(),
            Self::ExpectPath => "Expect a path".into(),
            Self::ExpectUseDecl => "Expect a 'use' declaration".into(),
            Self::ExpectStructDecl => "Expect a 'struct' declaration".into(),
            Self::ExpectIdent => "Expect an identifer".into(),
            Self::ExpectColon => "Expect a colon (':')".into(),
            Self::ExpectLeftPar => "Expect an opening parenthesis '('".into(),
            Self::ExpectLeftBrace => "Expect an opening brace '{'".into(),
            Self::ExpectRightBrace => "Expect a closing brace '}'".into(),
            Self::ExpectFunKeyword => "Expect the 'fun' keyword for defining a function".into(),
            Self::ExpectImportKeyword => "Expect the 'import' keyword for defining imported items".into(),
            Self::MissingClosingPar => "Missing closing parenthesis ')'".into(),
        }
    }
}
