//! # Scanner
//!
//! TODO: handle UTF-8

use std::str;

use super::diagnostics::ScanError;
use super::tokens::TokenType;
use crate::diagnostics::{Diagnostics, Location, Position};

struct Scanner<'a> {
    source: &'a [u8],
    tokens: Vec<TokenType>,
    err: Diagnostics,

    /// Position in file
    line: u32,
    /// Idex of the start of the line
    line_start: usize,

    /// Index of the current character
    current: usize,
    /// Index of the start of the current token
    start: usize,
    /// Number of opened parenthesis
    parenthesis_count: u32,
    /// Wether the next line break is a statement ender
    stmt_ender: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source: source.as_bytes(),
            tokens: Vec::new(),
            err: Diagnostics::default(),
            line: 1,
            line_start: 0,
            current: 0,
            start: 0,
            parenthesis_count: 0,
            stmt_ender: false,
        }
    }

    pub fn scan(mut self) -> (Vec<TokenType>, Diagnostics) {
        while !self.is_at_end() {
            self.scan_token();
        }

        self.add_token(TokenType::EOF);
        (self.tokens, self.err)
    }

    /// Scan the next token, advancing the cursor.
    fn scan_token(&mut self) {
        let token = match self.advance() {
            // Single character tokens
            b'(' => TokenType::LeftPar,
            b')' => TokenType::RightPar,
            b'{' => TokenType::LeftBrace,
            b'}' => TokenType::RightBrace,
            b',' => TokenType::Comma,
            b':' => TokenType::Colon,
            b'.' => TokenType::Dot,
            b'-' => TokenType::Minus,
            b'+' => TokenType::Plus,
            b'*' => TokenType::Star,
            b'%' => TokenType::Percent,
            b'^' => TokenType::Hat,

            // Two character tokens
            b'!' => {
                if self.next_match(b'=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                }
            }
            b'=' => {
                if self.next_match(b'=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                }
            }
            b'>' => {
                if self.next_match(b'=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                }
            }
            b'<' => {
                if self.next_match(b'=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                }
            }
            b'&' => {
                if self.next_match(b'&') {
                    TokenType::AndAnd
                } else {
                    TokenType::And
                }
            }
            b'|' => {
                if self.next_match(b'|') {
                    TokenType::OrOr
                } else {
                    TokenType::Or
                }
            }
            b'/' => {
                if self.next_match(b'/') {
                    self.consume_comment();
                    return;
                } else {
                    TokenType::Slash
                }
            }

            // Whitespaces and end of line
            b'\n' => {
                self.new_line();
                return;
            }
            b' ' | b'\t' | b'\r' => {
                self.consume_whitespace();
                return;
            }

            // Advanced logic for multi-character tokens
            c => {
                if c.is_ascii_digit() {
                    self.scan_number();
                } else if c.is_ascii_alphabetic() || c == b'_' {
                    self.scan_identifier();
                } else if c == b'"' {
                    self.scan_string();
                } else {
                    self.err
                        .report(ScanError::UnexpectedCharacter(c), self.get_previous_loc(1));
                }
                return;
            }
        };
        self.add_token(token);
    }

    // —————————————————————————————— Identifiers ——————————————————————————————— //

    /// Consumes consecutive alphanumeric characters into an identifier. Procude a keyword token if
    /// the identifier happens to be one of the reserved keywords.
    fn scan_identifier(&mut self) {
        // Move until the end of the current identifier [a-zA-Z0-9_]
        // Note: we don't disambiguate with numbers here, the caller should do it
        while !self.is_at_end() && self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            self.advance();
        }
        // Convert that sequence of chars to a string
        // TODO: error handling
        let ident = str::from_utf8(&self.source[self.start..self.current]).unwrap();
        if let Some(keyword) = as_keyword(&ident) {
            self.add_token(keyword);
        } else {
            self.add_token(TokenType::Identifier(ident.to_string()));
        }
    }

    // ———————————————————————————————— Strings ————————————————————————————————— //

    /// Consumes any (non carriage-return) characters between double quotes
    fn scan_string(&mut self) {
        let mut bytes = Vec::new();

        // Consume until the next char is a double quote
        while !self.is_at_end() {
            match self.advance() {
                // Some characters might be escaped, we have to process those differently
                b'\\' => match self.advance() {
                    b'\\' => bytes.push(b'\\'),
                    b'n' => bytes.push(b'\n'),
                    b'r' => bytes.push(b'\r'),
                    b't' => bytes.push(b'\t'),
                    b'0' => bytes.push(b'\0'),
                    b'"' => bytes.push(b'"'),
                    c => {
                        let loc = self.get_previous_loc(2);
                        self.err.report(ScanError::BadEscapeSequence(c), loc);
                    }
                },
                b'"' => break, // End of string
                b'\n' => {
                    // Exit if the double quote is not found on this line
                    self.err
                        .report(ScanError::MultilineString, self.get_current_token_loc());
                    break;
                }
                c => bytes.push(c),
            }
        }
        let str_val = String::from_utf8(bytes).unwrap(); // TODO: error handling
        self.add_token(TokenType::StringLit(str_val));
    }

    // ———————————————————————————————— Numbers ————————————————————————————————— //

    // TODO: report an error if a non-numeric character is found
    /// Consumes consecutive digit characters and push a number token
    fn scan_number(&mut self) {
        // Look for a basis (hexa or binary)
        if self.peek() == b'x' {
            self.advance();
            self.scan_hexadecimal_number()
        } else if self.peek() == b'b' {
            self.advance();
            self.scan_binary_number()
        } else {
            self.scan_decimal_number()
        }
    }

    fn scan_decimal_number(&mut self) {
        let start = self.start;
        let mut is_float = false;

        // Consume digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }
        if self.peek() == b'.' {
            is_float = true;
            while !self.is_at_end() && self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        // Parse token
        let bytes = &self.source[start..self.current];
        let bytes = str::from_utf8(bytes).unwrap();
        let token = if is_float {
            let float = bytes.parse::<f64>().unwrap(); // TODO: error handling
            TokenType::FloatLit(float)
        } else {
            let int = bytes.parse::<u64>().unwrap(); // TODO: error handling
            TokenType::IntegerLit(int)
        };
        self.add_token(token);
    }

    fn scan_hexadecimal_number(&mut self) {
        let start = self.current;

        // Consume digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }
        // Floating points are not valid in hexadecimal
        if self.peek() == b'.' {
            self.err
                .report(ScanError::NoFloatHexa, self.get_current_loc());
        }

        // Parse token
        let bytes = &self.source[start..self.current];
        let bytes = str::from_utf8(bytes).unwrap();
        let hexa = u64::from_str_radix(bytes, 16).unwrap(); // TODO: error handling
        self.add_token(TokenType::IntegerLit(hexa));
    }

    fn scan_binary_number(&mut self) {
        let start = self.current;

        // Consume digits
        while !self.is_at_end() && self.peek().is_ascii_digit() {
            self.advance();
        }
        // Floating points are not valid in binary
        if self.peek() == b'.' {
            self.err
                .report(ScanError::NoFloatBinary, self.get_current_loc());
        }

        // Parse token
        let bytes = &self.source[start..self.current];
        let bytes = str::from_utf8(bytes).unwrap();
        let hexa = u64::from_str_radix(bytes, 2).unwrap(); // TODO: error handling
        self.add_token(TokenType::IntegerLit(hexa));
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Adds the token to the token list & update scanner state.
    fn add_token(&mut self, token: TokenType) {
        self.check_stmt_ender(&token);
        self.tokens.push(token);
        self.start = self.current;
    }

    /// Registers a new line
    fn new_line(&mut self) {
        if self.stmt_ender {
            self.add_token(TokenType::SemiColon);
        }
        self.line += 1;
        self.line_start = self.current;
        self.start = self.current;
    }

    /// Consumes every character until the end of source or the next line break.
    fn consume_comment(&mut self) {
        while !self.is_at_end() && self.peek() != b'\n' {
            self.advance();
        }
    }

    fn consume_whitespace(&mut self) {
        while !self.is_at_end() && self.peek().is_ascii_whitespace() {
            self.advance();
        }
        self.start = self.current;
    }

    /// Check if the token can be a statement ender and updates internal state accordingly.
    fn check_stmt_ender(&mut self, token: &TokenType) {
        let candidate = match token {
            TokenType::IntegerLit(_) => true,
            TokenType::FloatLit(_) => true,
            TokenType::BooleanLit(_) => true,
            TokenType::StringLit(_) => true,
            TokenType::Identifier(_) => true,
            TokenType::Return => true,
            TokenType::RightBrace => true,
            TokenType::RightPar => {
                self.parenthesis_count -= 1;
                true
            }
            TokenType::LeftPar => {
                self.parenthesis_count += 1;
                false
            }
            _ => false,
        };
        self.stmt_ender = candidate && self.parenthesis_count <= 0
    }

    /// If the next character is 'c', consumes it and returns `true`. Returns `false` otherwise.
    fn next_match(&mut self, c: u8) -> bool {
        if self.is_at_end() {
            false
        } else if self.peek() != c {
            false
        } else {
            self.current += 1;
            true
        }
    }

    /// Returns the next character by advancing the iterator.
    fn advance(&mut self) -> u8 {
        let c = self.peek();
        self.current += 1;
        c
    }

    /// Returns the previous character and move the iterator back to it.
    fn back(&mut self) -> u8 {
        self.current -= 1;
        self.source[self.current]
    }

    /// Returns the next character *without* advancing the iterator.
    fn peek(&self) -> u8 {
        self.source[self.current]
    }

    /// Returns `true` if end of the source was reached.
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    /// Returns the location under the cursor (location of `self.peek`).
    fn get_current_loc(&self) -> Location {
        let start = Position {
            line: self.line,
            column: (self.current - self.line_start) as u32,
        };
        let mut end = start;
        end.column += 1;
        Location { start, end }
    }

    /// Returns the location of the `len` last characters.
    fn get_previous_loc(&self, len: usize) -> Location {
        let end = Position {
            line: self.line,
            column: (self.current - self.line_start) as u32,
        };

        let column = self.current - self.line_start;
        let column = if column <= len {
            0
        } else {
            column - len
        } as u32;
        let start = Position {
            line: self.line,
            column,
        };

        Location { start, end }
    }

    /// Returns the location of the token being actively built.
    fn get_current_token_loc(&self) -> Location {
        let start = Position {
            line: self.line,
            column: self.start as u32,
        };
        let end = Position {
            line: self.line,
            column: self.current as u32,
        };
        Location { start, end }
    }
}

pub fn scan(source: &str) -> (Vec<TokenType>, Diagnostics) {
    Scanner::new(source).scan()
}

fn as_keyword(token: &str) -> Option<TokenType> {
    match token {
        "abstract" => Some(TokenType::Abstract),
        "as" => Some(TokenType::As),
        "else" => Some(TokenType::Else),
        "expose" => Some(TokenType::Expose),
        "false" => Some(TokenType::False),
        "from" => Some(TokenType::From),
        "fun" => Some(TokenType::Fun),
        "if" => Some(TokenType::If),
        "impl" => Some(TokenType::Impl),
        "import" => Some(TokenType::Import),
        "let" => Some(TokenType::Let),
        "module" => Some(TokenType::Module),
        "pub" => Some(TokenType::Pub),
        "return" => Some(TokenType::Return),
        "runtime" => Some(TokenType::Runtime),
        "standalone" => Some(TokenType::Standalone),
        "struct" => Some(TokenType::Struct),
        "true" => Some(TokenType::True),
        "use" => Some(TokenType::Use),
        "var" => Some(TokenType::Var),
        "while" => Some(TokenType::While),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn keywords() {
        assert_eq!(as_keyword("foo"), None);
        assert_eq!(as_keyword("fun"), Some(TokenType::Fun));
    }
}
