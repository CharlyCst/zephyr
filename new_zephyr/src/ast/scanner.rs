//! # Scanner

use std::str;

use super::diagnostics::ScanError;
use super::tokens::{Token, TokenStream, TokenType};
use crate::diagnostics::{Diagnostics, Location};

struct Scanner<'a> {
    source: &'a str,
    chars: Peekable<'a>,
    tokens: Vec<Token>,
    err: Diagnostics,

    /// Index of the start of the current token
    start: u32,
    /// Number of opened parenthesis
    parenthesis_count: u32,
    /// Wether the next line break is a statement ender
    stmt_ender: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            chars: Peekable::new(source),
            tokens: Vec::new(),
            err: Diagnostics::default(),
            start: 0,
            parenthesis_count: 0,
            stmt_ender: false,
        }
    }

    pub fn scan(mut self) -> (TokenStream<'a>, Diagnostics) {
        while !self.is_at_end() {
            self.scan_token();
        }

        self.add_token(TokenType::EOF);
        let token_stream = TokenStream::new(self.tokens, self.source);
        (token_stream, self.err)
    }

    /// Scan the next token, advancing the cursor.
    fn scan_token(&mut self) {
        let token = match self.advance() {
            Some(c) => match c {
                // Single character tokens
                '(' => TokenType::LeftPar,
                ')' => TokenType::RightPar,
                '{' => TokenType::LeftBrace,
                '}' => TokenType::RightBrace,
                ',' => TokenType::Comma,
                '.' => TokenType::Dot,
                '-' => TokenType::Minus,
                '+' => TokenType::Plus,
                '*' => TokenType::Star,
                '%' => TokenType::Percent,
                '^' => TokenType::Hat,

                // Two character tokens
                '!' => {
                    if self.next_match('=') {
                        TokenType::BangEqual
                    } else {
                        TokenType::Bang
                    }
                }
                ':' => {
                    if self.next_match(':') {
                        TokenType::ColonColon
                    } else {
                        TokenType::Colon
                    }
                }
                '=' => {
                    if self.next_match('=') {
                        TokenType::EqualEqual
                    } else {
                        TokenType::Equal
                    }
                }
                '>' => {
                    if self.next_match('=') {
                        TokenType::GreaterEqual
                    } else {
                        TokenType::Greater
                    }
                }
                '<' => {
                    if self.next_match('=') {
                        TokenType::LessEqual
                    } else {
                        TokenType::Less
                    }
                }
                '&' => {
                    if self.next_match('&') {
                        TokenType::AndAnd
                    } else {
                        TokenType::And
                    }
                }
                '|' => {
                    if self.next_match('|') {
                        TokenType::OrOr
                    } else {
                        TokenType::Or
                    }
                }
                '/' => {
                    if self.next_match('/') {
                        self.consume_comment();
                        TokenType::CommentString
                    } else {
                        TokenType::Slash
                    }
                }

                // Whitespaces and end of line
                '\n' => {
                    self.new_line();
                    TokenType::NewLine
                }
                ' ' | '\t' | '\r' => {
                    self.consume_whitespace();
                    TokenType::Whitespace
                }

                // Advanced logic for multi-character tokens
                c => {
                    if c.is_ascii_digit() {
                        self.scan_number()
                    } else if c.is_ascii_alphabetic() || c == '_' {
                        self.scan_identifier()
                    } else if c == '"' {
                        self.scan_string()
                    } else {
                        let loc = self.get_previous_loc(1);
                        self.err.report(ScanError::UnexpectedCharacter(c), loc);
                        TokenType::Error
                    }
                }
            },
            None => TokenType::EOF,
        };
        self.add_token(token);
    }

    // —————————————————————————————— Identifiers ——————————————————————————————— //

    /// Consumes consecutive alphanumeric characters into an identifier. Produce a keyword token if
    /// the identifier happens to be one of the reserved keywords.
    fn scan_identifier(&mut self) -> TokenType {
        // Move until the end of the current identifier [a-zA-Z0-9_]
        // Note: we don't disambiguate with numbers here, the caller should do it
        while let Some(c) = self.peek() {
            if !c.is_alphanumeric() && c != '_' {
                break;
            }
            self.advance();
        }

        // Convert that sequence of chars to str
        let ident = if let Some(end) = self.peek_idx() {
            &self.source[self.start as usize..end as usize]
        } else {
            &self.source[self.start as usize..]
        };
        if let Some(keyword) = as_keyword(&ident) {
            keyword
        } else {
            TokenType::Identifier
        }
    }

    // ———————————————————————————————— Strings ————————————————————————————————— //

    /// Consumes any (non carriage-return) characters between double quotes
    fn scan_string(&mut self) -> TokenType {
        // Consume until the next char is a double quote
        loop {
            if let Some(c) = self.advance() {
                match c {
                    // Escape the next character
                    '\\' => {
                        self.advance();
                    }
                    '"' => break, // End of string
                    '\n' => {
                        // Exit if the double quote is not found on this line
                        let loc = self.get_current_token_loc();
                        self.err.report(ScanError::MultilineString, loc);
                        return TokenType::Error;
                    }
                    _ => (),
                }
            } else {
                let loc = self.get_current_token_loc();
                self.err.report(ScanError::UnpairedDoubleQuote, loc);
                return TokenType::Error;
            }
        }
        TokenType::StringLit
    }

    // ———————————————————————————————— Numbers ————————————————————————————————— //
    // Note that we consume all alphanumeric characters, not only digits. This is //
    // done on purpose to emit an error in the scanner rather than in the parser. //
    // —————————————————————————————————————————————————————————————————————————— //

    /// Consumes consecutive digit characters and push a number token
    fn scan_number(&mut self) -> TokenType {
        // Look for a basis (hexa or binary)
        match self.peek() {
            Some('x') => {
                self.advance();
            }
            Some('b') => {
                self.advance();
            }
            _ => (),
        };

        // Consume digits
        while let Some(c) = self.peek() {
            if !c.is_numeric() {
                break;
            }
            self.advance();
        }
        if self.peek() == Some('.') {
            while let Some(c) = self.peek() {
                if !c.is_numeric() {
                    break;
                }
                self.advance();
            }
        }
        TokenType::NumberLit
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Adds the token to the token list & update scanner state.
    fn add_token(&mut self, token: TokenType) {
        let end = self.get_token_end();

        self.check_stmt_ender(&token);
        self.tokens.push(Token {
            t: token,
            start: self.start,
            end,
        });

        // Set start location of the next token
        self.start = end;
    }

    /// Registers a new line, that is insert a semi column if necessary.
    fn new_line(&mut self) {
        if self.stmt_ender {
            let idx = self.get_token_end();
            self.tokens.push(Token {
                t: TokenType::SemiColon,
                start: idx,
                end: idx,
            });
        }
    }

    /// Consumes every character until the end of source or the next line break.
    fn consume_comment(&mut self) {
        while let Some(c) = self.peek() {
            if c == '\n' {
                break;
            }
            self.advance();
        }
    }

    fn consume_whitespace(&mut self) {
        while let Some(c) = self.peek() {
            if !c.is_whitespace() {
                break;
            }
            self.advance();
        }
    }

    /// Check if the token can be a statement ender and updates internal state accordingly.
    fn check_stmt_ender(&mut self, token: &TokenType) {
        let candidate = match token {
            TokenType::True => true,
            TokenType::False => true,
            TokenType::Return => true,
            TokenType::NumberLit => true,
            TokenType::StringLit => true,
            TokenType::Identifier => true,
            TokenType::RightBrace => true,
            TokenType::RightPar => {
                self.parenthesis_count -= 1;
                self.parenthesis_count == 0
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
    fn next_match(&mut self, c: char) -> bool {
        match self.peek() {
            Some(next_c) if next_c == c => {
                self.advance();
                true
            }
            _ => false,
        }
    }

    /// Returns the next character by advancing the iterator.
    fn advance(&mut self) -> Option<char> {
        self.chars.next().map(|(_, c)| c)
    }

    /// Returns the next character *without* advancing the iterator.
    fn peek(&mut self) -> Option<char> {
        self.chars.peek().map(|(_, c)| c)
    }

    /// Returns the next character and its index *without* advancing the iterator.
    fn peek_idx(&mut self) -> Option<u32> {
        self.chars.peek().map(|(idx, _)| idx as u32)
    }

    /// Returns `true` if end of the source was reached.
    fn is_at_end(&mut self) -> bool {
        self.peek().is_none()
    }

    // ———————————————————————————— Location getters ———————————————————————————— //

    /// Returns the index of the current token end.
    fn get_token_end(&mut self) -> u32 {
        if let Some(idx) = self.peek_idx() {
            idx
        } else {
            self.source.len() as u32
        }
    }

    /// Returns the location under the cursor (location of `self.peek`).
    // fn get_current_loc(&mut self) -> Location {
    //     let idx = self.get_token_end();
    //     Location {
    //         start: idx,
    //         end: idx + 1,
    //     }
    // }

    /// Returns the location of the `len` last characters.
    fn get_previous_loc(&mut self, len: u32) -> Location {
        let end = self.get_token_end();
        let start = 1 + end - len;
        Location { start, end }
    }

    /// Returns the location of the token being actively built.
    fn get_current_token_loc(&mut self) -> Location {
        let end = self.get_token_end();
        let start = self.start;
        Location { start, end }
    }
}

pub fn scan(source: &str) -> (TokenStream, Diagnostics) {
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

// ————————————————————————————————— Utils —————————————————————————————————— //

/// A simple peakable iterator over the characters of a string and their positions.
struct Peekable<'a> {
    chars: str::CharIndices<'a>,
    next: Option<(usize, char)>,
}

impl<'a> Peekable<'a> {
    fn new(source: &'a str) -> Self {
        Self {
            chars: source.char_indices(),
            next: None,
        }
    }

    fn peek(&mut self) -> Option<(usize, char)> {
        if self.next.is_none() {
            self.next = self.chars.next();
        }
        self.next
    }

    fn next(&mut self) -> Option<(usize, char)> {
        if self.next.is_some() {
            let c = self.next;
            self.next = None;
            c
        } else {
            self.chars.next()
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn keywords() {
        assert_eq!(as_keyword("foo"), None);
        assert_eq!(as_keyword("fun"), Some(TokenType::Fun));
    }

    #[test]
    fn peek() {
        let mut peekable = Peekable::new("Hello");
        assert_eq!(peekable.next(), Some((0, 'H')));
        assert_eq!(peekable.peek(), Some((1, 'e')));
        assert_eq!(peekable.peek(), Some((1, 'e')));
        assert_eq!(peekable.next(), Some((1, 'e')));
        assert_eq!(peekable.next(), Some((2, 'l')));
        assert_eq!(peekable.next(), Some((3, 'l')));
        assert_eq!(peekable.peek(), Some((4, 'o')));
        assert_eq!(peekable.next(), Some((4, 'o')));
        assert_eq!(peekable.peek(), None);
        assert_eq!(peekable.next(), None);
    }
}
