//! # Scanner

use std::str;

use super::diagnostics::ScanError;
use super::tokens::token_kind::*;
use super::tokens::{SyntaxKind, Token, TokenStream};
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
        while self.scan_token() {}

        let token_stream = TokenStream::new(self.tokens, self.source);
        (token_stream, self.err)
    }

    /// Scan the next token, advancing the cursor. Return false when running out of tokens.
    fn scan_token(&mut self) -> bool {
        let token = match self.advance() {
            Some(c) => match c {
                // Single character tokens
                '(' => LeftPar,
                ')' => RightPar,
                '{' => LeftBrace,
                '}' => RightBrace,
                ',' => Comma,
                '.' => Dot,
                '-' => Minus,
                '+' => Plus,
                '*' => Star,
                '%' => Percent,
                '^' => Hat,

                // Two character tokens
                '!' => {
                    if self.next_match('=') {
                        BangEqual
                    } else {
                        Bang
                    }
                }
                ':' => {
                    if self.next_match(':') {
                        ColonColon
                    } else {
                        Colon
                    }
                }
                '=' => {
                    if self.next_match('=') {
                        EqualEqual
                    } else {
                        Equal
                    }
                }
                '>' => {
                    if self.next_match('=') {
                        GreaterEqual
                    } else {
                        Greater
                    }
                }
                '<' => {
                    if self.next_match('=') {
                        LessEqual
                    } else {
                        Less
                    }
                }
                '&' => {
                    if self.next_match('&') {
                        AndAnd
                    } else {
                        And
                    }
                }
                '|' => {
                    if self.next_match('|') {
                        OrOr
                    } else {
                        Or
                    }
                }
                '/' => {
                    if self.next_match('/') {
                        self.consume_comment();
                        Blank
                    } else {
                        Slash
                    }
                }

                // Whitespaces and end of line
                '\n' => {
                    self.new_line();
                    Blank
                }
                ' ' | '\t' | '\r' => {
                    self.consume_whitespaces();
                    Blank
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
                        Error
                    }
                }
            },
            None => return false,
        };
        self.add_token(token);
        true
    }

    // —————————————————————————————— Identifiers ——————————————————————————————— //

    /// Consumes consecutive alphanumeric characters into an identifier. Produce a keyword token if
    /// the identifier happens to be one of the reserved keywords.
    fn scan_identifier(&mut self) -> SyntaxKind {
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
            Identifier
        }
    }

    // ———————————————————————————————— Strings ————————————————————————————————— //

    /// Consumes any (non carriage-return) characters between double quotes
    fn scan_string(&mut self) -> SyntaxKind {
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
                        return Error;
                    }
                    _ => (),
                }
            } else {
                let loc = self.get_current_token_loc();
                self.err.report(ScanError::UnpairedDoubleQuote, loc);
                return Error;
            }
        }
        StringLit
    }

    // ———————————————————————————————— Numbers ————————————————————————————————— //
    // Note that we consume all alphanumeric characters, not only digits. This is //
    // done on purpose to emit an error in the scanner rather than in the parser. //
    // —————————————————————————————————————————————————————————————————————————— //

    /// Consumes consecutive digit characters and push a number token
    fn scan_number(&mut self) -> SyntaxKind {
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
        NumberLit
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Adds the token to the token list & update scanner state.
    fn add_token(&mut self, token: SyntaxKind) {
        let end = self.get_token_end();

        if token == Blank && self.tokens.len() > 0 && self.tokens.last().unwrap().t == Blank {
            // Last token is also a whitespace, merge both!
            self.tokens.last_mut().unwrap().end = end;
        } else {
            self.check_stmt_ender(&token);
            self.tokens.push(Token {
                t: token,
                start: self.start,
                end,
            });
        }

        // Set start location of the next token
        self.start = end;
    }

    /// Registers a new line, that is insert a semi column if necessary.
    fn new_line(&mut self) {
        if self.stmt_ender {
            let idx = self.get_token_end();
            self.tokens.push(Token {
                t: SemiColon,
                start: idx,
                end: idx,
            });
            self.stmt_ender = false;
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

    /// Consumes whitespace characters.
    fn consume_whitespaces(&mut self) {
        while let Some(c) = self.peek() {
            if c.is_whitespace() {
                self.advance();
            } else {
                return;
            }
        }
    }

    /// Check if the token can be a statement ender and updates internal state accordingly.
    fn check_stmt_ender(&mut self, token: &SyntaxKind) {
        let candidate = match token {
            True | False => true,
            Return => true,
            NumberLit => true,
            StringLit => true,
            Identifier => true,
            RightBrace => true,
            RightPar => {
                self.parenthesis_count -= 1;
                self.parenthesis_count == 0
            }
            LeftPar => {
                self.parenthesis_count += 1;
                false
            }
            Blank => self.stmt_ender,
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

fn as_keyword(token: &str) -> Option<SyntaxKind> {
    match token {
        "abstract" => Some(Abstract),
        "as" => Some(As),
        "else" => Some(Else),
        "expose" => Some(Expose),
        "false" => Some(False),
        "from" => Some(From),
        "fun" => Some(Fun),
        "if" => Some(If),
        "impl" => Some(Impl),
        "import" => Some(Import),
        "let" => Some(Let),
        "module" => Some(Module),
        "pub" => Some(Pub),
        "return" => Some(Return),
        "runtime" => Some(Runtime),
        "standalone" => Some(Standalone),
        "struct" => Some(Struct),
        "true" => Some(True),
        "use" => Some(Use),
        "var" => Some(Var),
        "while" => Some(While),
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
        assert_eq!(as_keyword("fun"), Some(Fun));
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

    #[test]
    fn loseless_scanner() {
        let program = r#"
        module   foo

        use std::string

        /// A super function!
        fun bar(a: i32) {
            let b = 0x42 * a
            let c = 012
            let s = "test :)"
        }
        "#;

        let (tokens, diagnostics) = scan(program);
        assert_eq!(&format!("{}", tokens), program);
        assert!(!diagnostics.has_error());
    }
}
