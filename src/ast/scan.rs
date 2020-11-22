use super::tokens::*;
use crate::error::{ErrorHandler, Location};
use std::collections::HashMap;

const RADIX: u32 = 10;

/// Stores source code as a vector of chars and provides functions to convert
/// the source code to a list of tokens.
pub struct Scanner<'a> {
    err: &'a mut ErrorHandler,
    f_id: u16,
    code: Vec<char>,
    start: usize,
    current: usize,
    keywords: HashMap<String, TokenType>,
    stmt_ender: bool,
    parenthesis_count: i32,
}

impl<'a> Scanner<'a> {
    // f_id MUST exist, no check performed.
    pub fn new(f_id: u16, error_handler: &'a mut ErrorHandler) -> Scanner<'a> {
        let keywords: HashMap<String, TokenType> = [
            (String::from("as"), TokenType::As),
            (String::from("else"), TokenType::Else),
            (String::from("expose"), TokenType::Expose),
            (String::from("false"), TokenType::False),
            (String::from("fun"), TokenType::Fun),
            (String::from("if"), TokenType::If),
            (String::from("let"), TokenType::Let),
            (String::from("package"), TokenType::Package),
            (String::from("pub"), TokenType::Pub),
            (String::from("return"), TokenType::Return),
            (String::from("standalone"), TokenType::Standalone),
            (String::from("true"), TokenType::True),
            (String::from("use"), TokenType::Use),
            (String::from("var"), TokenType::Var),
            (String::from("while"), TokenType::While),
        ]
        .iter()
        .cloned()
        .collect();

        // f_id MUST exist
        let code = error_handler.get_file(f_id).unwrap();

        Scanner {
            code: code.chars().collect(), // TODO: remove this copy
            err: error_handler,
            f_id,
            start: 0,
            current: 0,
            keywords,
            stmt_ender: false,
            parenthesis_count: 0,
        }
    }

    /// Main function starting the conversion to tokens
    pub fn scan(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.is_at_end() {
            self.scan_token(&mut tokens);
            self.start = self.current;
        }
        if self.stmt_ender {
            self.add_token(&mut tokens, TokenType::SemiColon);
        }
        self.add_token(&mut tokens, TokenType::EOF);

        tokens
    }

    /// Consumes all characters giving enough context to convert a section of
    /// code to tokens
    fn scan_token(&mut self, tokens: &mut Vec<Token>) {
        match self.advance() {
            '(' => self.add_token(tokens, TokenType::LeftPar),
            ')' => self.add_token(tokens, TokenType::RightPar),
            '{' => self.add_token(tokens, TokenType::LeftBrace),
            '}' => self.add_token(tokens, TokenType::RightBrace),
            ',' => self.add_token(tokens, TokenType::Comma),
            ':' => self.add_token(tokens, TokenType::Colon),
            '.' => self.add_token(tokens, TokenType::Dot),
            '-' => self.add_token(tokens, TokenType::Minus),
            '+' => self.add_token(tokens, TokenType::Plus),
            '*' => self.add_token(tokens, TokenType::Star),
            '%' => self.add_token(tokens, TokenType::Percent),
            '^' => self.add_token(tokens, TokenType::Hat),
            '!' => {
                if self.next_match('=') {
                    self.add_token(tokens, TokenType::BangEqual)
                } else {
                    self.add_token(tokens, TokenType::Bang)
                }
            }
            '=' => {
                if self.next_match('=') {
                    self.add_token(tokens, TokenType::EqualEqual)
                } else {
                    self.add_token(tokens, TokenType::Equal)
                }
            }
            '>' => {
                if self.next_match('=') {
                    self.add_token(tokens, TokenType::GreaterEqual)
                } else {
                    self.add_token(tokens, TokenType::Greater)
                }
            }
            '<' => {
                if self.next_match('=') {
                    self.add_token(tokens, TokenType::LessEqual)
                } else {
                    self.add_token(tokens, TokenType::Less)
                }
            }
            '&' => {
                if self.next_match('&') {
                    self.add_token(tokens, TokenType::AndAnd)
                } else {
                    self.add_token(tokens, TokenType::And)
                }
            }
            '|' => {
                if self.next_match('|') {
                    self.add_token(tokens, TokenType::OrOr)
                } else {
                    self.add_token(tokens, TokenType::Or)
                }
            }
            '/' => {
                if self.next_match('/') {
                    // Ignore comments
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                } else {
                    self.add_token(tokens, TokenType::Slash)
                }
            }
            '\n' => {
                if self.stmt_ender {
                    self.add_token(tokens, TokenType::SemiColon)
                }
            }
            ' ' | '\t' | '\r' => (),
            // Advanced logic for multi-character tokens
            c => {
                if c.is_digit(RADIX) {
                    self.number(tokens)
                } else if c.is_alphabetic() || c == '_' {
                    self.identifier(tokens)
                } else if c == '"' {
                    self.string(tokens);
                } else {
                    self.err
                        .report(self.get_loc(), format!("Unexpected character \"{}\"", c))
                }
            }
        }
    }

    /// Checks if a statement ender (;) should be added in the next carriage
    /// return
    fn check_stmt_ender(&mut self, token: &Token) {
        let candidate = match token.t {
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

    /// Checks if the next character is a given char and consumes it if true.
    fn next_match(&mut self, c: char) -> bool {
        if self.is_at_end() {
            false
        } else if self.code[self.current] != c {
            false
        } else {
            self.current += 1;
            true
        }
    }

    /// Wrapper to initialize a token while keeping track of its location, and
    /// sets the statement ender flag
    fn add_token(&mut self, tokens: &mut Vec<Token>, t: TokenType) {
        let token = Token {
            t,
            loc: Location {
                pos: self.start as u32,
                len: (self.current - self.start) as u32,
                f_id: self.f_id,
            },
        };
        self.check_stmt_ender(&token);
        tokens.push(token);
    }

    fn get_loc(&self) -> Location {
        Location {
            pos: self.start as u32,
            len: (self.current - self.start) as u32,
            f_id: self.f_id,
        }
    }

    /// Is the last character of the file?
    fn is_at_end(&self) -> bool {
        self.current >= self.code.len()
    }

    /// Move the cursor one position to the right and return the character
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        c
    }

    /// Returns the current character
    fn peek(&self) -> char {
        self.code[self.current]
    }

    /// Consumes consecutive digit characters and push a number token
    fn number(&mut self, tokens: &mut Vec<Token>) {
        let mut is_integer = true;
        let mut radix = RADIX;
        if self.peek() == 'x' {
            radix = 16;
            self.advance();
            self.start = self.current;
        } else if self.peek() == 'b' {
            radix = 2;
            self.advance();
            self.start = self.current;
        }
        while !self.is_at_end() && self.peek().is_digit(radix) {
            self.advance();
        }
        if self.peek() == '.' {
            self.advance();
            is_integer = false;
            while !self.is_at_end() && self.peek().is_digit(radix) {
                self.advance();
            }
        }
        let str_val = self.code[self.start..self.current]
            .iter()
            .cloned()
            .collect::<String>();
        if is_integer {
            match u64::from_str_radix(&str_val, radix) {
                Ok(n) => self.add_token(tokens, TokenType::IntegerLit(n)),
                Err(_) => self.err.report(
                    self.get_loc(),
                    format!("Could not parse {} as an integer.", str_val),
                ),
            }
        } else {
            if radix != RADIX {
                self.err.report(
                    self.get_loc(),
                    String::from("Float numbers can only be written in base 10."),
                );
                return ();
            }
            match str_val.parse::<f64>() {
                Ok(x) => self.add_token(tokens, TokenType::FloatLit(x)),
                Err(_) => self.err.report(
                    self.get_loc(),
                    format!("Could not parse {} as a float.", str_val),
                ),
            }
        }
    }

    /// Consumes any (non carriage-return) characters between double quotes
    fn string(&mut self, tokens: &mut Vec<Token>) {
        // Consume until the next char is a double quote
        while !self.is_at_end() && self.peek() != '"' {
            let c = self.advance();
            // Exit if the double quote is not found on this line
            if c == '\n' {
                self.err.report(
                    self.get_loc(),
                    String::from("string literal should start and end on the same line"),
                );
            }
        }
        // Advance to consume the ending double quote
        self.advance();
        let str_val = self.code[self.start + 1..self.current - 1]
            .iter()
            .cloned()
            .collect::<String>();
        self.add_token(tokens, TokenType::StringLit(str_val));
    }

    /// Converts a sequence of chars to a keyword, an itendifier or a boolean
    /// litteral
    fn identifier(&mut self, tokens: &mut Vec<Token>) {
        // Move until the end of the current identifier [a-zA-Z0-9_]
        // Note: we don't disambiguate with numbers here, the caller should do it
        while !self.is_at_end() && self.peek().is_alphanumeric() || self.peek() == '_' {
            self.advance();
        }
        // Convert that sequence of chars to a string
        let ident = self.code[self.start..self.current]
            .iter()
            .cloned()
            .collect::<String>();
        match self.keywords.get(&ident) {
            // Check if the string is a keyword
            Some(t) => match t {
                // Booleans are literals but included in the keywords hashmap
                // for convenience
                TokenType::True => self.add_token(tokens, TokenType::BooleanLit(true)),
                TokenType::False => self.add_token(tokens, TokenType::BooleanLit(false)),
                token_type => {
                    let t = token_type.clone();
                    self.add_token(tokens, t)
                }
            },
            // If it's not a keyword, it's an identifier
            None => self.add_token(tokens, TokenType::Identifier(ident)),
        }
    }
}
