use super::tokens::*;
use crate::error::{ErrorHandler, Location};
use std::collections::HashMap;

const RADIX: u32 = 10;

/// Fork Assembly Scanner, it produces tokens from source code.
pub struct Scanner<'a, 'b> {
    err: &'b mut ErrorHandler<'a>,
    f_id: u16,
    code: Vec<char>,
    start: usize,
    current: usize,
    keywords: HashMap<String, TokenType>,
    stmt_ender: bool,
}

impl<'a, 'b> Scanner<'a, 'b> {
    pub fn new(code: &str, f_id: u16, error_handler: &'b mut ErrorHandler<'a>) -> Scanner<'a, 'b> {
        let keywords = get_keyword_map();

        Scanner {
            err: error_handler,
            f_id: f_id,
            code: code.chars().collect(), // TODO: remove this copy
            start: 0,
            current: 0,
            keywords: keywords,
            stmt_ender: false,
        }
    }

    /// Scan the source code and return a vector of tokens.
    pub fn scan(&mut self) -> Vec<Token> {
        let mut tokens = Vec::new();
        while !self.is_at_end() {
            self.scan_token(&mut tokens);
            self.start = self.current;
        }
        self.add_token(&mut tokens, TokenType::EOF);

        tokens
    }

    fn scan_token(&mut self, tokens: &mut Vec<Token>) {
        match self.advance() {
            '(' => self.add_token(tokens, TokenType::LeftPar),
            ')' => self.add_token(tokens, TokenType::RightPar),
            '{' => self.add_token(tokens, TokenType::LeftBrace),
            '}' => self.add_token(tokens, TokenType::RightBrace),
            ',' => self.add_token(tokens, TokenType::Comma),
            '/' => {
                // Ignore comments
                if self.next_match('/') {
                    while !self.is_at_end() && self.peek() != '\n' {
                        self.advance();
                    }
                } else {
                    self.err
                        .report(self.get_loc(), String::from("Unexpected character \"/\""))
                }
            }
            '\n' => {
                if self.stmt_ender {
                    self.add_token(tokens, TokenType::SemiColon)
                }
                self.stmt_ender = false;
            }
            ' ' | '\t' | '\r' => (),
            c => {
                // Match literal
                if c.is_digit(RADIX) {
                    self.number(tokens)
                } else if c.is_alphabetic() {
                    self.identifier(tokens)
                } else if c == '"' {
                    self.string(tokens)
                } else {
                    self.err
                        .report(self.get_loc(), format!("Unexpected character \"{}\"", c))
                }
            }
        }
    }

    /// Return the current token location.
    fn get_loc(&self) -> Location {
        Location {
            pos: self.start as u32,
            len: (self.current - self.start) as u32,
            f_id: self.f_id,
        }
    }

    /// Return true if the end of file has been reached.
    fn is_at_end(&self) -> bool {
        self.current >= self.code.len()
    }

    /// Advance the cursor by one character and return it.
    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        c
    }

    /// Return the next character without consuming it.
    fn peek(&self) -> char {
        self.code[self.current]
    }

    /// If the next character match `c`, consume it and return true.
    /// Return false otherwise.
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

    /// Add a fresh token of type `t`.
    fn add_token(&mut self, tokens: &mut Vec<Token>, t: TokenType) {
        match t {
            TokenType::SemiColon => self.stmt_ender = false,
            TokenType::LeftBrace => self.stmt_ender = false,
            _ => self.stmt_ender = true,
        }
        let token = Token {
            t: t,
            loc: Location {
                pos: self.start as u32,
                len: (self.current - self.start) as u32,
                f_id: self.f_id,
            },
        };
        tokens.push(token);
    }

    /// Parse a number
    fn number(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && self.peek().is_digit(RADIX) {
            self.advance();
        }
        let str_val = self.code[self.start..self.current]
            .iter()
            .cloned()
            .collect::<String>();
        match str_val.parse::<u64>() {
            Ok(n) => self.add_token(tokens, TokenType::NumberLit(n)),
            Err(_) => self.err.report(
                self.get_loc(),
                format!("Could not parse {} as a number", str_val),
            ),
        }
    }

    /// Parse a string
    fn string(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && self.peek() != '"' {
            self.advance();
        }
        self.advance(); // Consume the closing '"'
        let str_val = self.code[(self.start + 1)..(self.current - 1)]
            .iter()
            .cloned()
            .collect::<String>();
        self.add_token(tokens, TokenType::StringLit(str_val));
    }

    /// Parse an identifier
    fn identifier(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && (self.peek().is_alphanumeric() || self.peek() == '.') {
            self.advance();
        }
        let ident = self.code[self.start..self.current]
            .iter()
            .cloned()
            .collect::<String>();
        match self.keywords.get(&ident) {
            Some(t) => {
                let t = t.clone();
                self.add_token(tokens, t);
            }
            None => self.add_token(tokens, TokenType::Identifier(ident)),
        }
    }
}
