use crate::error::ErrorHandler;
use std::collections::HashMap;

const RADIX: u32 = 10;

#[derive(Clone, Debug, PartialEq)]
pub enum TokenType {
    // Single character
    LeftPar,
    RightPar,
    LeftBrace,
    RightBrace,
    Comma,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Bang,
    Equal,
    Greater,
    Less,
    And,
    Or,

    // Two characters
    BangEqual,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    AndAnd,
    OrOr,

    // Literals
    Identifier(String),
    NumberLit(u64),
    BooleanLit(bool),
    // StringLit(String),

    // Keywords
    Let,
    Var,
    Fun,
    If,
    Else,
    While,
    Return,
    True,
    False,

    // Other
    SemiColon,
    EOF,
}

#[derive(Debug)]
pub struct Token {
    pub t: TokenType,
    pub line: usize,
    pub pos: usize,
    pub len: usize,
}

pub struct Scanner {
    error_handler: ErrorHandler,
    code: Vec<char>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<String, TokenType>,
    stmt_ender: bool,
}

impl Scanner {
    pub fn new(handler: ErrorHandler, code: String) -> Scanner {
        let keywords: HashMap<String, TokenType> = [
            (String::from("let"), TokenType::Let),
            (String::from("var"), TokenType::Var),
            (String::from("fun"), TokenType::Fun),
            (String::from("If"), TokenType::If),
            (String::from("else"), TokenType::Else),
            (String::from("while"), TokenType::While),
            (String::from("return"), TokenType::Return),
            (String::from("true"), TokenType::True),
            (String::from("false"), TokenType::False),
        ]
        .iter()
        .cloned()
        .collect();

        Scanner {
            error_handler: handler,
            code: code.chars().collect(),
            start: 0,
            current: 0,
            line: 1,
            keywords: keywords,
            stmt_ender: false,
        }
    }

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

    fn scan_token(&mut self, tokens: &mut Vec<Token>) {
        match self.advance() {
            '(' => self.add_token(tokens, TokenType::LeftPar),
            ')' => self.add_token(tokens, TokenType::RightPar),
            '{' => self.add_token(tokens, TokenType::LeftBrace),
            '}' => self.add_token(tokens, TokenType::RightBrace),
            ',' => self.add_token(tokens, TokenType::Comma),
            '.' => self.add_token(tokens, TokenType::Dot),
            '-' => self.add_token(tokens, TokenType::Minus),
            '+' => self.add_token(tokens, TokenType::Plus),
            '*' => self.add_token(tokens, TokenType::Star),
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
                self.line += 1;
                if self.stmt_ender {
                    self.add_token(tokens, TokenType::SemiColon)
                }
            }
            ' ' | '\t' | '\r' => (),
            c => {
                if c.is_digit(RADIX) {
                    self.number(tokens)
                } else if c.is_alphabetic() {
                    self.identifier(tokens)
                }
            }
        }
    }

    fn check_stmt_ender(&mut self, token: &Token) {
        match token.t {
            TokenType::NumberLit(_) => self.stmt_ender = true,
            TokenType::BooleanLit(_) => self.stmt_ender = true,
            TokenType::Identifier(_) => self.stmt_ender = true,
            TokenType::RightBrace => self.stmt_ender = true,
            TokenType::RightPar => self.stmt_ender = true,
            _ => self.stmt_ender = false,
        }
    }

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

    fn add_token(&mut self, tokens: &mut Vec<Token>, t: TokenType) {
        let token = Token {
            t: t,
            line: self.line,
            pos: self.start,
            len: self.current - self.start,
        };
        self.check_stmt_ender(&token);
        tokens.push(token);
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.code.len()
    }

    fn advance(&mut self) -> char {
        let c = self.peek();
        self.current += 1;
        c
    }

    fn peek(&self) -> char {
        self.code[self.current]
    }

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
            Err(_) => self.error_handler.report(
                self.line,
                &format!("Could not parse {} as a number", str_val),
            ),
        }
    }

    fn identifier(&mut self, tokens: &mut Vec<Token>) {
        while !self.is_at_end() && self.peek().is_alphanumeric() {
            self.advance();
        }
        let ident = self.code[self.start..self.current]
            .iter()
            .cloned()
            .collect::<String>();
        match self.keywords.get(&ident) {
            Some(t) => match t {
                TokenType::True => self.add_token(tokens, TokenType::BooleanLit(true)),
                TokenType::False => self.add_token(tokens, TokenType::BooleanLit(false)),
                token_type => {
                    let t = token_type.clone();
                    self.add_token(tokens, t)
                }
            },
            None => self.add_token(tokens, TokenType::Identifier(ident)),
        }
    }
}
