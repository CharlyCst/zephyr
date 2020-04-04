use crate::error::ErrorHandler;
use crate::scan::{Token, TokenType};
use std::fmt;

#[derive(Debug)]
pub enum Value {
    Identifier(String),
    Number(u64),
    Boolean(bool),
}

#[derive(Debug)]
pub enum BinaryOperator {
    Equal,
    NotEqual,
    LessEqual,
    GreaterEqual,
    Less,
    Greater,
    Plus,
    Minus,
    Multiply,
    Divide,
    BitwiseOr,
    BitwiseAnd,
    Or,
    And,
}

#[derive(Debug)]
pub enum UnaryOperator {
    Minus,
    Not,
}

#[derive(Debug)]
pub enum Expression {
    // Grouping {
    //     expr: Box<Expression>,
    // },
    Literal {
        value: Value,
    },
    Binary {
        expr_left: Box<Expression>,
        binop: BinaryOperator,
        expr_right: Box<Expression>,
    },
    Unary {
        unop: UnaryOperator,
        expr: Box<Expression>,
    },
}

impl fmt::Display for Expression {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expression::Literal { value: v } => match v {
                Value::Boolean(true) => write!(f, "true"),
                Value::Boolean(false) => write!(f, "false"),
                Value::Identifier(x) => write!(f, "{}", x),
                Value::Number(n) => write!(f, "{}", n),
            },
            Expression::Unary { unop, expr } => match unop {
                UnaryOperator::Not => write!(f, "-{}", expr),
                UnaryOperator::Minus => write!(f, "!{}", expr),
            },
            Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let op = match binop {
                    BinaryOperator::And => "&&",
                    BinaryOperator::BitwiseAnd => "&",
                    BinaryOperator::BitwiseOr => "|",
                    BinaryOperator::Divide => "/",
                    BinaryOperator::Equal => "==",
                    BinaryOperator::Greater => ">",
                    BinaryOperator::GreaterEqual => ">=",
                    BinaryOperator::Less => "<",
                    BinaryOperator::LessEqual => "<=",
                    BinaryOperator::Minus => "-",
                    BinaryOperator::Multiply => "*",
                    BinaryOperator::NotEqual => "!=",
                    BinaryOperator::Or => "||",
                    BinaryOperator::Plus => "+",
                };
                write!(f, "({} {} {})", expr_left, op, expr_right)
            }
        }
    }
}

pub struct Parser {
    error_handler: ErrorHandler,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
}

impl Parser {
    pub fn new(handler: ErrorHandler, tokens: Vec<Token>) -> Parser {
        Parser {
            error_handler: handler,
            tokens: tokens,
            start: 0,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Vec<Expression> {
        let mut exprs = Vec::new();

        while !self.is_at_end() {
            exprs.push(self.expression())
        }

        exprs
    }

    fn is_at_end(&self) -> bool {
        match self.peek().t {
            TokenType::EOF => true,
            _ => false,
        }
    }

    fn peek(&self) -> &Token {
        let cur = self.current;
        &self.tokens[cur]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        if !self.is_at_end() {
            self.current += 1;
        }
        token
    }

    fn next_match(&mut self, t: TokenType) -> bool {
        if self.peek().t == t {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expression(&mut self) -> Expression {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Expression {
        let mut left_and = self.logical_and();

        while self.next_match(TokenType::OrOr) {
            let right_and = self.logical_and();
            left_and = Expression::Binary {
                expr_left: Box::new(left_and),
                binop: BinaryOperator::Or,
                expr_right: Box::new(right_and),
            }
        }
        left_and
    }

    fn logical_and(&mut self) -> Expression {
        let mut left_eq = self.equality();

        while self.next_match(TokenType::AndAnd) {
            let right_eq = self.logical_and();
            left_eq = Expression::Binary {
                expr_left: Box::new(left_eq),
                binop: BinaryOperator::And,
                expr_right: Box::new(right_eq),
            }
        }
        left_eq
    }

    fn equality(&mut self) -> Expression {
        let mut left_comp = self.comparison();

        loop {
            let binop = match self.peek().t {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => break,
            };
            self.advance();
            let right_comp = self.comparison();
            left_comp = Expression::Binary {
                expr_left: Box::new(left_comp),
                binop: binop,
                expr_right: Box::new(right_comp),
            }
        }
        left_comp
    }

    fn comparison(&mut self) -> Expression {
        let mut left_b_or = self.bitwise_or();

        loop {
            let binop = match self.peek().t {
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => break,
            };
            self.advance();
            let right_b_or = self.bitwise_or();
            left_b_or = Expression::Binary {
                expr_left: Box::new(left_b_or),
                binop: binop,
                expr_right: Box::new(right_b_or),
            }
        }
        left_b_or
    }

    fn bitwise_or(&mut self) -> Expression {
        let mut left_b_and = self.bitwise_and();

        while self.next_match(TokenType::Or) {
            let right_b_and = self.bitwise_and();
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseOr,
                expr_right: Box::new(right_b_and),
            }
        }
        left_b_and
    }

    fn bitwise_and(&mut self) -> Expression {
        let mut left_add = self.addition();

        while self.next_match(TokenType::And) {
            let right_add = self.addition();
            left_add = Expression::Binary {
                expr_left: Box::new(left_add),
                binop: BinaryOperator::BitwiseAnd,
                expr_right: Box::new(right_add),
            }
        }
        left_add
    }

    fn addition(&mut self) -> Expression {
        let mut left_mult = self.multiplication();

        loop {
            let binop = match self.peek().t {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => break,
            };
            self.advance();
            let right_mult = self.multiplication();
            left_mult = Expression::Binary {
                expr_left: Box::new(left_mult),
                binop: binop,
                expr_right: Box::new(right_mult),
            }
        }
        left_mult
    }

    fn multiplication(&mut self) -> Expression {
        let mut left_unary = self.unary();

        loop {
            let binop = match self.peek().t {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => break,
            };
            self.advance();
            let right_unary = self.unary();
            left_unary = Expression::Binary {
                expr_left: Box::new(left_unary),
                binop: binop,
                expr_right: Box::new(right_unary),
            }
        }
        left_unary
    }

    fn unary(&mut self) -> Expression {
        let token = self.peek();

        match token.t {
            TokenType::Bang => {
                self.advance();
                Expression::Unary {
                    unop: UnaryOperator::Not,
                    expr: Box::new(self.unary()),
                }
            }
            TokenType::Minus => {
                self.advance();
                Expression::Unary {
                    unop: UnaryOperator::Minus,
                    expr: Box::new(self.unary()),
                }
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Expression {
        let token = self.advance();

        match &token.t {
            TokenType::NumberLit(n) => Expression::Literal {
                value: Value::Number(*n),
            },
            TokenType::BooleanLit(b) => Expression::Literal {
                value: Value::Boolean(*b),
            },
            TokenType::Identifier(ref x) => Expression::Literal {
                value: Value::Identifier(x.clone()),
            },
            TokenType::LeftPar => {
                let expr = self.expression();
                let left_brace = self.advance();
                if left_brace.t != TokenType::RightPar {
                    // Handle error
                    println!("ERROR missing parenthesis");
                }
                expr
            }
            _ => {
                // Handle error
                println!("{:?}", token);
                println!("ERROR in parsing primary");
                Expression::Literal {
                    value: Value::Number(777),
                }
            }
        }
    }
}
