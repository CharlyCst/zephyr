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
    Value {
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
            Expression::Value { value: v } => match v {
                Value::Boolean(true) => write!(f, "true"),
                Value::Boolean(false) => write!(f, "false"),
                Value::Identifier(x) => write!(f, "{}", x),
                Value::Number(n) => write!(f, "{}", n),
            },
            Expression::Unary { unop, expr } => match unop {
                UnaryOperator::Not => write!(f, "!{}", expr),
                UnaryOperator::Minus => write!(f, "-{}", expr),
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

pub enum Statement {
    ExprStmt {
        expr: Box<Expression>,
    },
    LetStmt {
        ident: String,
        expr: Box<Expression>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::ExprStmt { expr } => write!(f, "{};", expr),
            Statement::LetStmt { ident, expr } => write!(f, "let {} = {};", ident, expr),
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

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            match self.statement() {
                Ok(expr) => stmts.push(expr),
                Err(()) => (),
            }
        }

        stmts
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

    fn statement(&mut self) -> Result<Statement, ()> {
        match self.peek().t {
            TokenType::Let => {
                self.advance();
                self.let_stmt()
            }
            _ => self.expr_stmt(),
        }
    }

    fn expr_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression()?;
        if self.next_match(TokenType::SemiColon) {
            Ok(Statement::ExprStmt {
                expr: Box::new(expr),
            })
        } else {
            let line = self.peek().line;
            self.error_handler
                .report(line, "This statement is not complete");
            Err(())
        }
    }

    // The let token must have been consumed
    fn let_stmt(&mut self) -> Result<Statement, ()> {
        let next = self.advance();
        let ident = match next {
            Token {
                t: TokenType::Identifier(ref x),
                ..
            } => x.clone(),
            Token { line, .. } => {
                let l = *line;
                self.error_handler.report(
                    l,
                    "Let statement requires an identifier after the \"let\" keyword",
                );
                return Err(());
            }
        };
        if !self.next_match(TokenType::Equal) {
            self.error_handler.report(
                self.peek().line,
                "Let statement requires an \"=\" after the identifier",
            );
            return Err(());
        }
        let expr = self.expression()?;
        let semi_colon = self.advance();
        if semi_colon.t != TokenType::SemiColon {
            let l = semi_colon.line;
            self.error_handler
                .report(l, "Expect statement ender, try to add a line break")
        }
        Ok(Statement::LetStmt {
            ident: ident,
            expr: Box::new(expr),
        })
    }

    fn expression(&mut self) -> Result<Expression, ()> {
        self.logical_or()
    }

    fn logical_or(&mut self) -> Result<Expression, ()> {
        let mut left_and = self.logical_and()?;

        while self.next_match(TokenType::OrOr) {
            let right_and = self.logical_and()?;
            left_and = Expression::Binary {
                expr_left: Box::new(left_and),
                binop: BinaryOperator::Or,
                expr_right: Box::new(right_and),
            }
        }
        Ok(left_and)
    }

    fn logical_and(&mut self) -> Result<Expression, ()> {
        let mut left_eq = self.equality()?;

        while self.next_match(TokenType::AndAnd) {
            let right_eq = self.logical_and()?;
            left_eq = Expression::Binary {
                expr_left: Box::new(left_eq),
                binop: BinaryOperator::And,
                expr_right: Box::new(right_eq),
            }
        }
        Ok(left_eq)
    }

    fn equality(&mut self) -> Result<Expression, ()> {
        let mut left_comp = self.comparison()?;

        loop {
            let binop = match self.peek().t {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => break,
            };
            self.advance();
            let right_comp = self.comparison()?;
            left_comp = Expression::Binary {
                expr_left: Box::new(left_comp),
                binop: binop,
                expr_right: Box::new(right_comp),
            }
        }
        Ok(left_comp)
    }

    fn comparison(&mut self) -> Result<Expression, ()> {
        let mut left_b_or = self.bitwise_or()?;

        loop {
            let binop = match self.peek().t {
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => break,
            };
            self.advance();
            let right_b_or = self.bitwise_or()?;
            left_b_or = Expression::Binary {
                expr_left: Box::new(left_b_or),
                binop: binop,
                expr_right: Box::new(right_b_or),
            }
        }
        Ok(left_b_or)
    }

    fn bitwise_or(&mut self) -> Result<Expression, ()> {
        let mut left_b_and = self.bitwise_and()?;

        while self.next_match(TokenType::Or) {
            let right_b_and = self.bitwise_and()?;
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseOr,
                expr_right: Box::new(right_b_and),
            }
        }
        Ok(left_b_and)
    }

    fn bitwise_and(&mut self) -> Result<Expression, ()> {
        let mut left_add = self.addition()?;

        while self.next_match(TokenType::And) {
            let right_add = self.addition()?;
            left_add = Expression::Binary {
                expr_left: Box::new(left_add),
                binop: BinaryOperator::BitwiseAnd,
                expr_right: Box::new(right_add),
            }
        }
        Ok(left_add)
    }

    fn addition(&mut self) -> Result<Expression, ()> {
        let mut left_mult = self.multiplication()?;

        loop {
            let binop = match self.peek().t {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => break,
            };
            self.advance();
            let right_mult = self.multiplication()?;
            left_mult = Expression::Binary {
                expr_left: Box::new(left_mult),
                binop: binop,
                expr_right: Box::new(right_mult),
            }
        }
        Ok(left_mult)
    }

    fn multiplication(&mut self) -> Result<Expression, ()> {
        let mut left_unary = self.unary()?;

        loop {
            let binop = match self.peek().t {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                _ => break,
            };
            self.advance();
            let right_unary = self.unary()?;
            left_unary = Expression::Binary {
                expr_left: Box::new(left_unary),
                binop: binop,
                expr_right: Box::new(right_unary),
            }
        }
        Ok(left_unary)
    }

    fn unary(&mut self) -> Result<Expression, ()> {
        let token = self.peek();

        match token.t {
            TokenType::Bang => {
                self.advance();
                Ok(Expression::Unary {
                    unop: UnaryOperator::Not,
                    expr: Box::new(self.unary()?),
                })
            }
            TokenType::Minus => {
                self.advance();
                Ok(Expression::Unary {
                    unop: UnaryOperator::Minus,
                    expr: Box::new(self.unary()?),
                })
            }
            _ => self.primary(),
        }
    }

    fn primary(&mut self) -> Result<Expression, ()> {
        let token = self.advance();

        match &token.t {
            TokenType::NumberLit(n) => Ok(Expression::Value {
                value: Value::Number(*n),
            }),
            TokenType::BooleanLit(b) => Ok(Expression::Value {
                value: Value::Boolean(*b),
            }),
            TokenType::Identifier(ref x) => Ok(Expression::Value {
                value: Value::Identifier(x.clone()),
            }),
            TokenType::LeftPar => {
                let expr = self.expression()?;
                let left_brace = self.advance();
                if left_brace.t != TokenType::RightPar {
                    let line = left_brace.line;
                    self.error_handler
                        .report(line, "Expected closing parenthesis");
                    Err(())
                } else {
                    Ok(expr)
                }
            }
            _ => {
                let line = token.line;
                self.error_handler.report(line, "Expected expression");
                Err(())
            }
        }
    }
}
