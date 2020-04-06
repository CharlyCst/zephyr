use crate::error::ErrorHandler;
use crate::scan::{Token, TokenType};
use std::fmt;

pub enum Value {
    Identifier(String),
    Number(u64),
    Boolean(bool),
}

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

pub enum UnaryOperator {
    Minus,
    Not,
}

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
    Call {
        fun: Box<Expression>,
        args: Vec<Expression>,
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
            Expression::Call { fun, args } => write!(
                f,
                "{}({})",
                fun,
                args.iter()
                    .map(|arg| format!("{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
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
    AssignStmt {
        ident: String,
        expr: Box<Expression>,
    },
    IfStmt {
        expr: Box<Expression>,
        block: Block,
    },
    WhileStmt {
        expr: Box<Expression>,
        block: Block,
    },
}

pub struct Block {
    stmts: Vec<Statement>,
}

impl fmt::Display for Block {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut strs = Vec::new();
        strs.push(String::from("{"));
        for stmt in self.stmts.iter() {
            strs.push(format!("    {}", stmt));
        }
        strs.push(String::from("};"));
        write!(
            f,
            "{}",
            strs.iter().map(|s| &**s).collect::<Vec<&str>>().join("\n"),
        )
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Statement::ExprStmt { expr } => write!(f, "{};", expr),
            Statement::LetStmt { ident, expr } => write!(f, "let {} = {};", ident, expr),
            Statement::AssignStmt { ident, expr } => write!(f, "{} = {};", ident, expr),
            Statement::IfStmt { expr, block } => write!(f, "if {} {}", expr, block),
            Statement::WhileStmt { expr, block } => write!(f, "while {} {}", expr, block),
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

    pub fn success(&self) -> bool {
        self.error_handler.success()
    }

    pub fn parse(&mut self) -> Vec<Statement> {
        let mut stmts = Vec::new();

        while !self.is_at_end() {
            match self.statement() {
                Ok(expr) => stmts.push(expr),
                Err(()) => self.error_handler.silent_report(),
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

    fn peekpeek(&self) -> &Token {
        let next = self.current + 1;
        if next >= self.tokens.len() {
            &self.tokens[next - 1]
        } else {
            &self.tokens[next]
        }
    }

    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        if !self.is_at_end() {
            self.current += 1;
        }
        token
    }

    fn back(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    fn next_match(&mut self, t: TokenType) -> bool {
        if self.peek().t == t {
            self.advance();
            true
        } else {
            false
        }
    }

    fn synchronize(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::SemiColon && !self.is_at_end() {
            token = self.advance();
        }
    }

    fn consume_semi_colon(&mut self) {
        let semi_colon = self.advance();
        if semi_colon.t != TokenType::SemiColon {
            let l = semi_colon.line;
            self.error_handler
                .report(l, "Expect statement ender, try to add a line break");
            self.synchronize();
        }
    }

    fn statement(&mut self) -> Result<Statement, ()> {
        match self.peek().t {
            TokenType::Let => {
                self.advance();
                self.let_stmt()
            }
            TokenType::If => {
                self.advance();
                self.if_stmt()
            }
            TokenType::While => {
                self.advance();
                self.while_stmt()
            }
            TokenType::Identifier(_) => match self.peekpeek().t {
                TokenType::Equal => self.assign_stmt(),
                _ => self.expr_stmt(),
            },
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

    fn assign_stmt(&mut self) -> Result<Statement, ()> {
        let ident = match self.advance() {
            Token {
                t: TokenType::Identifier(ref x),
                ..
            } => x.clone(),
            Token { line, .. } => {
                let l = *line;
                self.error_handler
                    .report_internal(l, "Assignment statement does not start with an identifier");
                return Err(());
            }
        };
        match self.advance() {
            Token {
                t: TokenType::Equal,
                ..
            } => (),
            Token { line, .. } => {
                let l = *line;
                self.error_handler
                    .report_internal(l, "Assignment identifier is not followed by an \"=\"");
                return Err(());
            }
        };
        let expr = self.expression()?;
        self.consume_semi_colon();
        Ok(Statement::AssignStmt {
            ident: ident,
            expr: Box::new(expr),
        })
    }

    // The `let` token must have been consumed
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
        self.consume_semi_colon();
        Ok(Statement::LetStmt {
            ident: ident,
            expr: Box::new(expr),
        })
    }

    // The `if` token must have been consumed
    fn if_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression()?;
        if !self.next_match(TokenType::LeftBrace) {
            self.error_handler.report(
                self.peek().line,
                "If statement requires an \"{\" after the condition",
            );
            return Err(());
        };
        let block = self.block()?;
        Ok(Statement::IfStmt {
            expr: Box::new(expr),
            block: block,
        })
    }

    // The `while` token must have been consumed
    fn while_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression()?;
        if !self.next_match(TokenType::LeftBrace) {
            self.error_handler.report(
                self.peek().line,
                "While statement requires an \"{\" after the condition",
            );
            return Err(());
        };
        let block = self.block()?;
        Ok(Statement::WhileStmt {
            expr: Box::new(expr),
            block: block,
        })
    }

    // The `{` token must have been consumed
    fn block(&mut self) -> Result<Block, ()> {
        let mut stmts = Vec::new();
        while !self.next_match(TokenType::RightBrace) {
            let next_expr = self.statement();
            match next_expr {
                Ok(e) => stmts.push(e),
                Err(()) => (),
            }
        }
        self.consume_semi_colon();
        Ok(Block { stmts: stmts })
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
            _ => self.call(),
        }
    }

    fn call(&mut self) -> Result<Expression, ()> {
        let mut expr = self.primary()?;
        while self.next_match(TokenType::LeftPar) {
            let args = self.arguments();
            // println!("{}", self.peek());
            if !self.next_match(TokenType::RightPar) {
                let line = self.peek().line;
                self.error_handler
                    .report(line, "Expected a closing parenthesis `)` to function call");
                self.synchronize();
                return Err(());
            }
            expr = Expression::Call {
                fun: Box::new(expr),
                args: args,
            };
        }
        Ok(expr)
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
            _ => Err(()),
        }
    }

    fn arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        while let Ok(expr) = self.expression() {
            args.push(expr);
            if !self.next_match(TokenType::Comma) {
                return args;
            }
        }
        self.back(); // expression consume one token when failing
        args
    }
}
