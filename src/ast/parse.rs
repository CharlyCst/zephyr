use super::ast::*;
use super::tokens::{Token, TokenType};
use crate::error::{ErrorHandler, Location};

pub struct Parser<'a, 'b> {
    err: &'b mut ErrorHandler<'a>,
    tokens: Vec<Token>,
    current: usize,
}

impl<'a, 'b> Parser<'a, 'b> {
    pub fn new(tokens: Vec<Token>, error_handler: &'b mut ErrorHandler<'a>) -> Parser<'a, 'b> {
        Parser {
            err: error_handler,
            tokens: tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut funs = Vec::new();

        while !self.is_at_end() {
            match self.function() {
                Ok(expr) => funs.push(expr),
                Err(()) => self.err.silent_report(),
            }
        }

        Program { funs: funs }
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
        let prev = self.current - 1;
        if prev > 0 {
            &self.tokens[prev]
        } else {
            &self.tokens[0]
        }
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

    // If the next token has type t, consume it and return true, return false otherwise
    fn next_match(&mut self, t: TokenType) -> bool {
        if self.peek().t == t {
            self.advance();
            true
        } else {
            false
        }
    }

    // Same as `next_match` but report an error if the token doesn't match
    fn next_match_report(&mut self, t: TokenType, err: &str) -> bool {
        if self.peek().t == t {
            self.advance();
            true
        } else {
            let loc = self.peek().loc;
            self.err.report(loc, String::from(err));
            false
        }
    }

    fn synchronize(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::SemiColon && !self.is_at_end() {
            token = self.advance();
        }
    }

    fn synchronize_fun(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::Fun && !self.is_at_end() {
            token = self.advance();
        }
    }

    fn consume_semi_colon(&mut self) {
        let semi_colon = self.advance();
        if semi_colon.t != TokenType::SemiColon {
            let loc = semi_colon.loc;
            self.err.report(
                loc,
                String::from("Expect statement ender, try to add a line break"),
            );
            self.synchronize();
        }
    }

    fn function(&mut self) -> Result<Function, ()> {
        let exported = self.next_match(TokenType::Export);
        if !self.next_match_report(TokenType::Fun, "Top level declaration must be functions") {
            self.synchronize_fun();
            return Err(());
        }
        let loc = self.peek().loc;
        let ident = match self.advance() {
            Token {
                t: TokenType::Identifier(ref x),
                ..
            } => x.clone(),
            _ => {
                self.err
                    .report(loc, String::from("Top level declaration must be functions"));
                self.synchronize_fun();
                return Err(());
            }
        };
        if !self.next_match_report(
            TokenType::LeftPar,
            "Parenthesis are expected after function declaration",
        ) {
            self.synchronize_fun();
            return Err(());
        }
        let params = self.parameters();
        if !self.next_match_report(
            TokenType::RightPar,
            "Parenthesis are expected after function declaration",
        ) {
            self.synchronize_fun();
            return Err(());
        }
        let result = self.result();
        let block = self.block()?;
        Ok(Function {
            ident: ident,
            params: params,
            result: result,
            block: block,
            exported: exported,
            loc: loc,
        })
    }

    fn parameters(&mut self) -> Vec<Variable> {
        let mut params = Vec::new();
        while let Token {
            t: TokenType::Identifier(ref param),
            loc: var_loc,
        } = self.advance()
        {
            let ident = param.clone();
            let var_loc = *var_loc;
            let token = self.advance();
            let loc = token.loc;
            let t = match token {
                Token {
                    t: TokenType::Identifier(ref x),
                    ..
                } => Some(x.clone()),
                _ => {
                    self.err
                        .report(loc, String::from("Expected parameter type"));
                    self.back();
                    None
                }
            };

            params.push(Variable {
                ident: ident,
                t: t,
                loc: var_loc,
            });
            if !self.next_match(TokenType::Comma) {
                return params;
            }
        }
        self.back();
        params
    }

    fn result(&mut self) -> Option<(String, Location)> {
        if let TokenType::Identifier(ref t) = self.peek().t {
            let result = Some((t.clone(), self.peek().loc));
            self.advance();
            result
        } else {
            None
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
            TokenType::Return => {
                self.advance();
                self.return_stmt()
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
        if !self.next_match_report(TokenType::SemiColon, "This statement is not complete") {
            return Err(());
        }
        Ok(Statement::ExprStmt {
            expr: Box::new(expr),
        })
    }

    fn assign_stmt(&mut self) -> Result<Statement, ()> {
        let (ident, loc) = match self.advance() {
            Token {
                t: TokenType::Identifier(ref x),
                loc,
            } => (x.clone(), loc),
            Token { loc, .. } => {
                let loc = *loc;
                self.err.report_internal(
                    loc,
                    String::from("Assignment statement does not start with an identifier"),
                );
                return Err(());
            }
        };
        let loc = *loc;
        if !self.next_match_report(
            TokenType::Equal,
            "Assignment identifier is not followed by an \"=\"",
        ) {
            self.synchronize();
            return Err(());
        }
        let expr = self.expression()?;
        self.consume_semi_colon();
        Ok(Statement::AssignStmt {
            var: Box::new(Variable {
                ident: ident,
                t: None,
                loc: loc,
            }),
            expr: Box::new(expr),
        })
    }

    // The `let` token must have been consumed
    fn let_stmt(&mut self) -> Result<Statement, ()> {
        let next = self.advance();
        let (ident, loc) = match next {
            Token {
                t: TokenType::Identifier(ref x),
                loc,
            } => (x.clone(), *loc),
            Token { loc, .. } => {
                let loc = *loc;
                self.err.report(
                    loc,
                    String::from("Let statement requires an identifier after the \"let\" keyword"),
                );
                return Err(());
            }
        };
        if !self.next_match_report(
            TokenType::Equal,
            "Let statement requires an \"=\" after the identifier",
        ) {
            self.synchronize();
            return Err(());
        }
        let expr = self.expression()?;
        self.consume_semi_colon();
        Ok(Statement::LetStmt {
            var: Box::new(Variable {
                ident: ident,
                t: None,
                loc: loc,
            }),
            expr: Box::new(expr),
        })
    }

    // The `if` token must have been consumed
    fn if_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression()?;
        if !self.next_match_report(
            TokenType::LeftBrace,
            "If statement requires an \"{\" after the condition",
        ) {
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
        if !self.next_match_report(
            TokenType::LeftBrace,
            "While statement requires an \"{\" after the condition",
        ) {
            return Err(());
        };
        let block = self.block()?;
        Ok(Statement::WhileStmt {
            expr: Box::new(expr),
            block: block,
        })
    }

    // The `return` token must have been consumed
    fn return_stmt(&mut self) -> Result<Statement, ()> {
        let loc = self.previous().loc;
        let expr = self.expression();
        match expr {
            Ok(e) => {
                self.consume_semi_colon();
                Ok(Statement::ReturnStmt {
                    expr: Some(e),
                    loc: loc,
                })
            }
            Err(()) => {
                self.back(); // expression consumes one character
                self.consume_semi_colon();
                Ok(Statement::ReturnStmt {
                    expr: None,
                    loc: loc,
                })
            }
        }
    }

    // The `{` token must have been consumed
    fn block(&mut self) -> Result<Block, ()> {
        let mut stmts = Vec::new();
        while !self.next_match(TokenType::RightBrace) && !self.is_at_end() {
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
                TokenType::Percent => BinaryOperator::Remainder,
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
            if !self.next_match_report(
                TokenType::RightPar,
                "Expected a closing parenthesis `)` to function call",
            ) {
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
            TokenType::NumberLit(n) => Ok(Expression::Literal {
                value: Value::Integer {
                    val: *n,
                    loc: token.loc,
                },
            }),
            TokenType::BooleanLit(b) => Ok(Expression::Literal {
                value: Value::Boolean {
                    val: *b,
                    loc: token.loc,
                },
            }),
            TokenType::Identifier(ref x) => Ok(Expression::Variable {
                var: Variable {
                    ident: x.clone(),
                    t: None,
                    loc: token.loc,
                },
            }),
            TokenType::LeftPar => {
                let expr = self.expression()?;
                if !self.next_match_report(TokenType::RightPar, "Expected closing parenthesis") {
                    return Err(());
                }
                Ok(expr)
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