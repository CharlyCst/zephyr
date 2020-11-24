use super::ast::*;
use super::tokens::{Token, TokenType};
use crate::error::{ErrorHandler, Location};

pub struct Parser<'a> {
    err: &'a mut ErrorHandler,
    tokens: Vec<Token>,
    current: usize,
    package_id: u32,
}

/// Works on a list of tokens and converts it into an Abstract Syntax Tree,
/// following the grammar of the language (defined in 'grammar.md')
impl<'a> Parser<'a> {
    pub fn new(tokens: Vec<Token>, package_id: u32, error_handler: &mut ErrorHandler) -> Parser {
        Parser {
            err: error_handler,
            tokens,
            current: 0,
            package_id,
        }
    }

    /// Main function converting a list of token in vectors of declarations
    /// (functions, use, expose)
    pub fn parse(&mut self) -> Program {
        let mut funs = Vec::new();
        let mut exposed = Vec::new();
        let mut used = Vec::new();

        let package = match self.package() {
            Ok(pkg) => pkg,
            Err(()) => {
                self.err.silent_report();
                // @TODO: In the future we may wan to parse the code anyway in order to provide feedback
                // even though the `package` declaration is missing.
                Package {
                    id: self.package_id,
                    name: String::from(""),
                    loc: Location {
                        pos: 0,
                        len: 0,
                        f_id: 0,
                    },
                    t: PackageType::Standard,
                    kind: PackageKind::Package,
                }
            }
        };

        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => match decl {
                    Declaration::Function(fun) => funs.push(fun),
                    Declaration::Use(uses) => used.push(uses),
                    Declaration::Expose(expose) => exposed.push(expose),
                },
                Err(()) => self.err.silent_report(),
            }
        }

        Program {
            package,
            funs,
            exposed,
            used,
        }
    }

    /// Is the last token of the file?
    fn is_at_end(&self) -> bool {
        match self.peek().t {
            TokenType::EOF => true,
            _ => false,
        }
    }

    /// Shows the current token without consuming it
    fn peek(&self) -> &Token {
        let cur = self.current;
        &self.tokens[cur]
    }

    // @TODO: test
    // In theory if the tokens list is valid, only EOF is at the last position
    // of the tokens list, so peekpeek <=> peek only for EOF (is_at_end == true)
    /// Shows the next token (after the current) if it exists, else shows the
    /// current token
    fn peekpeek(&self) -> &Token {
        let next = self.current + 1;
        if next >= self.tokens.len() {
            &self.tokens[next - 1]
        } else {
            &self.tokens[next]
        }
    }

    /// Shows the previous token (already consumed)
    fn previous(&self) -> &Token {
        let prev = self.current - 1;
        if prev > 0 {
            &self.tokens[prev]
        } else {
            &self.tokens[0]
        }
    }

    /// Consumes and returns the current token by moving current to the right
    fn advance(&mut self) -> &Token {
        let token = &self.tokens[self.current];
        if !self.is_at_end() {
            self.current += 1;
        }
        token
    }

    /// Moves the cursor one position to the left (marking already consumed
    /// tokens as not consumed)
    fn back(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    /// If the next token has type t, consume it and return true, return false
    /// otherwise
    fn next_match(&mut self, t: TokenType) -> bool {
        if self.peek().t == t {
            self.advance();
            true
        } else {
            false
        }
    }

    /// Same as `next_match` but report an error if the token doesn't match
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

    /// Consumes token until a semi-colon, used in the context of invalid lines
    fn synchronize(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::SemiColon && !self.is_at_end() {
            token = self.advance();
        }
    }

    /// Consumes until the next top level declaration
    fn synchronize_fun(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::Fun && !self.is_at_end() {
            token = self.advance();
        }
    }

    /// Expects the current token to be a semicolon and consumes it, throws an
    /// error if it's not
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

    /* All the following functions try to parse a grammar element, and
    recursively parse all sub-elements as defined in the gammar of the
    language */

    /// Parses the 'package' grammar element
    fn package(&mut self) -> Result<Package, ()> {
        let start = self.peek().loc;
        let package_type = if self.next_match(TokenType::Standalone) {
            PackageType::Standalone
        } else {
            PackageType::Standard
        };
        let package_kind = if self.next_match(TokenType::Runtime) {
            PackageKind::Runtime
        } else {
            PackageKind::Package
        };
        if !self.next_match_report(
            TokenType::Package,
            "Programs must start with a package declaration",
        ) {
            return Err(());
        }

        let token = self.advance();
        if let TokenType::StringLit(ref string) = token.t {
            let string = string.clone();
            let end = self.peek().loc;
            self.consume_semi_colon();
            Ok(Package {
                id: self.package_id,
                loc: start.merge(end),
                name: string,
                t: package_type,
                kind: package_kind,
            })
        } else {
            let loc = token.loc;
            self.synchronize();
            self.err.report(
                loc,
                String::from("'package' keyword should be followed by a double quoted path"),
            );
            Err(())
        }
    }

    /// Parses a 'declaration' that can be either a 'use', 'expose' or 'fun'
    fn declaration(&mut self) -> Result<Declaration, ()> {
        match self.peek().t {
            TokenType::Fun | TokenType::Pub => Ok(Declaration::Function(self.function()?)),
            TokenType::Use => Ok(Declaration::Use(self._use()?)),
            TokenType::Expose => Ok(Declaration::Expose(self.expose()?)),
            _ => {
                self.err.report(
                    self.peek().loc,
                    String::from(
                        "Top level declaration must be one of 'function', 'use', 'expose'",
                    ),
                );
                self.synchronize();
                Err(())
            }
        }
    }

    /// Parses the 'use' grammar element
    fn _use(&mut self) -> Result<Use, ()> {
        let start = self.peek().loc;
        if !self.next_match_report(TokenType::Use, "Use statement must start by 'use' keyword") {
            return Err(());
        }

        let token = self.advance();
        if let TokenType::StringLit(ref string) = token.t {
            let string = string.clone();
            let alias = if self.next_match(TokenType::As) {
                let token = self.advance();
                if let TokenType::Identifier(ref ident) = token.t {
                    Some(ident.clone())
                } else {
                    let loc = token.loc;
                    self.err.report(
                        loc,
                        String::from("'as' should be followed by an identifier"),
                    );
                    return Err(());
                }
            } else {
                None
            };
            let end = self.peek().loc;
            self.consume_semi_colon();
            Ok(Use {
                loc: start.merge(end),
                path: string,
                alias,
            })
        } else {
            let loc = token.loc;
            self.err.report(
                loc,
                String::from("'use' keyword should be followed by a double quoted path"),
            );
            Err(())
        }
    }

    /// Parses the 'expose' grammar element
    fn expose(&mut self) -> Result<Expose, ()> {
        let start = self.peek().loc;
        if !self.next_match_report(
            TokenType::Expose,
            "Expose statement must start by 'expose' keyword",
        ) {
            return Err(());
        }

        let token = self.advance();
        if let TokenType::Identifier(ref ident) = token.t {
            let ident = ident.clone();
            let alias = if self.next_match(TokenType::As) {
                let token = self.advance();
                if let TokenType::Identifier(ref alias_ident) = token.t {
                    Some(alias_ident.clone())
                } else {
                    let loc = token.loc;
                    self.err.report(
                        loc,
                        String::from("'as' should be followed by an identifier"),
                    );
                    return Err(());
                }
            } else {
                None
            };
            let end = self.peek().loc;
            self.consume_semi_colon();
            Ok(Expose {
                loc: start.merge(end),
                ident,
                alias,
            })
        } else {
            let loc = token.loc;
            self.err.report(
                loc,
                String::from("'expose' keyword should be followed by an identifier"),
            );
            Err(())
        }
    }

    /// Parses the 'function' grammar element
    fn function(&mut self) -> Result<Function, ()> {
        let is_pub = self.next_match(TokenType::Pub);
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
        let error = if result.is_some() {
            "A left brace '{' is expected at the beginning of the function body."
        } else {
            "Expected a type (': MyType') or a brace ('{')."
        };
        if !self.next_match_report(TokenType::LeftBrace, error) {
            self.synchronize_fun();
            return Err(());
        }
        let block = self.block()?;
        self.consume_semi_colon();
        Ok(Function {
            ident,
            params,
            result,
            body: Body::Zephyr(block),
            is_pub,
            loc,
        })
    }

    /// Parses the 'parameters' grammar element
    fn parameters(&mut self) -> Vec<Variable> {
        let mut params = Vec::new();
        while let Token {
            t: TokenType::Identifier(ref param),
            loc: var_loc,
        } = self.advance()
        {
            let ident = param.clone();
            let var_loc = *var_loc;
            if !self.next_match_report(
                TokenType::Colon,
                "Expected a type after parameter identifier",
            ) {
                return params;
            }
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
                ident,
                t,
                loc: var_loc,
            });
            if !self.next_match(TokenType::Comma) {
                return params;
            }
        }
        self.back();
        params
    }

    /// Parses the 'result' grammar element
    fn result(&mut self) -> Option<(String, Location)> {
        if self.next_match(TokenType::Colon) {
            let token = self.peek();
            if let TokenType::Identifier(ref t) = token.t {
                let result = Some((t.clone(), self.peek().loc));
                self.advance();
                result
            } else {
                let loc = token.loc;
                self.err.report(loc, String::from("Expected a type"));
                return None;
            }
        } else {
            None
        }
    }

    /// Parses the 'statement' grammar element
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

    /// Parses the 'expr_stmt' grammar element
    fn expr_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression()?;
        if !self.next_match_report(TokenType::SemiColon, "This statement is not complete") {
            return Err(());
        }
        Ok(Statement::ExprStmt {
            expr: Box::new(expr),
        })
    }

    /// Parses the 'assign_stmt' grammar element
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
                ident,
                t: None,
                loc,
            }),
            expr: Box::new(expr),
        })
    }

    /// Parses the 'let_stmt' grammar element (assuming the `let` token has
    /// been consumed )
    fn let_stmt(&mut self) -> Result<Statement, ()> {
        // The `let` token must have been consumed
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
                ident,
                t: None,
                loc,
            }),
            expr: Box::new(expr),
        })
    }

    /// Parses the 'if_stmt' grammar element (assuming the `if` token has
    /// been consumed )
    fn if_stmt(&mut self) -> Result<Statement, ()> {
        // The `if` token must have been consumed
        let expr = self.expression()?;
        if !self.next_match_report(
            TokenType::LeftBrace,
            "If statement requires an \"{\" after the condition",
        ) {
            return Err(());
        };
        let block = self.block()?;

        // Check for else clause
        if self.peek().t == TokenType::Else {
            self.advance();
            if !self.next_match_report(
                TokenType::LeftBrace,
                "If statement requires an \"{\" after else clause",
            ) {
                return Err(());
            };
            let else_block = self.block()?;
            self.consume_semi_colon();
            Ok(Statement::IfStmt {
                expr: Box::new(expr),
                block,
                else_block: Some(else_block),
            })
        } else {
            self.consume_semi_colon();
            Ok(Statement::IfStmt {
                expr: Box::new(expr),
                block,
                else_block: None,
            })
        }
    }

    /// Parses the 'while_stmt' grammar element (assuming the `while` token has
    /// been consumed )
    fn while_stmt(&mut self) -> Result<Statement, ()> {
        // The `while` token must have been consumed
        let expr = self.expression()?;
        if !self.next_match_report(
            TokenType::LeftBrace,
            "While statement requires an \"{\" after the condition",
        ) {
            return Err(());
        };
        let block = self.block()?;
        self.consume_semi_colon();
        Ok(Statement::WhileStmt {
            expr: Box::new(expr),
            block,
        })
    }

    /// Parses the 'return_stmt' grammar element (assuming the `return` token has
    /// been consumed )
    fn return_stmt(&mut self) -> Result<Statement, ()> {
        // The `return` token must have been consumed
        let loc = self.previous().loc;
        let expr = self.expression();
        match expr {
            Ok(e) => {
                self.consume_semi_colon();
                Ok(Statement::ReturnStmt { expr: Some(e), loc })
            }
            Err(()) => {
                self.back(); // expression consumes one character
                self.consume_semi_colon();
                Ok(Statement::ReturnStmt { expr: None, loc })
            }
        }
    }

    /// Parses the '{' grammar element (assuming the `{` token has been
    /// consumed )
    fn block(&mut self) -> Result<Block, ()> {
        // The `{` token must have been consumed
        let mut stmts = Vec::new();
        while !self.next_match(TokenType::RightBrace) && !self.is_at_end() {
            let next_expr = self.statement();
            match next_expr {
                Ok(e) => stmts.push(e),
                Err(()) => (),
            }
        }
        Ok(Block { stmts })
    }

    /// Parses the 'expression' grammar element. As the grammar is unambiguous,
    /// precedence are directly baked into the grammar: the lowest priority
    /// elements are processed first, and will recursively call elements of
    /// higher priority
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
                binop,
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
                binop,
                expr_right: Box::new(right_b_or),
            }
        }
        Ok(left_b_or)
    }

    fn bitwise_or(&mut self) -> Result<Expression, ()> {
        let mut left_b_and = self.bitwise_xor()?;

        while self.next_match(TokenType::Or) {
            let right_b_and = self.bitwise_xor()?;
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseOr,
                expr_right: Box::new(right_b_and),
            }
        }
        Ok(left_b_and)
    }

    fn bitwise_xor(&mut self) -> Result<Expression, ()> {
        let mut left_b_and = self.bitwise_and()?;

        while self.next_match(TokenType::Hat) {
            let right_b_and = self.bitwise_and()?;
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseXor,
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
                binop,
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
                binop,
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
        let mut expr = self.access()?;
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
                args,
            };
        }
        Ok(expr)
    }

    fn access(&mut self) -> Result<Expression, ()> {
        let mut namespace = self.primary()?;

        loop {
            if self.next_match(TokenType::Dot) {
                let field = self.primary()?;
                namespace = Expression::Access {
                    namespace: Box::new(namespace),
                    field: Box::new(field),
                }
            } else {
                break;
            }
        }
        Ok(namespace)
    }

    fn primary(&mut self) -> Result<Expression, ()> {
        let token = self.advance();

        match &token.t {
            TokenType::IntegerLit(n) => Ok(Expression::Literal {
                value: Value::Integer {
                    val: *n,
                    loc: token.loc,
                },
            }),
            TokenType::FloatLit(x) => Ok(Expression::Literal {
                value: Value::Float {
                    val: *x,
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
