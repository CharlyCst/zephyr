use super::ast::AsmStatement;
use super::opcode_to_asm::{opcode_to_asm, Argument};
use super::asm_tokens::{Token, TokenType};
use super::ast;
use crate::error::{ErrorHandler, Location};

enum Declaration {
    Expose(ast::Expose),
    Fun(ast::Function),
}

/// Zephyr assembly parser, it consumes tokens to produces MIR.
pub struct Parser<'a> {
    err: &'a mut ErrorHandler,
    tokens: Vec<Token>,
    current: usize, // current token index
    package_id: u32,
}

impl<'a> Parser<'a> {
    pub fn new(
        tokens: Vec<Token>,
        package_id: u32,
        error_handler: &'a mut ErrorHandler,
    ) -> Parser<'a> {
        Parser {
            err: error_handler,
            tokens: tokens,
            current: 0,
            package_id,
        }
    }

    /// Convert the list of tokens into MIR
    pub fn parse(&mut self) -> ast::Program {
        let mut funs = Vec::new();
        let mut exposed = Vec::new();

        let package = match self.package() {
            Ok(pkg) => pkg,
            Err(_) => {
                self.err.silent_report(); // Error message is already emited by self.package.
                ast::Package {
                    path: String::from(""),
                    loc: Location::dummy(),
                }
            }
        };

        while !self.is_at_end() {
            match self.declaration() {
                Ok(decl) => match decl {
                    Declaration::Expose(e) => exposed.push(e),
                    Declaration::Fun(fun) => funs.push(fun),
                },
                Err(()) => self.err.silent_report(),
            }
        }

        ast::Program {
            package_id: self.package_id,
            package: package,
            exposed: exposed,
            funs: funs,
            used: vec![],
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

    /// Consumes and returns the current token by moving current to the right
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

    /// Consumes token until a semi-colon, used in the context of invalid lines
    fn synchronize(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::SemiColon && !self.is_at_end() {
            token = self.advance();
        }
    }

    /// Parses the 'package' grammar element
    fn package(&mut self) -> Result<ast::Package, ()> {
        if !self.next_match_report(
            TokenType::Package,
            "File must start with a 'package' declaration.",
        ) {
            return Err(());
        }
        let token = self.advance();
        let loc = token.loc;
        let path = match token.t {
            TokenType::StringLit(ref s) => s.clone(),
            _ => {
                self.err.report(
                    loc,
                    String::from("Expected a string after 'package' declaration."),
                );
                return Err(());
            }
        };
        self.consume_semi_colon();
        Ok(ast::Package {
            path: path,
            loc: loc,
        })
    }

    /// Parse a `declaration`.
    /// Declaration consumes the first token of the declaration,
    /// this is done to centralize error handling.
    fn declaration(&mut self) -> Result<Declaration, ()> {
        // Expose declaration
        if self.next_match(TokenType::Expose) {
            return Ok(Declaration::Expose(self.expose()?));
        }
        // Fun declaration
        let is_pub = self.next_match(TokenType::Pub);
        if self.next_match(TokenType::Fun) {
            let mut fun = self.function()?;
            fun.is_pub = is_pub;
            return Ok(Declaration::Fun(fun));
        }
        // No declaration found
        let loc = self.peek().loc;
        self.err.report(
            loc,
            String::from("Expected a top level declaration: `expose` or `fun`"),
        );
        self.synchronize();
        Err(())
    }

    /// Parses the 'expose' grammar element
    /// The `Expose` token must have been consumed.
    fn expose(&mut self) -> Result<ast::Expose, ()> {
        let token = self.peek();
        let loc = token.loc;
        // First identifier
        if let TokenType::Identifier(ref ident) = token.t {
            let fun_name = ident.clone();
            self.advance();
            // Check for `as` keyword
            let alias = if self.next_match(TokenType::As) {
                let token = self.peek();
                if let TokenType::Identifier(ref as_ident) = token.t {
                    let as_ident = as_ident.clone();
                    self.advance();
                    Some(as_ident)
                } else {
                    let loc = token.loc;
                    self.err.report(
                        loc,
                        String::from("Expected an identifier after 'as' keyword."),
                    );
                    self.synchronize();
                    return Err(());
                }
            } else {
                None
            };
            self.consume_semi_colon();
            return Ok(ast::Expose {
                ident: fun_name,
                alias: alias,
                loc: loc,
            });
        }
        self.err.report(
            loc,
            String::from("Expect an identifier after 'expose' keyword."),
        );
        self.synchronize();
        Err(())
    }

    /// Parses the 'function' grammar element
    /// The `Pub` (if any) and `Fun` tokens must have been consumed.
    fn function(&mut self) -> Result<ast::Function, ()> {
        let loc = self.peek().loc;
        let token = self.advance();
        let ident = match token.t {
            TokenType::Identifier(ref x) => x.clone(),
            _ => {
                self.err.report(
                    loc,
                    String::from("Identifier expected after `fun` keyword."),
                );
                self.synchronize(); // TODO better error handling: find the next top-level declaration
                return Err(());
            }
        };
        if !self.next_match_report(
            TokenType::LeftPar,
            "Expected an opening parenthesis `(` after function name.",
        ) {
            self.synchronize();
            return Err(());
        }
        let params = self.parameters()?;
        if !self.next_match_report(
            TokenType::RightPar,
            "Expected a closing parenthesis `)` after function parameters.",
        ) {
            self.synchronize();
            return Err(());
        }
        let token = self.peek();
        let result = if let TokenType::Identifier(ref result) = token.t {
            let loc = token.loc;
            let result = result.clone();
            self.advance();
            Some((result, loc))
        } else {
            None
        };
        let stmts = self.block()?;
        self.consume_semi_colon();

        Ok(ast::Function {
            ident: ident,
            params: params,
            result: result,
            body: ast::Body::Asm(stmts),
            is_pub: false, // handled by the called who may have consumed the "pub" keyword
            loc: loc,      // location of the identifier
        })
    }

    /// Parses the 'parameters' grammar element
    fn parameters(&mut self) -> Result<Vec<ast::Variable>, ()> {
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
            let t = match token.t {
                TokenType::Identifier(ref x) => x.clone(),
                _ => {
                    self.err
                        .report(loc, String::from("Expected parameter type"));
                    self.back();
                    self.synchronize();
                    return Err(());
                }
            };

            params.push(ast::Variable {
                ident: ident,
                t: Some(t),
                loc: var_loc,
            });
            if !self.next_match(TokenType::Comma) {
                return Ok(params);
            }
        }
        self.back();
        Ok(params)
    }

    /// Parses the 'block' grammar element
    fn block(&mut self) -> Result<Vec<AsmStatement>, ()> {
        let mut stmts = Vec::new();

        // Left brace
        if !self.next_match_report(
            TokenType::LeftBrace,
            "A block must start with an opening brace `{`.",
        ) {
            self.synchronize();
            return Err(());
        }
        // Statements
        while let TokenType::Opcode(_) = self.peek().t {
            match self.statement() {
                Ok(stmt) => stmts.push(stmt),
                Err(_) => self.synchronize(),
            }
        }
        // Right brace
        if !self.next_match_report(
            TokenType::RightBrace,
            "A block must end with a closing brace `}`.",
        ) {
            self.synchronize();
            return Err(());
        }
        Ok(stmts)
    }

    /// Parses the 'statement' grammar element
    fn statement(&mut self) -> Result<AsmStatement, ()> {
        let token = self.peek();
        let mut loc = token.loc;
        if let TokenType::Opcode(opcode) = token.t {
            self.advance();
            let token = self.peek();
            let arg_loc = token.loc;
            let arg = match token.t {
                TokenType::NumberLit(n) => {
                    loc = loc.merge(arg_loc);
                    self.advance();
                    Some(Argument::Integer(n))
                }
                _ => None,
            };
            self.consume_semi_colon();
            match opcode_to_asm(opcode, arg) {
                Ok(stmt) => Ok(stmt),
                Err(err) => {
                    self.err.report(loc, err);
                    Err(())
                }
            }
        } else {
            self.err.report(loc, String::from("Expected a statement."));
            Err(())
        }
    }
}
