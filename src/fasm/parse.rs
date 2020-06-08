use super::fasm::*;
use super::opcode_to_mir::{opcode_to_mir, Argument};
use super::tokens::{Token, TokenType};
use crate::error::ErrorHandler;
use crate::mir;

enum Declaration {
    Expose(Exposed),
    Fun(Function),
}

/// Fork assembly parser, it produces MIR.
pub struct Parser<'a, 'b> {
    err: &'b mut ErrorHandler<'a>,
    tokens: Vec<Token>,
    current: usize, // current token index
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
        let mut exposed = Vec::new();

        let package = match self.package() {
            Ok(pkg) => pkg,
            Err(_) => String::from(""),
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

        Program {
            package: package,
            exposed: exposed,
            funs: funs,
        }
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

    fn back(&mut self) {
        if self.current > 0 {
            self.current -= 1;
        }
    }

    /// If the next token has type t, consume it and return true, return false otherwise
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

    fn synchronize(&mut self) {
        let mut token = self.advance();
        while token.t != TokenType::SemiColon && !self.is_at_end() {
            token = self.advance();
        }
    }

    fn package(&mut self) -> Result<String, ()> {
        if !self.next_match_report(
            TokenType::Package,
            "File must start with a 'package' declaration.",
        ) {
            return Err(());
        }
        let token = self.advance();
        let s = match token.t {
            TokenType::StringLit(ref s) => s.clone(),
            _ => {
                let loc = token.loc;
                self.err.report(
                    loc,
                    String::from("Expected a string after 'package' declaration."),
                );
                return Err(());
            }
        };
        self.consume_semi_colon();
        Ok(s)
    }

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

    /// The `Expose` token must have been consumed.
    fn expose(&mut self) -> Result<Exposed, ()> {
        let token = self.peek();
        // First identifier
        if let TokenType::Identifier(ref ident) = token.t {
            let fun_name = ident.clone();
            self.advance();
            // Check for `as` keyword
            let exposed_as = if self.next_match(TokenType::As) {
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
            return Ok(Exposed {
                fun_name: fun_name,
                exposed_as: exposed_as,
            });
        }
        self.synchronize();
        Err(())
    }

    /// The `Pub` (if any) and `Fun` tokens must have been consumed.
    fn function(&mut self) -> Result<Function, ()> {
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

        Ok(Function {
            ident: ident,
            params: params,
            result: result,
            stmts: stmts,
            is_pub: false, // handled by the called who may have consumed the "pub" keyword
            loc: loc,      // location of the identifier
        })
    }

    fn parameters(&mut self) -> Result<Vec<Variable>, ()> {
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

            params.push(Variable {
                ident: ident,
                t: t,
                loc: var_loc,
            });
            if !self.next_match(TokenType::Comma) {
                return Ok(params);
            }
        }
        self.back();
        Ok(params)
    }

    fn block(&mut self) -> Result<Vec<mir::Statement>, ()> {
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

    fn statement(&mut self) -> Result<mir::Statement, ()> {
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
            match opcode_to_mir(opcode, arg) {
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
