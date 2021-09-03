//! Parser

use rowan::GreenNodeBuilder;

use super::diagnostics::ParseError;
use super::syntax::SyntaxKind;
use super::syntax::SyntaxKind::*;
use super::syntax::SyntaxTree;
use super::tokens::{Token, TokenStream};
use crate::diagnostics::Diagnostics;

struct Parser<'tokens, 'cache> {
    tokens: TokenStream<'tokens>,
    builder: GreenNodeBuilder<'cache>,
    err: Diagnostics,
}

impl<'tokens, 'cache> Parser<'tokens, 'cache> {
    fn new(tokens: TokenStream<'tokens>) -> Self {
        Self {
            tokens,
            builder: GreenNodeBuilder::new(),
            err: Diagnostics::default(),
        }
    }

    fn parse(mut self) -> (SyntaxTree, Diagnostics) {
        self.builder.start_node(Root.into());

        while let Some(_) = self.tokens.peek() {
            self.parse_decl();
        }

        self.builder.finish_node();
        let tree = SyntaxTree::new(self.builder.finish());
        (tree, self.err)
    }

    /// Tries to parse a declaration
    fn parse_decl(&mut self) {
        self.consume_blanks();
        if let Some(token) = self.tokens.peek() {
            match token.t {
                Standalone | Runtime | Module => self.build_module_decl(),
                Pub => {
                    if let Some(token) = self.tokens.peekpeek() {
                        match token.t {
                            _ => self.synchronize_declaration(),
                        }
                    }
                }
                Use => todo!(),
                Expose => todo!(),
                Fun => todo!(),
                Struct => todo!(),
                Import => todo!(),
                _ => self.synchronize_declaration(),
            }
        }
    }

    fn build_module_decl(&mut self) {
        self.builder.start_node(ModDecl.into());

        if self.token_next_match(Standalone) {
            self.consume_blanks();
        }
        if self.token_next_match(Runtime) {
            self.consume_blanks();
        }
        if self.token_next_match(Module) {
            self.consume_blanks();
            if !self.token_next_match(Identifier) {
                self.report_token(ParseError::ModDeclMissIdent);
            }
        } else {
            self.report_token(ParseError::BadModDecl);
        }

        self.consume_blanks();
        self.consume_semicolon_decl();
        self.builder.finish_node();
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Consumes the next token and add it to the current node.
    fn consume_token(&mut self) {
        if let Some(token) = self.tokens.advance() {
            self.add_token(token);
        }
    }

    /// Consumes whitespaces, new lines and comments by adding them to the current node.
    fn consume_blanks(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match token.t {
                Whitespace | NewLine | CommentString => {
                    self.consume_token();
                }
                _ => return,
            }
        }
    }

    /// Consumes a semi-colon, raise an error and synchronize up to the next declaration if none is
    /// found.
    fn consume_semicolon_decl(&mut self) {
        if !self.token_next_match(SemiColon) {
            self.report_token(ParseError::MissingSemicolon);
            self.synchronize_declaration();
        }
    }

    /// Consumes the next token by adding it to the current node and return true if it matches the
    /// given kind. Return false otherwise.
    fn token_next_match(&mut self, kind: SyntaxKind) -> bool {
        if let Some(token) = self.tokens.peek() {
            if token.t == kind {
                let token = self.tokens.advance().unwrap();
                self.add_token(token);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Adds a token to the current node.
    fn add_token(&mut self, token: Token) {
        let text = self.tokens.text(token.start, token.end);
        self.builder.token(token.t.into(), text);
    }

    // ————————————————————————————— Error Handling ————————————————————————————— //

    /// Consumes tokens until the next declaration is found.
    fn synchronize_declaration(&mut self) {
        while let Some(token) = self.tokens.peek() {
            match token.t {
                Standalone | Runtime | Module | Pub | Use | Expose | Fun | Struct | Import => break,
                _ => self.consume_token(),
            }
        }
    }

    /// Report an error for the current token.
    fn report_token(&mut self, error: ParseError) {
        if let Some(token) = self.tokens.peek() {
            self.err.report(error, token.loc());
        } else {
            self.err.report_no_loc(error);
        }
    }
}

pub fn parse(tokens: TokenStream) -> (SyntaxTree, Diagnostics) {
    Parser::new(tokens).parse()
}
