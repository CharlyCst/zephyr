//! Parser

use std::cell::RefCell;

use rowan::GreenNodeBuilder;

use super::diagnostics::ParseError;
use super::syntax::SyntaxKind;
use super::syntax::SyntaxKind::*;
use super::syntax::SyntaxTree;
use super::tokens::{Token, TokenStream};
use crate::diagnostics::Diagnostics;

pub struct Builder<'cache> {
    inner: RefCell<GreenNodeBuilder<'cache>>,
}

pub struct NodeContext<'builder, 'cache> {
    builder: &'builder Builder<'cache>,
}

type ParserResult<'a, 'b> = Result<(), &'a Builder<'b>>;

struct Parser<'tokens> {
    tokens: TokenStream<'tokens>,
    // builder: GreenNodeBuilder<'cache>,
    err: Diagnostics,
}

impl<'tokens> Parser<'tokens> {
    fn new(tokens: TokenStream<'tokens>) -> Self {
        Self {
            tokens,
            err: Diagnostics::default(),
        }
    }

    fn parse(mut self, builder: Builder) -> (SyntaxTree, Diagnostics) {
        {
            let _node_ctx = builder.start_node(Root);

            while let Some(_) = self.tokens.peek() {
                self.parse_decl(&builder);
            }
        }

        // self.builder.finish_node();
        let tree = SyntaxTree::new(builder.finish());
        (tree, self.err)
    }

    /// Tries to parse a declaration
    fn parse_decl(&mut self, builder: &Builder) {
        self.consume_blanks(builder);
        if let Some(token) = self.tokens.peek() {
            match token.t {
                Standalone | Runtime | Module => self.build_module_decl(builder),
                Pub => {
                    if let Some(token) = self.tokens.peekpeek() {
                        match token.t {
                            Use => self.build_use_decl(builder),
                            _ => self.synchronize_declaration(builder),
                        }
                    }
                }
                Use => self.build_use_decl(builder),
                Expose => todo!(),
                Fun => todo!(),
                Struct => todo!(),
                Import => todo!(),
                _ => self.synchronize_declaration(builder),
            }
        }
    }

    fn build_module_decl(&mut self, builder: &Builder) {
        let _node_ctx = builder.start_node(ModDecl.into());

        if self.token_next_match(Standalone, builder) {
            self.consume_blanks(builder);
        }
        if self.token_next_match(Runtime, builder) {
            self.consume_blanks(builder);
        }
        if self.token_next_match(Module, builder) {
            self.consume_blanks(builder);
            if !self.token_next_match(Identifier, builder) {
                self.report_token(ParseError::ModDeclMissIdent);
            }
        } else {
            self.report_token(ParseError::BadModDecl);
        }

        self.consume_blanks(builder);
        self.consume_semicolon_decl(builder);
    }

    fn build_use_decl(&mut self, builder: &Builder) {
        let _node_ctx = builder.start_node(UseDecl.into());

        // "use"
        if self.token_next_match(Use, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectedUseDecl);
            self.synchronize_declaration(builder);
            return;
        }

        // "pub"
        if self.token_next_match(Pub, builder) {
            self.consume_blanks(builder);
        }

        // module path
        let err = self.build_module_path(builder);
        self.recover_decl(err);
        self.consume_blanks(builder);

        // TODO: handle "as" part of use decl
        self.consume_semicolon_decl(builder);
    }

    fn build_module_path<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ModPath.into());

        // First identifier
        if !self.token_next_match(Identifier, builder) {
            self.report_token(ParseError::ExpectModPath);
            return Err(builder);
        }
        self.consume_blanks(builder);

        // Optionnal path separated by '::' tokens
        while self.token_next_match(ColonColon, builder) {
            if !self.token_next_match(Identifier, builder) {
                self.report_token(ParseError::ExpectModPath); // TODO: could be more precise
                return Err(builder);
            }
            self.consume_blanks(builder);
        }

        Ok(())
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Consumes the next token and add it to the current node.
    fn consume_token(&mut self, builder: &Builder) {
        if let Some(token) = self.tokens.advance() {
            self.add_token(token, builder);
        }
    }

    /// Consumes whitespaces, new lines and comments by adding them to the current node.
    fn consume_blanks(&mut self, builder: &Builder) {
        while let Some(token) = self.tokens.peek() {
            match token.t {
                Whitespace | NewLine | CommentString => {
                    self.consume_token(builder);
                }
                _ => return,
            }
        }
    }

    /// Consumes a semi-colon, raise an error and synchronize up to the next declaration if none is
    /// found.
    fn consume_semicolon_decl(&mut self, builder: &Builder) {
        if !self.token_next_match(SemiColon, builder) {
            self.report_token(ParseError::MissingSemicolon);
            self.synchronize_declaration(builder);
        }
    }

    /// Consumes the next token by adding it to the current node and return true if it matches the
    /// given kind. Return false otherwise.
    fn token_next_match(&mut self, kind: SyntaxKind, builder: &Builder) -> bool {
        if let Some(token) = self.tokens.peek() {
            if token.t == kind {
                let token = self.tokens.advance().unwrap();
                self.add_token(token, builder);
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    /// Adds a token to the current node.
    fn add_token(&mut self, token: Token, builder: &Builder) {
        let text = self.tokens.text(token.start, token.end);
        builder.token(token.t.into(), text);
    }

    // ————————————————————————————— Error Handling ————————————————————————————— //

    /// Consumes tokens until the next declaration is found.
    fn synchronize_declaration(&mut self, builder: &Builder) {
        while let Some(token) = self.tokens.peek() {
            match token.t {
                Standalone | Runtime | Module | Pub | Use | Expose | Fun | Struct | Import => break,
                _ => self.consume_token(builder),
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

    /// Synchronizes up to the next declaration in case of error.
    fn recover_decl(&mut self, result: ParserResult) {
        match result {
            Ok(()) => (),
            Err(builder) => self.synchronize_declaration(builder),
        }
    }
}

pub fn parse(tokens: TokenStream) -> (SyntaxTree, Diagnostics) {
    let builder = Builder::new();
    Parser::new(tokens).parse(builder)
}

// ———————————————————————————————— Builder ————————————————————————————————— //

impl<'cache> Builder<'cache> {
    pub fn new() -> Self {
        Self {
            inner: RefCell::new(GreenNodeBuilder::new()),
        }
    }

    pub fn start_node<'a>(&'a self, kind: SyntaxKind) -> NodeContext<'a, 'cache> {
        self.inner.borrow_mut().start_node(kind.into());
        NodeContext { builder: &self }
    }

    fn finish_node(&self) {
        self.inner.borrow_mut().finish_node();
    }

    pub fn finish(self) -> rowan::GreenNode {
        self.inner.into_inner().finish()
    }

    pub fn token(&self, kind: SyntaxKind, text: &str) {
        self.inner.borrow_mut().token(kind.into(), text);
    }
}

impl<'builder, 'cache> Drop for NodeContext<'builder, 'cache> {
    fn drop(&mut self) {
        self.builder.finish_node()
    }
}
