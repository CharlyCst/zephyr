//! Parser

use std::cell::{Cell, RefCell};

use rowan::Checkpoint;
use rowan::GreenNodeBuilder;

use super::diagnostics::ParseError as Error;
use super::syntax::SyntaxKind;
use super::syntax::SyntaxKind::*;
use super::syntax::SyntaxTree;
use super::tokens::{Token, TokenStream};
use crate::diagnostics::Diagnostics;

/// A node builder, used to build the syntax tree.
pub struct Builder<'cache> {
    inner: RefCell<GreenNodeBuilder<'cache>>,
    is_buffering: Cell<bool>,
    buffer: RefCell<Vec<(SyntaxKind, String)>>,
}

/// A note context. Creating a context starts a new node and while the context is alive all the new
/// nodes and tokens are added as children to that node.
#[must_use]
pub struct NodeContext<'builder, 'cache> {
    builder: &'builder Builder<'cache>,
}

type ParserResult<'a, 'b> = Result<(), &'a Builder<'b>>;

struct Parser<'tokens> {
    tokens: TokenStream<'tokens>,
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

            while !self.tokens.is_at_end() {
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
        let token = self.tokens.peek();
        let err = match token.t {
            Standalone | Runtime | Module => self.build_module_decl(builder),
            Pub => {
                builder.start_buffering();
                self.add_token(token, builder);
                self.tokens.advance();
                self.consume_blanks(builder);
                let token = self.tokens.peek();
                match token.t {
                    Use => self.build_use_decl(builder),
                    Fun => self.build_fun_decl(builder),
                    Struct => self.build_struct_decl(builder),
                    Expose => self.build_expose_decl(builder),
                    From => self.build_import_decl(builder),
                    _ => {
                        self.report_token(Error::Unknown);
                        Err(builder)
                    }
                }
            }
            Use => self.build_use_decl(builder),
            Fun => self.build_fun_decl(builder),
            Struct => self.build_struct_decl(builder),
            Expose => self.build_expose_decl(builder),
            From => self.build_import_decl(builder),
            _ => {
                self.report_token(Error::Unknown);
                Err(builder)
            }
        };
        self.recover_decl(err).ok();
    }

    fn build_module_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ModDecl);

        self.next_match(Standalone, builder);
        self.next_match(Runtime, builder);
        if self.next_match(Module, builder) {
            if !self.next_match(Identifier, builder) {
                self.report_token(Error::ModDeclMissIdent);
            }
        } else {
            self.report_token(Error::BadModDecl);
        }

        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_use_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(UseDecl);

        self.next_match(Pub, builder);
        self.next_match_or_repport(Use, Error::ExpectUseDecl, builder)?;

        // module path
        let err = self.build_module_path(builder);
        self.recover_decl(err)?;
        self.consume_blanks(builder);

        self.build_optionnal_as(builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_import_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ImportDecl);

        self.next_match(Pub, builder);
        self.next_match_or_repport(From, Error::Unknown, builder)?;
        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)
            .ok();
        self.next_match_or_repport(Import, Error::Internal, builder)
            .ok();
        self.build_import_block(builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_import_block<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ImportBlock);

        self.next_match_or_repport(LeftBrace, Error::ExpectLeftBrace, builder)
            .ok();
        loop {
            let token = self.tokens.peek();
            let err = match token.t {
                Pub | Fun => self.build_import_item(builder),
                RightBrace => break,
                _ => {
                    self.report_token(Error::ExpectRightBrace);
                    Err(builder)
                }
            };
            self.recover_decl(err)?;
        }
        self.next_match_or_repport(RightBrace, Error::ExpectRightBrace, builder)?;
        Ok(())
    }

    fn build_import_item<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ImportItem);

        self.next_match(Pub, builder);
        self.build_prototype(builder)?;
        self.consume_semicolon_decl(builder);
        self.build_optionnal_as(builder)
    }

    fn build_expose_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ExposeDecl);

        self.next_match_or_repport(Expose, Error::Unknown, builder)?;
        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)
            .ok();
        self.build_optionnal_as(builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_struct_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(StructDecl);

        self.next_match(Pub, builder);
        self.next_match_or_repport(Struct, Error::ExpectUseDecl, builder)?;
        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)
            .ok();

        // Block start
        self.next_match_or_repport(LeftBrace, Error::ExpectLeftBrace, builder)
            .ok();

        loop {
            let token = self.tokens.peek();
            let err = match token.t {
                Pub => {
                    builder.start_buffering();
                    self.add_token(token, builder);
                    self.tokens.advance();
                    self.consume_blanks(builder);
                    let token = self.tokens.peek();
                    match token.t {
                        Use => self.build_use_decl(builder),
                        Fun => self.build_fun_decl(builder),
                        From => self.build_import_decl(builder),
                        Expose => self.build_expose_decl(builder), // TODO: public expose are not allowed
                        Struct => self.build_struct_decl(builder),
                        Identifier => self.build_struct_field(builder), // TODO: recover statement in this case
                        RightBrace => break,
                        _ => {
                            self.report_token(Error::Unknown);
                            Err(builder)
                        }
                    }
                }
                Use => self.build_use_decl(builder),
                Fun => self.build_fun_decl(builder),
                From => self.build_import_decl(builder),
                Struct => self.build_struct_decl(builder),
                Expose => self.build_expose_decl(builder),
                Identifier => self.build_struct_field(builder), // TODO: recover statement in that case
                RightBrace => break,
                _ => {
                    self.report_token(Error::Unknown);
                    Err(builder)
                }
            };
            self.recover_decl(err).ok();
            self.consume_blanks(builder);
        }

        // Block end
        self.next_match_or_repport(RightBrace, Error::ExpectRightBrace, builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_struct_field<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(StructField);

        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)?;
        self.next_match_or_repport(Colon, Error::ExpectColon, builder)?;
        self.build_type(builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_fun_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Fun);

        self.next_match(Pub, builder);

        // prototype
        let err = self.build_prototype(builder);
        self.recover_decl(err)?;
        self.consume_blanks(builder);

        // block
        let err = self.build_block(builder);
        self.recover_decl(err)?;
        self.consume_blanks(builder);

        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_prototype<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Prototype);

        self.next_match_or_repport(Fun, Error::ExpectFunKeyword, builder)?;
        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)
            .ok();
        self.build_parameter_list(builder)?;

        if self.next_match(Colon, builder) {
            self.build_type(builder)?;
        }
        Ok(())
    }

    fn build_parameter_list<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ParameterList);
        self.next_match_or_repport(LeftPar, Error::ExpectLeftPar, builder)?;
        let mut first_param = true;
        while !self.next_match(RightPar, builder) {
            if !first_param {
                self.next_match_or_repport(Comma, Error::ExpectLeftPar, builder)?;
            }
            self.build_parameter(builder)?;
            self.consume_blanks(builder);
            first_param = false;
        }
        Ok(())
    }

    fn build_parameter<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Parameter);

        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)?;
        self.next_match_or_repport(Colon, Error::ExpectIdent, builder)?;
        self.build_type(builder)?;
        Ok(())
    }

    fn build_type<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Type);

        if self.tokens.peek().t == LeftPar {
            // Tuple type
            let _node_ctx = builder.start_node(TupleType);
            self.consume_token(builder);
            self.consume_blanks(builder);
            self.build_type(builder)?;
            while self.next_match(Comma, builder) {
                self.consume_blanks(builder);
                if self.tokens.peek().t == RightPar {
                    break;
                }
                self.build_type(builder)?;
            }
            self.consume_blanks(builder);
            self.next_match_or_repport(RightPar, Error::MissingClosingPar, builder)?;
        } else {
            // Path
            self.build_path(builder)?
        }

        Ok(())
    }

    fn build_path<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Path);

        // First identifier
        self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)?;

        // Optionnal path separated by '.' tokens
        while self.next_match(Dot, builder) {
            // TODO: error can be more precise
            self.next_match_or_repport(Identifier, Error::ExpectPath, builder)?;
        }

        Ok(())
    }

    fn build_module_path<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ModPath);

        // First identifier
        self.next_match_or_repport(Identifier, Error::ExpectModPath, builder)?;

        // Optionnal path separated by '::' tokens
        while self.next_match(ColonColon, builder) {
            // TODO: error could be more precise
            self.next_match_or_repport(Identifier, Error::ExpectModPath, builder)?;
        }

        Ok(())
    }

    /// Try to build an "as" node if and only if the next token is the "as" keyword. Do nothing
    /// otherwise.
    fn build_optionnal_as<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        if self.tokens.peek().t == As {
            let _node_ctx = builder.start_node(AsIdent);
            self.next_match_or_repport(As, Error::Internal, builder)?;
            self.next_match_or_repport(Identifier, Error::ExpectIdent, builder)
        } else {
            Ok(())
        }
    }

    fn build_block<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Block);

        self.next_match_or_repport(LeftBrace, Error::ExpectLeftBrace, builder)?;

        // TODO: parse statements
        if !self.next_match(RightBrace, builder) {
            self.report_token(Error::Unknown);
            return Err(builder);
        }
        Ok(())
    }

    fn build_expression<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let checkpoint = builder.checkpoint();

        Ok(())
    }

    fn build_primary<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let token = self.tokens.peek();
        match token.t {
            NumberLit | StringLit | True | False => {
                self.consume_token(builder);
                self.consume_blanks(builder);
            }
            Identifier => {
                builder.start_buffering();
                self.consume_token(builder);
                self.consume_blanks(builder);
                // if let Some(token) = self.tokens.peek() && token.t == LeftBrace {
                //     todo!(); // TODO: struct literal
                // } else {
                //     builder.start_node(Identifier);
                // }
            }
            LeftPar => {
                // TODO: can be either a penrenthesed expression or a tuple
            }
            _ => {
                // TODO: error
            }
        }

        todo!();
    }

    // ————————————————————————————————— Utils —————————————————————————————————— //

    /// Consumes the next token and add it to the current node.
    fn consume_token(&mut self, builder: &Builder) {
        let token = self.tokens.advance();
        self.add_token(token, builder);
    }

    /// Consumes whitespaces, new lines and comments by adding them to the current node.
    fn consume_blanks(&mut self, builder: &Builder) {
        loop {
            let token = self.tokens.peek();
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
        if !self.next_match(SemiColon, builder) {
            self.report_token(Error::MissingSemicolon);
            self.synchronize_declaration(builder);
        }
    }

    /// Consumes the next token and following blanks by adding them to the current node and return
    /// true if it matches the given kind. Return false otherwise.
    fn next_match(&mut self, kind: SyntaxKind, builder: &Builder) -> bool {
        if self.tokens.peek().t == kind {
            let token = self.tokens.advance();
            self.add_token(token, builder);
            self.consume_blanks(builder);
            true
        } else {
            false
        }
    }

    /// Consumes the next token and following blanks by adding them to the current node if it
    /// matches the given kind, raise an error otherwise.
    fn next_match_or_repport<'a, 'b>(
        &mut self,
        kind: SyntaxKind,
        error: Error,
        builder: &'a Builder<'b>,
    ) -> ParserResult<'a, 'b> {
        if self.next_match(kind, builder) {
            Ok(())
        } else {
            self.report_token(error);
            Err(builder)
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
        loop {
            match self.tokens.peek().t {
                Standalone | Runtime | Module | Pub | Use | Expose | Fun | Struct | Import
                | From | EOF => break,
                _ => self.consume_token(builder),
            }
        }
    }

    /// Report an error for the current token.
    fn report_token(&mut self, error: Error) {
        let token = self.tokens.peek();
        if token.t != EOF {
            self.err.report(error, token.loc());
        } else {
            self.err.report_no_loc(error);
        }
    }

    /// Synchronizes up to the next declaration in case of error.
    fn recover_decl<'a, 'b>(&mut self, result: ParserResult<'a, 'b>) -> ParserResult<'a, 'b> {
        match result {
            Ok(()) => Ok(()),
            Err(builder) => {
                self.synchronize_declaration(builder);
                Err(builder)
            }
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
            is_buffering: Cell::new(false),
            buffer: RefCell::new(Vec::new()),
        }
    }

    pub fn start_node<'a>(&'a self, kind: SyntaxKind) -> NodeContext<'a, 'cache> {
        let mut inner = self.inner.borrow_mut();
        inner.start_node(kind.into());
        if self.is_buffering.get() {
            self.is_buffering.set(false);
            for (kind, text) in self.buffer.borrow_mut().drain(..) {
                inner.token(kind.into(), &text);
            }
        }
        NodeContext { builder: &self }
    }

    pub fn start_node_at<'a>(
        &'a self,
        checkpoint: Checkpoint,
        kind: SyntaxKind,
    ) -> NodeContext<'a, 'cache> {
        let mut inner = self.inner.borrow_mut();
        inner.start_node_at(checkpoint, kind.into());
        if self.is_buffering.get() {
            self.is_buffering.set(false);
            for (kind, text) in self.buffer.borrow_mut().drain(..) {
                inner.token(kind.into(), &text);
            }
        }
        NodeContext { builder: &self }
    }

    pub fn checkpoint(&self) -> Checkpoint {
        self.inner.borrow().checkpoint()
    }

    fn finish_node(&self) {
        self.inner.borrow_mut().finish_node();
    }

    pub fn finish(self) -> rowan::GreenNode {
        self.inner.into_inner().finish()
    }

    pub fn token(&self, kind: SyntaxKind, text: &str) {
        if kind != SemiColon {
            if self.is_buffering.get() {
                // TODO: we can do better than cloning the string here...
                self.buffer.borrow_mut().push((kind, text.to_owned()));
            } else {
                self.inner.borrow_mut().token(kind.into(), text);
            }
        }
    }

    /// Enter buffering mode, all tokens added are buffered until a new node is started, at which
    /// points the buffered tokens are moved into that node.
    pub fn start_buffering(&self) {
        self.is_buffering.set(true)
    }
}

impl<'builder, 'cache> Drop for NodeContext<'builder, 'cache> {
    fn drop(&mut self) {
        self.builder.finish_node()
    }
}
