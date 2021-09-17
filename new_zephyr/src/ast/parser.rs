//! Parser

use std::cell::{Cell, RefCell};

use rowan::GreenNodeBuilder;

use super::diagnostics::ParseError;
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
            let err = match token.t {
                Standalone | Runtime | Module => self.build_module_decl(builder),
                Pub => {
                    builder.start_buffering();
                    self.add_token(token, builder);
                    self.tokens.advance();
                    self.consume_blanks(builder);
                    if let Some(token) = self.tokens.peek() {
                        match token.t {
                            Use => self.build_use_decl(builder),
                            Fun => self.build_fun(builder),
                            Struct => self.build_struct(builder),
                            Expose => self.build_expose_decl(builder),
                            _ => {
                                self.report_token(ParseError::Unknown);
                                Err(builder)
                            }
                        }
                    } else {
                        Ok(())
                    }
                }
                Use => self.build_use_decl(builder),
                Fun => self.build_fun(builder),
                Struct => self.build_struct(builder),
                Expose => self.build_expose_decl(builder),
                Import => todo!(),
                _ => {
                    self.report_token(ParseError::Unknown);
                    Err(builder)
                }
            };
            self.recover_decl(err).ok();
        }
    }

    fn build_module_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ModDecl);

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
        Ok(())
    }

    fn build_use_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(UseDecl);

        // "pub"
        if self.token_next_match(Pub, builder) {
            self.consume_blanks(builder);
        }

        // "use"
        if self.token_next_match(Use, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectUseDecl);
            return Err(builder);
        }

        // module path
        let err = self.build_module_path(builder);
        self.recover_decl(err)?;
        self.consume_blanks(builder);

        // "as"
        if self.token_next_match(As, builder) {
            self.consume_blanks(builder);
            if !self.token_next_match(Identifier, builder) {
                self.report_token(ParseError::ExpectIdent);
                return Err(builder);
            }
            self.consume_blanks(builder);
        }

        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_expose_decl<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ExposeDecl);

        if self.token_next_match(Expose, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::Unknown);
            return Err(builder);
        }

        if self.token_next_match(Identifier, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
        }

        if self.token_next_match(As, builder) {
            self.consume_blanks(builder);
            if self.token_next_match(Identifier, builder) {
                self.consume_blanks(builder);
            } else {
                self.report_token(ParseError::ExpectIdent);
            }
        }

        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_struct<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(StructDecl);

        // "pub"
        if self.token_next_match(Pub, builder) {
            self.consume_blanks(builder);
        }

        // "struct"
        if self.token_next_match(Struct, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectUseDecl);
            return Err(builder);
        }

        // Identifier
        if self.token_next_match(Identifier, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
        }

        // Block start
        if self.token_next_match(LeftBrace, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectLeftBrace);
        }

        // TODO
        while let Some(token) = self.tokens.peek() {
            let err = match token.t {
                Pub => {
                    builder.start_buffering();
                    self.add_token(token, builder);
                    self.tokens.advance();
                    self.consume_blanks(builder);
                    if let Some(token) = self.tokens.peek() {
                        match token.t {
                            Use => self.build_use_decl(builder),
                            Fun => self.build_fun(builder),
                            Struct => self.build_struct(builder),
                            Import => todo!(),
                            Expose => self.build_expose_decl(builder),
                            Identifier => self.build_struct_field(builder), // TODO: recover statement in this case
                            RightBrace => break,
                            _ => {
                                self.report_token(ParseError::Unknown);
                                Err(builder)
                            }
                        }
                    } else {
                        Err(builder)
                    }
                }
                Use => self.build_use_decl(builder),
                Fun => self.build_fun(builder),
                Struct => self.build_struct(builder),
                Import => todo!(),
                Expose => self.build_expose_decl(builder),
                Identifier => self.build_struct_field(builder), // TODO: recover statement in that case
                RightBrace => break,
                _ => {
                    self.report_token(ParseError::Unknown);
                    Err(builder)
                }
            };
            self.recover_decl(err).ok();
            self.consume_blanks(builder);
        }

        // Block end
        if self.token_next_match(RightBrace, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectRightBrace);
            return Err(builder);
        }
        self.consume_semicolon_decl(builder);

        Ok(())
    }

    fn build_struct_field<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(StructField);

        if self.token_next_match(Identifier, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
            return Err(builder);
        }

        if self.token_next_match(Colon, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectColon);
            return Err(builder);
        }

        self.build_type(builder)?;
        self.consume_semicolon_decl(builder);
        Ok(())
    }

    fn build_fun<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Fun);

        // "pub"
        if self.token_next_match(Pub, builder) {
            self.consume_blanks(builder);
        }

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

        // "fun"
        if self.token_next_match(Fun, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectFunKeyword);
            return Err(builder);
        }

        // identifier
        if self.token_next_match(Identifier, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
        }

        if self.token_next_match(LeftPar, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectLeftPar);
            return Err(builder);
        }

        let mut first_param = true;
        while !self.token_next_match(RightPar, builder) {
            if !first_param {
                if self.token_next_match(Comma, builder) {
                    self.consume_blanks(builder);
                } else {
                    self.report_token(ParseError::ExpectLeftPar);
                    return Err(builder);
                }
            }
            self.build_parameter(builder)?;
            self.consume_blanks(builder);
            first_param = false;
        }

        if self.token_next_match(Colon, builder) {
            self.consume_blanks(builder);
            self.build_type(builder)?;
        }

        Ok(())
    }

    fn build_parameter<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Parameter);

        if self.token_next_match(Identifier, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
            return Err(builder);
        }

        if self.token_next_match(Colon, builder) {
            self.consume_blanks(builder);
        } else {
            self.report_token(ParseError::ExpectIdent);
            return Err(builder);
        }

        self.build_type(builder)?;
        Ok(())
    }

    fn build_type<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Type);

        if let Some(Token { t: LeftPar, .. }) = self.tokens.peek() {
            // Tuple type
            let _node_ctx = builder.start_node(TupleType);
            self.consume_token(builder);
            self.consume_blanks(builder);
            self.build_type(builder)?;
            while self.token_next_match(Comma, builder) {
                self.consume_blanks(builder);
                if let Some(Token { t: RightPar, .. }) = self.tokens.peek() {
                    break;
                }
                self.build_type(builder)?;
            }
            self.consume_blanks(builder);
            if !self.token_next_match(RightPar, builder) {
                self.report_token(ParseError::MissingClosingPar);
                return Err(builder);
            }
        } else {
            // Path
            self.build_path(builder)?
        }

        Ok(())
    }

    fn build_path<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Path);

        // First identifier
        if !self.token_next_match(Identifier, builder) {
            self.report_token(ParseError::ExpectIdent);
            return Err(builder);
        }
        self.consume_blanks(builder);

        // Optionnal path separated by '.' tokens
        while self.token_next_match(Dot, builder) {
            if !self.token_next_match(Identifier, builder) {
                self.report_token(ParseError::ExpectPath); // TODO: could be more precise
                return Err(builder);
            }
            self.consume_blanks(builder);
        }

        Ok(())
    }

    fn build_module_path<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(ModPath);

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

    fn build_block<'a, 'b>(&mut self, builder: &'a Builder<'b>) -> ParserResult<'a, 'b> {
        let _node_ctx = builder.start_node(Block);

        if !self.token_next_match(LeftBrace, builder) {
            self.report_token(ParseError::ExpectLeftBrace);
            return Err(builder);
        }
        self.consume_blanks(builder);
        // TODO: parse statements
        if !self.token_next_match(RightBrace, builder) {
            self.report_token(ParseError::Unknown);
            return Err(builder);
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
                self.add_token(token, builder);
                self.tokens.advance();
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
