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
        let mut structs = Vec::new();
        let mut exposed = Vec::new();
        let mut imports = Vec::new();
        let mut used = Vec::new();

        let package = match self.package() {
            Ok(pkg) => pkg,
            Err(()) => {
                self.err.silent_report();
                // TODO: In the future we may want to parse the code anyway in order to provide feedback
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
                    Declaration::Struct(struc) => structs.push(struc),
                    Declaration::Use(uses) => used.push(uses),
                    Declaration::Expose(expose) => exposed.push(expose),
                    Declaration::Imports(import) => imports.push(import),
                },
                Err(()) => self.err.silent_report(),
            }
        }

        Program {
            package,
            funs,
            structs,
            exposed,
            imports,
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
    fn next_match_report(&mut self, t: TokenType, err: &str) -> Result<(), ()> {
        if self.peek().t == t {
            self.advance();
            Ok(())
        } else {
            let loc = self.peek().loc;
            self.err.report(loc, String::from(err));
            Err(())
        }
    }

    /// Same as `next_match` but report an error and synchronize to the next statement if the
    /// token doesn't match.
    fn next_match_report_synchronize(&mut self, t: TokenType, err: &str) -> Result<(), ()> {
        match self.next_match_report(t, err) {
            Ok(_) => Ok(()),
            Err(_) => {
                self.synchronize();
                return Err(());
            }
        }
    }

    /// Same as `next_match` but report an error and synchronize to the next declaration if the
    /// token doesn't match.
    fn next_match_report_synchronize_decl(&mut self, t: TokenType, err: &str) -> Result<(), ()> {
        match self.next_match_report(t, err) {
            Ok(_) => Ok(()),
            Err(_) => {
                self.synchronize_decl();
                return Err(());
            }
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
    fn synchronize_decl(&mut self) {
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

    /// Expects the current token to be an identifier and consumes it, throws and exception and
    /// synchronize to the next declaration if it's not.
    fn expect_identifier(&mut self, error_message: &str) -> Result<String, ()> {
        let token = self.advance();
        if let Token {
            t: TokenType::Identifier(ref ident),
            ..
        } = token
        {
            Ok(ident.clone())
        } else {
            let loc = token.loc;
            self.err.report(loc, String::from(error_message));
            self.synchronize_decl();
            Err(())
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
        self.next_match_report(
            TokenType::Package,
            "Programs must start with a package declaration",
        )?;
        let token = self.advance();
        if let TokenType::Identifier(ref ident) = token.t {
            let ident = ident.clone();
            let end = self.peek().loc;
            self.consume_semi_colon();
            Ok(Package {
                id: self.package_id,
                loc: start.merge(end),
                name: ident,
                t: package_type,
                kind: package_kind,
            })
        } else {
            let loc = token.loc;
            self.synchronize();
            self.err.report(
                loc,
                String::from("'package' keyword should be followed by the name of the package"),
            );
            Err(())
        }
    }

    /// Parses a 'declaration' that can be either a 'use', 'expose', 'import' or 'fun'
    fn declaration(&mut self) -> Result<Declaration, ()> {
        match self.peek().t {
            TokenType::Fun => Ok(Declaration::Function(self.function()?)),
            TokenType::Use => Ok(Declaration::Use(self._use()?)),
            TokenType::Expose => Ok(Declaration::Expose(self.expose()?)),
            TokenType::From => Ok(Declaration::Imports(self.imports()?)),
            TokenType::Struct => Ok(Declaration::Struct(self._struct()?)),
            TokenType::Pub => match self.peekpeek().t {
                TokenType::Fun => Ok(Declaration::Function(self.function()?)),
                TokenType::Struct => Ok(Declaration::Struct(self._struct()?)),
                _ => {
                    self.err.report(
                        self.peekpeek().loc,
                        String::from(
                            "Top level declaration must be one of 'function', 'use', 'expose' or 'from ... import'.",
                        ),
                    );
                    self.synchronize();
                    Err(())
                }
            },
            _ => {
                self.err.report(
                    self.peek().loc,
                    String::from(
                        "Top level declaration must be one of 'function', 'use', 'expose' or 'from ... import'.",
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
        self.next_match_report(TokenType::Use, "Use statement must start by 'use' keyword")?;
        let path = self.path()?;
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
            path: ModulePath {
                root: path.root,
                path: path.path,
            },
            alias,
        })
    }

    /// Parses the 'expose' grammar element
    fn expose(&mut self) -> Result<Expose, ()> {
        let start = self.peek().loc;
        self.next_match_report(
            TokenType::Expose,
            "Expose statement must start with 'expose' keyword",
        )?;
        let ident = self.expect_identifier("'expose' keyword must be followed by an identifier")?;
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
    }

    /// Parses the 'imports' grammar element
    fn imports(&mut self) -> Result<Imports, ()> {
        self.next_match_report_synchronize_decl(
            TokenType::From,
            "Expected 'from' to start an import declaration",
        )?;
        let loc = self.peek().loc;
        let from = self.expect_identifier("Expected a function identifier after 'import'")?;
        let loc = self.peek().loc.merge(loc);
        self.next_match_report_synchronize_decl(
            TokenType::Import,
            "Expected 'import' keyword after 'from' identifier",
        )?;
        let prototypes = self.import_block()?;
        self.consume_semi_colon();
        Ok(Imports {
            from,
            prototypes,
            loc,
        })
    }

    /// Parses the 'import_block' grammar element
    fn import_block(&mut self) -> Result<Vec<FunctionPrototype>, ()> {
        self.next_match_report_synchronize_decl(
            TokenType::LeftBrace,
            "Expected a left brace '{' to open import block",
        )?;
        let mut prototypes = Vec::new();
        while !self.next_match(TokenType::RightBrace) && !self.is_at_end() {
            let next_expr = self.import();
            match next_expr {
                Ok(import) => prototypes.push(import),
                Err(()) => (),
            }
        }
        Ok(prototypes)
    }

    /// Parses the 'import' grammar element
    fn import(&mut self) -> Result<FunctionPrototype, ()> {
        let is_pub = self.next_match(TokenType::Pub);
        self.next_match_report_synchronize_decl(TokenType::Fun, "Unexpected function prototype")?;
        let loc = self.peek().loc;
        let ident = if let Token {
            t: TokenType::Identifier(ref ident),
            ..
        } = self.advance()
        {
            ident.clone()
        } else {
            self.err.report(
                loc,
                String::from("Expected a function identifier after 'import'"),
            );
            self.synchronize_decl();
            return Err(());
        };
        self.next_match_report_synchronize_decl(
            TokenType::LeftPar,
            "Parenthesis are expected after import identifier",
        )?;
        let params = self.parameters();
        self.next_match_report_synchronize_decl(
            TokenType::RightPar,
            "Expected a right parenthesis ')'",
        )?;
        let result = self.result();
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
        Ok(FunctionPrototype {
            ident,
            params,
            result,
            alias,
            is_pub,
            loc: loc.merge(end),
        })
    }

    /// Parses the 'struct" grammar element
    fn _struct(&mut self) -> Result<Struct, ()> {
        let is_pub = self.next_match(TokenType::Pub);
        self.next_match_report_synchronize_decl(
            TokenType::Struct,
            "Unexpected top level declaration",
        )?;
        let loc = self.peek().loc;
        let ident = self.expect_identifier("Expected identifier after 'struct' keyword")?;
        self.warn_if_struct_not_capitalized(&ident, loc);
        let fields = self.struct_block()?;
        self.consume_semi_colon();
        Ok(Struct {
            ident,
            fields,
            is_pub,
            loc,
        })
    }

    fn struct_block(&mut self) -> Result<Vec<StructField>, ()> {
        self.next_match_report_synchronize_decl(
            TokenType::LeftBrace,
            "Expected a left brace '{' to open struct definition block",
        )?;
        let mut fields = Vec::new();
        while let Some(field) = self.struct_field() {
            fields.push(field);
            if !self.next_match(TokenType::Comma) && !self.next_match(TokenType::SemiColon) {
                break;
            }
        }
        self.next_match_report(
            TokenType::RightBrace,
            "Expect a right brace ('}') after a struct declaration",
        )?;
        Ok(fields)
    }

    fn struct_field(&mut self) -> Option<StructField> {
        let is_pub = self.next_match(TokenType::Pub);
        let loc = self.peek().loc;
        let ident = if let Token {
            t: TokenType::Identifier(ref ident),
            ..
        } = self.advance()
        {
            ident.clone()
        } else {
            // Restore initial state
            self.back();
            if is_pub {
                self.back();
            }
            return None;
        };
        self.next_match_report_synchronize(
            TokenType::Colon,
            "Expect a colon (`:`) after field identifier",
        )
        .ok()?;
        let t_loc = self.peek().loc;
        let t = self.type_().ok()?;
        let loc = loc.merge(t_loc);
        Some(StructField {
            is_pub,
            ident,
            t,
            loc,
        })
    }

    /// Parses the 'function' grammar element
    fn function(&mut self) -> Result<Function, ()> {
        let is_pub = self.next_match(TokenType::Pub);
        self.next_match_report_synchronize_decl(
            TokenType::Fun,
            "Unexpected top level declaration",
        )?;
        let loc = self.peek().loc;
        let ident = self.expect_identifier("Expected identifier after 'fun' keyword")?;
        self.next_match_report_synchronize_decl(
            TokenType::LeftPar,
            "Parenthesis are expected after function declaration",
        )?;
        let params = self.parameters();
        self.next_match_report_synchronize_decl(
            TokenType::RightPar,
            "Parenthesis are expected after function declaration",
        )?;
        let result = self.result();
        let error = if result.is_some() {
            "A left brace '{' is expected at the beginning of the function body."
        } else {
            "Expected a type (': MyType') or a brace ('{')."
        };
        self.next_match_report_synchronize_decl(TokenType::LeftBrace, error)?;
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
    fn parameters(&mut self) -> Vec<Parameter> {
        let mut params = Vec::new();
        while let Token {
            t: TokenType::Identifier(ref param),
            loc: var_loc,
        } = self.advance()
        {
            let ident = param.clone();
            let var_loc = *var_loc;
            self.next_match_report(
                TokenType::Colon,
                "Expected a type after parameter identifier",
            )
            .ok();

            let t = match self.type_() {
                Ok(t) => t,
                Err(()) => {
                    self.err.silent_report();
                    return params;
                }
            };

            params.push(Parameter {
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
    fn result(&mut self) -> Option<(Type, Location)> {
        if self.next_match(TokenType::Colon) {
            let loc = self.peek().loc;
            match self.type_() {
                Ok(t) => Some((t, loc.merge(self.peek().loc))),
                Err(()) => None,
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
            _ => self.expr_or_assign_stmt(),
        }
    }

    /// Parses the 'expr_stmt' or 'assign_stmt' grammar element, both are handled because thay
    /// share a common prefix.
    fn expr_or_assign_stmt(&mut self) -> Result<Statement, ()> {
        let expr = self.expression(true)?;
        let stmt = if self.next_match(TokenType::Equal) {
            let value = self.expression(true)?;
            Statement::AssignStmt {
                target: expr,
                expr: value,
            }
        } else {
            Statement::ExprStmt(expr)
        };
        self.consume_semi_colon();
        Ok(stmt)
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
        self.next_match_report_synchronize(
            TokenType::Equal,
            "Let statement requires an \"=\" after the identifier",
        )?;
        let expr = self.expression(true)?;
        self.consume_semi_colon();
        Ok(Statement::LetStmt {
            var: Variable {
                namespace: None,
                t: None,
                ident,
                loc,
            },
            expr,
        })
    }

    /// Parses the 'if_stmt' grammar element (assuming the `if` token has
    /// been consumed )
    fn if_stmt(&mut self) -> Result<Statement, ()> {
        // The `if` token must have been consumed
        let expr = self.expression(false)?;
        self.next_match_report(
            TokenType::LeftBrace,
            "If statement requires an \"{\" after the condition",
        )?;
        let block = self.block()?;

        // Check for else clause
        if self.peek().t == TokenType::Else {
            self.advance();
            self.next_match_report(
                TokenType::LeftBrace,
                "If statement requires an \"{\" after else clause",
            )?;
            let else_block = self.block()?;
            self.consume_semi_colon();
            Ok(Statement::IfStmt {
                expr,
                block,
                else_block: Some(else_block),
            })
        } else {
            self.consume_semi_colon();
            Ok(Statement::IfStmt {
                expr,
                block,
                else_block: None,
            })
        }
    }

    /// Parses the 'while_stmt' grammar element (assuming the `while` token has
    /// been consumed )
    fn while_stmt(&mut self) -> Result<Statement, ()> {
        // The `while` token must have been consumed
        let expr = self.expression(false)?;
        self.next_match_report(
            TokenType::LeftBrace,
            "While statement requires an \"{\" after the condition",
        )?;
        let block = self.block()?;
        self.consume_semi_colon();
        Ok(Statement::WhileStmt { expr, block })
    }

    /// Parses the 'return_stmt' grammar element (assuming the `return` token has
    /// been consumed )
    fn return_stmt(&mut self) -> Result<Statement, ()> {
        // The `return` token must have been consumed
        let loc = self.previous().loc;
        let expr = self.expression(true);
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

    /// Parses the 'block' grammar element (assuming the `{` token has been
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
    ///
    /// Struct literals may be disallowed inside some expressions to remove ambiguity (consider `if
    /// x == MyStruct {} {}`), in that case `struct_lit` should be set to `false`.
    fn expression(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        self.logical_or(struct_lit)
    }

    fn logical_or(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_and = self.logical_and(struct_lit)?;

        while self.next_match(TokenType::OrOr) {
            let right_and = self.logical_and(struct_lit)?;
            left_and = Expression::Binary {
                expr_left: Box::new(left_and),
                binop: BinaryOperator::Or,
                expr_right: Box::new(right_and),
            }
        }
        Ok(left_and)
    }

    fn logical_and(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_eq = self.equality(struct_lit)?;

        while self.next_match(TokenType::AndAnd) {
            let right_eq = self.logical_and(struct_lit)?;
            left_eq = Expression::Binary {
                expr_left: Box::new(left_eq),
                binop: BinaryOperator::And,
                expr_right: Box::new(right_eq),
            }
        }
        Ok(left_eq)
    }

    fn equality(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_comp = self.comparison(struct_lit)?;

        loop {
            let binop = match self.peek().t {
                TokenType::EqualEqual => BinaryOperator::Equal,
                TokenType::BangEqual => BinaryOperator::NotEqual,
                _ => break,
            };
            self.advance();
            let right_comp = self.comparison(struct_lit)?;
            left_comp = Expression::Binary {
                expr_left: Box::new(left_comp),
                binop,
                expr_right: Box::new(right_comp),
            }
        }
        Ok(left_comp)
    }

    fn comparison(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_b_or = self.bitwise_or(struct_lit)?;

        loop {
            let binop = match self.peek().t {
                TokenType::Less => BinaryOperator::Less,
                TokenType::LessEqual => BinaryOperator::LessEqual,
                TokenType::Greater => BinaryOperator::Greater,
                TokenType::GreaterEqual => BinaryOperator::GreaterEqual,
                _ => break,
            };
            self.advance();
            let right_b_or = self.bitwise_or(struct_lit)?;
            left_b_or = Expression::Binary {
                expr_left: Box::new(left_b_or),
                binop,
                expr_right: Box::new(right_b_or),
            }
        }
        Ok(left_b_or)
    }

    fn bitwise_or(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_b_and = self.bitwise_xor(struct_lit)?;

        while self.next_match(TokenType::Or) {
            let right_b_and = self.bitwise_xor(struct_lit)?;
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseOr,
                expr_right: Box::new(right_b_and),
            }
        }
        Ok(left_b_and)
    }

    fn bitwise_xor(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_b_and = self.bitwise_and(struct_lit)?;

        while self.next_match(TokenType::Hat) {
            let right_b_and = self.bitwise_and(struct_lit)?;
            left_b_and = Expression::Binary {
                expr_left: Box::new(left_b_and),
                binop: BinaryOperator::BitwiseXor,
                expr_right: Box::new(right_b_and),
            }
        }
        Ok(left_b_and)
    }

    fn bitwise_and(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_add = self.addition(struct_lit)?;

        while self.next_match(TokenType::And) {
            let right_add = self.addition(struct_lit)?;
            left_add = Expression::Binary {
                expr_left: Box::new(left_add),
                binop: BinaryOperator::BitwiseAnd,
                expr_right: Box::new(right_add),
            }
        }
        Ok(left_add)
    }

    fn addition(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_mult = self.multiplication(struct_lit)?;

        loop {
            let binop = match self.peek().t {
                TokenType::Plus => BinaryOperator::Plus,
                TokenType::Minus => BinaryOperator::Minus,
                _ => break,
            };
            self.advance();
            let right_mult = self.multiplication(struct_lit)?;
            left_mult = Expression::Binary {
                expr_left: Box::new(left_mult),
                binop,
                expr_right: Box::new(right_mult),
            }
        }
        Ok(left_mult)
    }

    fn multiplication(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut left_unary = self.unary(struct_lit)?;

        loop {
            let binop = match self.peek().t {
                TokenType::Star => BinaryOperator::Multiply,
                TokenType::Slash => BinaryOperator::Divide,
                TokenType::Percent => BinaryOperator::Remainder,
                _ => break,
            };
            self.advance();
            let right_unary = self.unary(struct_lit)?;
            left_unary = Expression::Binary {
                expr_left: Box::new(left_unary),
                binop,
                expr_right: Box::new(right_unary),
            }
        }
        Ok(left_unary)
    }

    fn unary(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let token = self.peek();

        match token.t {
            TokenType::Bang => {
                self.advance();
                Ok(Expression::Unary {
                    unop: UnaryOperator::Not,
                    expr: Box::new(self.unary(struct_lit)?),
                })
            }
            TokenType::Minus => {
                self.advance();
                Ok(Expression::Unary {
                    unop: UnaryOperator::Minus,
                    expr: Box::new(self.unary(struct_lit)?),
                })
            }
            _ => self.call(struct_lit),
        }
    }

    fn call(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut expr = self.access(struct_lit)?;
        while self.next_match(TokenType::LeftPar) {
            let args = self.arguments();
            self.next_match_report_synchronize(
                TokenType::RightPar,
                "Expected a closing parenthesis `)` to function call",
            )?;
            expr = Expression::Call {
                fun: Box::new(expr),
                args,
            };
        }
        Ok(expr)
    }

    fn access(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let mut namespace = self.primary(struct_lit)?;

        loop {
            if self.next_match(TokenType::Dot) {
                let field = self.primary(struct_lit)?;
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

    fn primary(&mut self, struct_lit: bool) -> Result<Expression, ()> {
        let token = self.advance();
        let loc = token.loc;

        match &token.t {
            TokenType::IntegerLit(n) => Ok(Expression::Literal(Value::Integer { val: *n, loc })),
            TokenType::FloatLit(x) => Ok(Expression::Literal(Value::Float { val: *x, loc })),
            TokenType::BooleanLit(b) => Ok(Expression::Literal(Value::Boolean { val: *b, loc })),
            TokenType::StringLit(ref s) => Ok(Expression::Literal(Value::Str {
                val: s.clone(),
                loc,
            })),
            TokenType::Identifier(ref x) => {
                let ident = x.clone();
                let loc = token.loc;
                if struct_lit && self.next_match(TokenType::LeftBrace) {
                    // Struct instantiation
                    let fields = self.struct_literal()?;
                    let loc = loc.merge(self.peek().loc);
                    self.next_match_report_synchronize(
                        TokenType::RightBrace,
                        "Expect closing brace '}' after struct instantiation",
                    )?;
                    Ok(Expression::Literal(Value::Struct {
                        namespace: None,
                        ident,
                        fields,
                        loc,
                    }))
                } else {
                    // Variable
                    Ok(Expression::Variable(Variable {
                        namespace: None,
                        t: None,
                        ident,
                        loc,
                    }))
                }
            }
            TokenType::LeftPar => {
                let expr = self.expression(true)?;
                self.next_match_report(TokenType::RightPar, "Expected closing parenthesis")?;
                Ok(expr)
            }
            _ => Err(()),
        }
    }

    fn arguments(&mut self) -> Vec<Expression> {
        let mut args = Vec::new();
        while let Ok(expr) = self.expression(true) {
            args.push(expr);
            if !self.next_match(TokenType::Comma) {
                return args;
            }
        }
        self.back(); // expression consume one token when failing
        args
    }

    /// Expects that `IDENTIFIER` and `{` have been consumed, does not consume final `}`.
    fn struct_literal(&mut self) -> Result<Vec<FieldValue>, ()> {
        let mut fields = Vec::new();
        loop {
            let field = match self.field() {
                Ok(Some(field)) => field,
                Ok(None) => break,
                Err(()) => {
                    self.err.silent_report();
                    continue;
                }
            };
            fields.push(field);
            if !self.next_match(TokenType::Comma) && !self.next_match(TokenType::SemiColon) {
                break;
            }
        }
        Ok(fields)
    }

    fn field(&mut self) -> Result<Option<FieldValue>, ()> {
        let token = self.advance();
        let loc = token.loc;
        let ident = if let TokenType::Identifier(ident) = &token.t {
            ident.clone()
        } else {
            // Restore initial state
            self.back();
            return Ok(None);
        };
        let expr = if self.next_match(TokenType::Colon) {
            match self.expression(true) {
                Ok(expr) => expr,
                Err(()) => {
                    self.err.report(
                        loc,
                        String::from("Expected an expression after field's ':'"),
                    );
                    return Err(());
                }
            }
        } else {
            Expression::Variable(Variable {
                ident: ident.clone(),
                namespace: None,
                t: None,
                loc,
            })
        };

        return Ok(Some(FieldValue { ident, expr, loc }));
    }

    fn path(&mut self) -> Result<Path, ()> {
        let token = self.advance();
        let (ident, loc) = if let Token {
            t: TokenType::Identifier(ident),
            loc,
        } = token
        {
            (ident.clone(), *loc)
        } else {
            let loc = token.loc;
            self.err.report(loc, String::from("Expected an identifier"));
            self.synchronize();
            return Err(());
        };
        let mut path = Path {
            root: ident,
            path: Vec::new(),
            loc,
        };
        while self.next_match(TokenType::Dot) {
            let token = self.advance();
            let (ident, loc) = if let Token {
                t: TokenType::Identifier(ident),
                loc,
            } = token
            {
                (ident.clone(), *loc)
            } else {
                let loc = token.loc;
                self.err.report(loc, String::from("Expected an identifier"));
                self.synchronize();
                return Err(());
            };
            path.path.push(ident);
            path.loc = path.loc.merge(loc);
        }
        Ok(path)
    }

    fn type_(&mut self) -> Result<Type, ()> {
        if self.next_match(TokenType::LeftPar) {
            // Tuple type
            let mut paths = vec![self.type_()?];
            while self.next_match(TokenType::Comma) {
                paths.push(self.type_()?);
            }
            // Consumes optionnal comma
            self.next_match(TokenType::Comma);
            // Consumes the closing parenthesis
            self.next_match_report_synchronize(
                TokenType::RightPar,
                "Expected a right parenthesis ')'",
            )?;
            Ok(Type::Tuple(paths))
        } else {
            // Simple type
            Ok(Type::Simple(self.path()?))
        }
    }

    // ——————————————————————————— Helper Functions ———————————————————————————— //

    fn warn_if_struct_not_capitalized(&mut self, ident: &str, loc: Location) {
        if let Some(c) = ident.chars().next() {
            if !c.is_uppercase() {
                self.err.warn(
                    loc,
                    String::from("Struct identifier should start with a capital letter."),
                );
            }
        }
    }
}
