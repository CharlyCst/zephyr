//! # Syntax

#![allow(dead_code)]

use std::fmt;

use rowan;
use rowan::SyntaxNode;

/// Describes all the tokens and nodes that can appear in a syntax tree.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u16)]
pub enum SyntaxKind {
    // Nodes

    // Declarations
    ModDecl,
    FunDecl,
    UseDecl,
    ExposeDecl,
    ImportDecl,
    StructDecl,

    // Declaration items
    Path,
    ModPath,
    Prototype,
    Parameter,
    Type,
    TupleType,
    Block,

    // Statements
    ExprStmt,
    LetStmt,
    AssignStmt,
    IfStmt,
    WhileStmt,
    ReturnStmt,

    // Expressions
    Call,
    Access,
    BinaryOperator,
    UnaryOperator,

    // Tokens

    // Single character
    LeftPar,
    RightPar,
    LeftBrace,
    RightBrace,
    Comma,
    Colon,
    Dot,
    Minus,
    Plus,
    Slash,
    Star,
    Percent,
    Bang,
    Equal,
    Greater,
    Less,
    And,
    Or,
    Hat,

    // Two characters
    BangEqual,
    ColonColon,
    EqualEqual,
    GreaterEqual,
    LessEqual,
    AndAnd,
    OrOr,

    // Literals
    Identifier,
    NumberLit,
    StringLit,
    False,
    True,

    // Keywords
    Abstract,
    As,
    Else,
    Expose,
    From,
    Fun,
    If,
    Impl,
    Import,
    Let,
    Module,
    Pub,
    Return,
    Runtime,
    Standalone,
    Struct,
    Use,
    Var,
    While,

    // Other
    CommentString,
    SemiColon,
    EOF,
    Error,
    NewLine,
    Whitespace,

    // The module root:
    Root,
}

// ————————————————————————————— Rowan Adapter —————————————————————————————— //
// Rowan uses its own SyntaxKind(u16) to represent syntax kinds, so we        //
// implement helpers to go back and forth between rowan and zephyr            //
// representations.                                                           //
// —————————————————————————————————————————————————————————————————————————— //

impl From<SyntaxKind> for rowan::SyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u16)
    }
}

/// The language trait teach Rowan to convert between the two representations.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Lang {}
impl rowan::Language for Lang {
    type Kind = SyntaxKind;
    fn kind_from_raw(raw: rowan::SyntaxKind) -> Self::Kind {
        debug_assert!(raw.0 <= SyntaxKind::Root as u16);
        unsafe { std::mem::transmute::<u16, SyntaxKind>(raw.0) }
    }
    fn kind_to_raw(kind: Self::Kind) -> rowan::SyntaxKind {
        kind.into()
    }
}

// ——————————————————————————————— SyntaxTree ——————————————————————————————— //

/// A loseless syntax tree, representing the program as an untyped tree and preserving the full
/// source code includng whitespaces and comments.
pub struct SyntaxTree {
    root: rowan::GreenNode,
}

impl SyntaxTree {
    pub fn new(root: rowan::GreenNode) -> Self {
        Self { root }
    }

    pub fn syntax(&self) -> rowan::SyntaxNode<Lang> {
        rowan::SyntaxNode::new_root(self.root.clone())
    }
}

impl fmt::Display for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let node = self.syntax();
        write!(f, "{}", node)?;
        Ok(())
    }
}

impl fmt::Debug for SyntaxTree {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let node = self.syntax();
        write!(f, "{:?}\n", node)?;
        for child in node.children_with_tokens() {
            debug_syntax_rec(f, child, 2)?;
        }
        Ok(())
    }
}

fn debug_syntax_rec(
    f: &mut fmt::Formatter<'_>,
    node: rowan::NodeOrToken<SyntaxNode<Lang>, rowan::SyntaxToken<Lang>>,
    offset: usize,
) -> fmt::Result {
    match node {
        rowan::NodeOrToken::Node(node) => {
            write!(f, "{:>width$}{:?}\n", "", node, width = offset)?;
            for child in node.children_with_tokens() {
                debug_syntax_rec(f, child, offset + 2)?;
            }
        }
        rowan::NodeOrToken::Token(token) => {
            write!(f, "{:>width$}{:?}\n", "", token, width = offset)?;
        }
    }
    Ok(())
}
