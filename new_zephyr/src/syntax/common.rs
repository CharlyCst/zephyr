#![allow(dead_code)]

#[derive(Clone, Debug)]
pub enum NodeOrToken<N, T> {
    Node(N),
    Token(T),
}

#[derive(Clone, Copy, Debug)]
pub enum SyntaxKind {
    // Nodes
    Fun,
    BinExpr,

    // Tokens
    Whitespace,
    Int,
    Float,
    Plus,
    Minus,
    Star,
    Slash,
}

impl<N, T> NodeOrToken<N , T> {
    pub fn into_node(self) -> Option<N> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    pub fn into_token(self) -> Option<T> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }
}
