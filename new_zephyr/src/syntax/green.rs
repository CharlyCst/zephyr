#![allow(dead_code)]

use std::fmt;
use std::sync::Arc;

use super::common::{NodeOrToken, SyntaxKind};

pub type GreenNode = Arc<GreenNodeData>;
pub type GreenToken = Arc<GreenTokenData>;
pub type GreenElement = NodeOrToken<GreenNode, GreenToken>;

#[derive(Debug)]
pub struct GreenTokenData {
    kind: SyntaxKind,
    text: String,
}

#[derive(Debug)]
pub struct GreenNodeData {
    kind: SyntaxKind,
    len: u32,
    children: Vec<GreenElement>,
}

impl GreenTokenData {
    pub fn new(kind: SyntaxKind, text: String) -> GreenTokenData {
        GreenTokenData { kind, text }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text(&self) -> &str {
        self.text.as_str()
    }

    pub fn text_len(&self) -> usize {
        self.text.len()
    }
}

impl GreenNodeData {
    pub fn new(kind: SyntaxKind, children: Vec<GreenElement>) -> GreenNodeData {
        let len = children.iter().map(|c| c.text_len()).sum::<usize>() as u32;
        GreenNodeData {
            kind,
            len,
            children,
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.kind
    }

    pub fn text_len(&self) -> usize {
        self.len as usize
    }

    pub fn children(&self) -> impl Iterator<Item = &GreenElement> {
        self.children.iter()
    }

    pub fn replace_child(&self, idx: usize, new_child: GreenElement) -> GreenNodeData {
        assert!(idx < self.children.len());

        let left = self.children.iter().take(idx).cloned();
        let middle = std::iter::once(new_child);
        let right = self.children.iter().skip(idx + 1).cloned();
        let new_children = left.chain(middle).chain(right).collect();

        GreenNodeData::new(self.kind, new_children)
    }
}

impl From<GreenToken> for GreenElement {
    fn from(token: GreenToken) -> Self {
        Self::Token(token)
    }
}

impl From<GreenNode> for GreenElement {
    fn from(node: GreenNode) -> Self {
        Self::Node(node)
    }
}

impl GreenElement {
    pub fn text_len(&self) -> usize {
        match self {
            NodeOrToken::Node(node) => node.text_len(),
            NodeOrToken::Token(token) => token.text_len(),
        }
    }
}

impl fmt::Display for GreenTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl fmt::Display for GreenNodeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for child in &self.children {
            fmt::Display::fmt(child, f)?;
        }
        Ok(())
    }
}

impl fmt::Display for GreenElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeOrToken::Node(node) => fmt::Display::fmt(node, f),
            NodeOrToken::Token(token) => fmt::Display::fmt(token, f),
        }
    }
}
