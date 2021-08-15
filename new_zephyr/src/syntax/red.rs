#![allow(dead_code)]

use std::fmt;
use std::rc::Rc;
use std::sync::Arc;

use super::common::{NodeOrToken, SyntaxKind};
use super::green::{GreenElement, GreenNode, GreenToken};

pub type RedNode = Rc<RedNodeData>;
pub type RedToken = Rc<RedTokenData>;
pub type RedElement = NodeOrToken<RedNode, RedToken>;

#[derive(Clone)]
pub struct RedNodeData {
    parent: Option<RedNode>,
    index_in_parent: u32,
    text_offset: u32,
    green: GreenNode,
}

#[derive(Clone)]
pub struct RedTokenData {
    parent: Option<RedNode>,
    text_offset: u32,
    green: GreenToken,
}

impl RedNodeData {
    pub fn new(root: GreenNode) -> RedNode {
        Rc::new(RedNodeData {
            parent: None,
            index_in_parent: 0,
            text_offset: 0,
            green: root,
        })
    }

    fn green(&self) -> &GreenNode {
        &self.green
    }

    fn index_in_parent(&self) -> usize {
        self.index_in_parent as usize
    }

    pub fn kind(&self) -> SyntaxKind {
        self.green().kind()
    }

    pub fn text_len(&self) -> usize {
        self.green().text_len()
    }

    pub fn text_offset(&self) -> usize {
        self.text_offset as usize
    }

    pub fn children<'a>(self: &'a RedNode) -> impl std::iter::Iterator<Item = RedElement> + 'a {
        let mut offset_in_parent = 0;
        self.green.children().enumerate().map(move |(idx, child)| {
            let text_offset = self.text_offset + offset_in_parent;
            offset_in_parent += child.text_len() as u32;

            match child {
                NodeOrToken::Node(node) => Rc::new(RedNodeData {
                    parent: Some(Rc::clone(self)),
                    index_in_parent: idx as u32,
                    text_offset,
                    green: node.clone(),
                })
                .into(),
                NodeOrToken::Token(token) => Rc::new(RedTokenData {
                    parent: Some(Rc::clone(self)),
                    text_offset,
                    green: token.clone(),
                })
                .into(),
            }
        })
    }

    pub fn parent(&self) -> Option<&RedNode> {
        self.parent.as_ref()
    }

    pub fn replace_child<'a>(self: &'a RedNode, idx: usize, new_child: RedElement) -> RedNode {
        let new_green = Arc::new(self.green().replace_child(idx, new_child.green()));
        self.replace_ourselves(new_green)
    }

    fn replace_ourselves(self: &RedNode, green: GreenNode) -> RedNode {
        match self.parent() {
            Some(parent) => {
                let new_green = Arc::new(
                    parent
                        .green()
                        .replace_child(self.index_in_parent(), green.clone().into()),
                );
                let new_parent = parent.replace_ourselves(new_green);
                Rc::new(RedNodeData {
                    parent: Some(new_parent),
                    index_in_parent: self.index_in_parent,
                    text_offset: self.text_offset,
                    green,
                })
            }
            None => RedNodeData::new(green),
        }
    }
}

impl RedTokenData {
    pub fn new(root: GreenToken) -> RedToken {
        Rc::new(RedTokenData {
            parent: None,
            text_offset: 0,
            green: root,
        })
    }

    fn green(&self) -> &GreenToken {
        &self.green
    }

    pub fn kind(&self) -> SyntaxKind {
        self.green.kind()
    }

    pub fn text_len(&self) -> usize {
        self.green.text_len()
    }

    pub fn text_offset(&self) -> usize {
        self.text_offset as usize
    }

    pub fn text(&self) -> &str {
        self.green().text()
    }

    pub fn parent(&self) -> Option<&RedNode> {
        self.parent.as_ref()
    }
}

impl RedElement {
    fn green(&self) -> GreenElement {
        match self {
            NodeOrToken::Node(node) => node.green().clone().into(),
            NodeOrToken::Token(token) => token.green().clone().into(),
        }
    }
}

impl From<RedToken> for RedElement {
    fn from(token: RedToken) -> Self {
        Self::Token(token)
    }
}

impl From<RedNode> for RedElement {
    fn from(node: RedNode) -> Self {
        Self::Node(node)
    }
}

impl fmt::Display for RedTokenData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.text())
    }
}

impl fmt::Display for RedNodeData {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for child in self.green().children() {
            fmt::Display::fmt(child, f)?;
        }
        Ok(())
    }
}

impl fmt::Display for RedElement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            NodeOrToken::Node(node) => fmt::Display::fmt(node, f),
            NodeOrToken::Token(token) => fmt::Display::fmt(token, f),
        }
    }
}
