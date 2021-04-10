use std::collections::{HashMap, HashSet};
use std::fmt;

use super::ctx::ModId;
use crate::ast;
use crate::hir;

pub use hir::ValueDeclaration;

/// A list of packages known from the compiler and expected to be available.
#[allow(dead_code)]
pub enum KnownPackage {
    Core,
}

impl KnownPackage {
    #[allow(dead_code)]
    pub fn as_str(&self) -> &'static str {
        match self {
            KnownPackage::Core => "core",
        }
    }
}

/// A list of public declarations in a given package.
#[derive(Clone)]
pub struct ModuleDeclarations {
    pub mod_id: ModId,
    pub val_decls: HashMap<String, hir::ValueDeclaration>,
    pub type_decls: HashMap<String, hir::Type>,
    pub runtime_modules: HashSet<String>,
}

impl ModuleDeclarations {
    pub fn new(mod_id: ModId) -> Self {
        Self {
            mod_id,
            val_decls: HashMap::new(),
            type_decls: HashMap::new(),
            runtime_modules: HashSet::new(),
        }
    }
}

/// A file prepared to be passed to the AST parser.
pub struct PreparedFile {
    pub code: String,
    pub f_id: u16,
    pub file_name: String,
    pub kind: ast::Kind,
}

/// A module can be either standalone (inside a single file) or standard (occupate the whole
/// directory).
#[derive(Eq, PartialEq)]
pub enum ModuleKind {
    Standalone,
    Standard,
}

/// A path to a module from the package root.
#[derive(Clone, Eq, PartialEq, Hash)]
pub struct ModulePath {
    pub root: String,
    pub path: Vec<String>,
}

impl ModulePath {
    pub fn from_root(root: String) -> Self {
        Self {
            root,
            path: Vec::new(),
        }
    }

    #[allow(dead_code)]
    pub fn from_known_package(pkg: KnownPackage) -> Self {
        let root = pkg.as_str().to_owned();
        Self {
            root,
            path: Vec::new(),
        }
    }

    pub fn alias(&self) -> &str {
        if let Some(module) = self.path.last() {
            module.as_str()
        } else {
            self.root.as_str()
        }
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut path = Vec::with_capacity(1 + self.path.len());
        path.push(self.root.clone());
        path.extend(self.path.clone());
        write!(f, "{}", path.join("."))
    }
}
