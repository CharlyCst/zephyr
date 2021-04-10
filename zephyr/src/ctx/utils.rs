use std::collections::{HashMap, HashSet};

use super::ctx::ModId;
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
