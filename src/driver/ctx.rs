//! The Compilation Context
#![allow(dead_code)]
use std::collections::HashMap;

use crate::hir;
use super::utils::PackageDeclarations;

/// The global compilation context.
pub struct Ctx {
    structs: HashMap<hir::StructId, hir::Struct>,
    funs: HashMap<hir::FunId, hir::FunKind>,
    imports: Vec<hir::Import>,
    packages: Vec<hir::Package>,
    public_decls: HashMap<String, PackageDeclarations>,
}

impl Ctx {
    pub fn new() -> Self {
        Self {
            structs: HashMap::new(),
            funs: HashMap::new(),
            imports: Vec::new(),
            packages: Vec::new(),
            public_decls: HashMap::new(),
        }
    }

    /// Insert types into the context.
    pub fn extend_structs(&mut self, structs: HashMap<hir::StructId, hir::Struct>) {
        self.structs.extend(structs);
    }

    /// Extend the Ctx HIR with a package.
    ///
    /// params:
    ///  - hir: The hir package to add
    ///  - path: the import path of the hir package, can be use to get public declarations.
    pub fn extend_hir(&mut self, hir: hir::Program, path: String) {
        for (s_id, struc) in hir.structs {
            let prev = self.structs.insert(s_id, struc);
            assert!(prev.is_none()); // s_id must be unique
        }
        for fun in hir.funs {
            let prev = self.funs.insert(fun.fun_id, hir::FunKind::Fun(fun));
            assert!(prev.is_none()); // fun_id must be unique
        }
        for import in hir.imports {
            let mut prototypes = Vec::new();
            for fun in import.prototypes {
                prototypes.push(fun.fun_id);
                let prev = self.funs.insert(fun.fun_id, hir::FunKind::Extern(fun));
                assert!(prev.is_none()); // fun_id must be unique
            }
            self.imports.push(hir::Import {
                from: import.from,
                loc: import.loc,
                prototypes,
            })
        }
        self.packages.push(hir.package);
        self.public_decls.insert(path , hir.pub_decls);
    }

    pub fn get_s(&self, s_id: hir::StructId) -> Option<&hir::Struct> {
        self.structs.get(&s_id)
    }
}
