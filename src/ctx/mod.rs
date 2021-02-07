//! The compilation context
//!
//! This module contains the Ctx, that is the compilation context. The Ctx can be use either to
//! compile Zephyr code down to wasm, or to query information about the code itself.
mod ctx;
mod known_functions;
mod utils;

pub use ctx::{Ctx, ModId};
pub use known_functions::{KnownFunctions, KnownStructs, KnownValues};
pub use utils::{
    ModuleDeclarations, ModuleKind, ModulePath, PreparedFile, TypeDeclaration, ValueDeclaration,
};
