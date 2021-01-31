//! # Resolver
//!
//! A resolver is a piece responsible for resolving the path of packages and fetching the code,
//! it is used by the Ctx to retrieve imported modules.

use crate::ctx::{ModuleKind, ModulePath, PreparedFile};
use crate::error::ErrorHandler;

pub trait Resolver {
    /// Given a module path return a list of files for that module.
    fn resolve_module(
        &self,
        module: &ModulePath,
        err: &mut ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()>;
}
