use crate::ctx::{ModuleKind, ModulePath, PreparedFile};
use crate::error::ErrorHandler;

/// A module resolver, used to locate and retrieve code.
pub trait Resolver {
    /// Given a module path return a list of files for that module.
    fn resolve_module(
        &self,
        module: &ModulePath,
        err: &mut impl ErrorHandler,
    ) -> Result<(Vec<PreparedFile>, ModuleKind), ()>;
}
