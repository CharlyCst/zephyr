//! # Resolver
//!
//! A resolver is a piece responsible for resolving the path of packages and fetching the code,
//! it is used by the Ctx to retrieve imported modules.
mod resolver;
mod standard_resolver;

pub use resolver::Resolver;
pub use standard_resolver::StandardResolver;
