mod errors;
mod handler;

pub use errors::{Level, Location};
pub use handler::ErrorHandler;
pub(crate) use handler::DummyHandler;
