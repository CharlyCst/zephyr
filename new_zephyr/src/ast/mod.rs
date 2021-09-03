// mod ast;
mod diagnostics;
mod parser;
mod scanner;
mod tokens;
mod syntax;

pub use scanner::scan;
pub use parser::parse;
