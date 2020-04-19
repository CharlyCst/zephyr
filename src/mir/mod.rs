use crate::parse;

use self::names::NameStore;
use self::types::{ConstraintStore, TypeVarStore};

mod names;
mod resolver;
mod types;

pub struct Program {
    pub funs: Vec<parse::Function>,
    pub names: NameStore,
    pub types: TypeVarStore,
    pub constraints: ConstraintStore,
}

pub fn to_mir(functions: Vec<parse::Function>) {
    let mut name_resolver = resolver::NameResolver::new();
    let program = name_resolver.resolve(functions);

    println!("\n/// Name Resolution ///\n");

    println!("{}\n", program.names);
    println!("{}\n", program.types);
    println!("{}\n", program.constraints);
}
