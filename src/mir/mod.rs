use crate::parse;

use self::names::NameStore;
use self::types::{ConstraintStore, TypeStore, TypeVarStore};

pub use self::names::NameId;
pub use self::types::TypeId;

mod ast_to_mir;
mod mir;
mod names;
mod resolver;
mod type_check;
mod types;

pub struct ResolvedProgram {
    pub funs: Vec<names::Function>,
    pub names: NameStore,
    pub types: TypeVarStore,
    pub constraints: ConstraintStore,
}

pub struct TypedProgram {
    pub funs: Vec<names::Function>,
    pub names: NameStore,
    pub types: TypeStore,
}

pub use mir::Program;

pub fn to_mir(functions: Vec<parse::Function>) -> mir::Program {
    let mut name_resolver = resolver::NameResolver::new();
    let program = name_resolver.resolve(functions);

    println!("\n/// Name Resolution ///\n");

    println!("{}\n", program.names);
    println!("{}\n", program.types);
    println!("{}\n", program.constraints);

    println!("\n/// Type Checking ///\n");

    let mut type_checker = type_check::TypeChecker::new();
    let typed_program = type_checker.check(program);

    println!("{}", typed_program.types);

    println!("\n/// MIR Production ///\n");

    let mut mir_producer = ast_to_mir::MIRProducer::new();
    let mir = mir_producer.reduce(typed_program);

    println!("{}", mir);

    mir
}
