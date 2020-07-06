use crate::ast;
use crate::cli::Config;
use crate::error::ErrorHandler;

use self::names::{NameStore, ResolvedProgram};
use self::types::TypeStore;

use std::collections::HashMap;

pub use self::names::{Declaration, NameId};
pub use self::types::TypeId;
pub use mir::*;

mod ast_to_mir;
mod mir;
mod names;
mod resolver;
mod type_check;
mod types;

pub struct TypedProgram {
    pub funs: Vec<names::Function>,
    pub names: NameStore,
    pub types: TypeStore,
    pub pub_decls: HashMap<String, Declaration>,
}

pub use mir::Program;

pub fn to_mir<'a>(
    ast_program: ast::Program,
    namespace: HashMap<String, HashMap<String, Declaration>>,
    error_handler: &mut ErrorHandler,
    config: &Config,
) -> mir::Program {
    let mut name_resolver = resolver::NameResolver::new(error_handler);
    let program = name_resolver.resolve(ast_program, namespace);

    if config.verbose {
        println!("\n/// Name Resolution ///\n");

        println!("{}\n", program.names);
        println!("{}\n", program.types);
        println!("{}\n", program.constraints);

        println!("\n/// Type Checking ///\n");
    }

    let mut type_checker = type_check::TypeChecker::new(error_handler);
    let typed_program = type_checker.check(program);

    if config.verbose {
        println!("{}", typed_program.types);
    }

    error_handler.flush_and_exit_if_err();

    if config.verbose {
        println!("\n/// MIR Production ///\n");
    }

    let mut mir_producer = ast_to_mir::MIRProducer::new(error_handler);
    let mir = mir_producer.reduce(typed_program);

    if config.verbose {
        println!("{}", mir);
    }

    error_handler.flush_and_exit_if_err();
    mir
}
