use crate::ast;
use crate::ctx::{PublicDeclarations, Ctx};
use crate::error::ErrorHandler;

pub use self::names::{
    AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement, NameId, TypeDeclaration,
    ValueDeclaration,
};
pub use self::types::TypeId;
pub use crate::ast::Package;
pub use hir::Program;
pub use hir::*;

mod asm_validate;
mod ast_to_hir;
mod hir;
mod names;
mod resolver;
mod type_check;
mod types;

pub fn to_hir<'a>(
    ast_program: ast::Program,
    namespace: PublicDeclarations,
    ctx: &Ctx,
    error_handler: &mut ErrorHandler,
    verbose: bool,
) -> hir::Program {
    let mut name_resolver = resolver::NameResolver::new(error_handler);
    let program = name_resolver.resolve(ast_program, namespace);

    if verbose {
        println!("\n/// Name Resolution ///\n");

        println!("{}\n", program.names);
        println!("{}\n", program.types);
        println!("{}\n", program.constraints);

        println!("\n/// Type Checking ///\n");
    }

    let mut type_checker = type_check::TypeChecker::new(error_handler, ctx);
    let typed_program = type_checker.check(program);

    if verbose {
        println!("{}", typed_program.types);
        println!("\n/// Asm Validation ///\n");
    }

    let mut asm_validator = asm_validate::AsmValidator::new(&typed_program, error_handler);
    asm_validator.validate_asm();

    error_handler.flush_and_exit_if_err();

    if verbose {
        println!("\n/// MIR Production ///\n");
    }

    let mut hir_producer = ast_to_hir::HirProducer::new(error_handler);
    let hir = hir_producer.reduce(typed_program);

    if verbose {
        println!("{}", hir);
    }

    error_handler.flush_and_exit_if_err();
    hir
}
