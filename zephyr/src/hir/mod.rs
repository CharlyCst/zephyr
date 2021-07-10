use std::collections::HashMap;

use crate::ast;
use crate::ctx::{Ctx, KnownValues, ModId};
use crate::error::ErrorHandler;

pub use self::names::{
    AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement, NameId, TypeId, ValueDeclaration,
};
pub use crate::ast::Module;
pub use hir::*;
pub use names::{Data, DataId, NamespaceKind};
pub use store::known_ids;
pub use store::Identifier;

mod asm_validate;
mod ast_to_hir;
mod hir;
mod names;
mod resolver;
mod store;
mod type_check;

pub fn to_hir(
    ast_program: ast::Program,
    namespace: HashMap<String, ModId>,
    ctx: &Ctx,
    known_values: &KnownValues,
    error_handler: &mut impl ErrorHandler,
    verbose: bool,
) -> hir::Program {
    let store = type_check::TyStore::new();
    let mut checker = type_check::TypeChecker::new(ctx, &store, ast_program.module.id);
    let mut name_resolver = resolver::NameResolver::new(error_handler);
    let program = name_resolver.resolve(ast_program, namespace, ctx, &mut checker, known_values);

    if verbose {
        println!("\n/// Name Resolution ///\n");
        println!("{}\n", program.names);
        println!("{}\n", checker);
        println!("\n/// Type Checking ///\n");
    }

    let _ = checker.type_check(&program.structs, error_handler);

    if verbose {
        println!("{}", checker);
        println!("\n/// Asm Validation ///\n");
    }

    let mut asm_validator = asm_validate::AsmValidator::new(&program, &mut checker, error_handler);
    asm_validator.validate_asm();

    error_handler.flush_and_exit_if_err();

    if verbose {
        println!("\n/// HIR Production ///\n");
    }

    let mut hir_producer = ast_to_hir::HirProducer::new(error_handler);
    let hir = hir_producer.reduce(program, checker);

    if verbose {
        println!("{}", hir);
    }

    error_handler.flush_and_exit_if_err();
    hir
}
