//! # Known Functions
//!
//! This modules is responsible for extracting the known functions from the `core` Zephyr package.
//! This include the `malloc` function for instance.
//!
//! The functions parameters and return values are validated by this module according to the
//! following specification:
//!
//! ```
//! malloc: i32 -> i32
//! ```
use crate::error::ErrorHandler;
use crate::hir::{FunId, Function, Program, ScalarType, Type};

/// An inventory of functions known from the compilers. Those are expected to live in the `core`
/// package.
pub struct KnownFunctions {
    pub malloc: FunId,
}

impl KnownFunctions {
    /// Get the functions Id and validate their types against expectation. This is expected to be
    /// used on the `core` package HIR.
    pub fn from_hir(hir: &Program, err: &mut ErrorHandler) -> KnownFunctions {
        let mut malloc: Option<FunId> = None;
        for fun in &hir.funs {
            match fun.ident.as_str() {
                "malloc" => validate_malloc(fun, &mut malloc, err),
                _ => (),
            }
        }

        KnownFunctions {
            malloc: malloc.expect("Missing malloc in 'core'"),
        }
    }
}

fn validate_malloc(fun: &Function, malloc: &mut Option<FunId>, err: &mut ErrorHandler) {
    let loc = fun.loc;
    if malloc.is_some() {
        err.report_internal(loc, String::from("Multiple definitions of malloc found"));
        return;
    }

    if fun.t.params != vec![Type::Scalar(ScalarType::I32)] {
        err.report_internal(loc, String::from("Unexpected types for malloc parameters"));
        return;
    }
    if fun.t.ret != vec![Type::Scalar(ScalarType::I32)] {
        err.report_internal(loc, String::from("Unexpected return value in malloc"));
        return;
    }
    *malloc = Some(fun.fun_id);
}
