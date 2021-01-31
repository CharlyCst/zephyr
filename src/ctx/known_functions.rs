//! # Known Functions
//!
//! This modules is responsible for extracting the known functions from the `core` Zephyr package.
//! This include the `malloc` function for instance.
//!
//! The functions parameters and return values are validated by this module according to the
//! following specification:
//!
//! ```ignore
//! malloc: i32 -> i32
//! ```
use super::utils::ModulePath;
use crate::error::ErrorHandler;
use crate::hir::{FunId, FunKind, ScalarType, Type};

/// An inventory of functions known from the compilers. Those are expected to live in the `core`
/// package.
pub struct KnownFunctions {
    pub malloc: FunId,
}

pub struct KnownFunctionPaths {
    pub malloc: ModulePath,
}

impl KnownFunctionPaths {
    pub fn get() -> Self {
        let core = "core";
        Self {
            malloc: ModulePath {
                root: core.to_owned(),
                path: vec!["mem".to_owned()],
            },
        }
    }
}

pub fn validate_malloc(fun: &FunKind, err: &mut ErrorHandler) -> Result<FunId, ()> {
    let (fun_id, loc, params, ret) = match fun {
        FunKind::Fun(fun) => (fun.fun_id, fun.loc, &fun.t.params, &fun.t.ret),
        FunKind::Extern(fun) => (fun.fun_id, fun.loc, &fun.t.params, &fun.t.ret),
    };
    if params != &vec![Type::Scalar(ScalarType::I32)] {
        err.report_internal(loc, String::from("Unexpected types for malloc parameters"));
        return Err(());
    }
    if ret != &vec![Type::Scalar(ScalarType::I32)] {
        err.report_internal(loc, String::from("Unexpected return value in malloc"));
        return Err(());
    }
    Ok(fun_id)
}
