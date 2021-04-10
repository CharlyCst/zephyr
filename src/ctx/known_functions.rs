//! # Known Functions
//!
//! This modules is responsible for extracting the known functions, structs and other values from
//! the `core` Zephyr package. This include the `malloc` function for instance.
//!
//! The functions parameters and return values are validated by this module according to the
//! following specification:
//!
//! ```ignore
//! malloc: i32 -> i32
//! ```
use super::utils::ModulePath;
use crate::error::ErrorHandler;
use crate::hir::known_ids::*;
use crate::hir::{FunId, FunKind, ScalarType, Struct, StructId, Type};

const CORE: &str = "core";

/// A bunch of IDs for values expected by the compiler.
pub struct KnownValues {
    pub funs: KnownFunctions,
    pub structs: KnownStructs,
}

/// An inventory of functions expected by the compiler.
pub struct KnownFunctions {
    pub malloc: FunId,
}

pub struct KnownFunctionPaths {
    pub malloc: ModulePath,
}

/// A bunch of structs expected by the compiler.
pub struct KnownStructs {
    pub str: StructId,
}

pub struct KnownStructPaths {
    pub str: ModulePath,
}

impl KnownValues {
    /// Return uninitialized values, where IDs does not map to anything.
    /// This is useful for first building the HIR of the `core` package which is the one supposed
    /// to defines those values.
    ///
    /// Using uninitialized values while parsing a module other than `core` is undefined behaviors.
    pub fn uninitialized() -> Self {
        // The module ID 0 is reserved, thus a value ID of 0 will never collide.
        Self {
            funs: KnownFunctions { malloc: MALLOC_ID },
            structs: KnownStructs { str: STR_ID },
        }
    }

    /// Check if the IDs are initialized.
    pub fn is_initialized(&self) -> bool {
        self.funs.malloc != MALLOC_ID
    }
}

impl KnownFunctionPaths {
    pub fn get() -> Self {
        Self {
            malloc: ModulePath {
                root: CORE.to_owned(),
                path: vec!["mem".to_owned()],
            },
        }
    }
}

impl KnownStructPaths {
    pub fn get() -> Self {
        Self {
            str: ModulePath {
                root: CORE.to_owned(),
                path: vec!["str".to_owned()],
            },
        }
    }
}

// —————————————————————————————— Validation ——————————————————————————————— //

pub fn validate_malloc(fun: &FunKind, err: &mut impl ErrorHandler) -> Result<FunId, ()> {
    let (fun_id, loc, params, ret) = match fun {
        FunKind::Fun(fun) => (fun.fun_id, fun.loc, &fun.t.params, &fun.t.ret),
        FunKind::Extern(fun) => (fun.fun_id, fun.loc, &fun.t.params, &fun.t.ret),
    };
    if params != &vec![Type::Scalar(ScalarType::I32)] {
        err.report_internal(loc, String::from("Unexpected types for malloc parameters"));
        return Err(());
    }
    if ret.as_ref() != &Type::Scalar(ScalarType::I32) {
        err.report_internal(loc, String::from("Unexpected return value in malloc"));
        return Err(());
    }
    Ok(fun_id)
}

pub fn validate_str(struc: &Struct, err: &mut impl ErrorHandler) -> Result<StructId, ()> {
    let loc = struc.loc;
    if struc.fields.len() != 2 {
        err.report_internal(
            loc,
            String::from("Str must have exactly two fields: 'len' and 'start'"),
        );
        return Err(());
    }
    if let Some(len) = struc.fields.get("len") {
        if len.t != Type::Scalar(ScalarType::I32) {
            err.report_internal(len.loc, String::from("Str.len must have type i32"));
            return Err(());
        }
    } else {
        err.report_internal(loc, String::from("Str must have a 'len' field"));
        return Err(());
    };
    if let Some(start) = struc.fields.get("start") {
        if start.t != Type::Scalar(ScalarType::I32) {
            err.report_internal(start.loc, String::from("Str.start must have type i32"));
        }
    } else {
        err.report_internal(loc, String::from("Str must have a 'start' field"));
        return Err(());
    };
    Ok(struc.s_id)
}
