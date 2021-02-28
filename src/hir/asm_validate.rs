use super::hir::{ScalarType as HirScalar, Type as HirType};
use super::names::{
    AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement, Body, FunId, Function, NameId,
    NameStore, ResolvedProgram, TypeVar,
};
use super::type_check::TypeChecker;
use crate::error::{ErrorHandler, Location};
use crate::mir::Value as MirValue;

use std::collections::HashMap;
use std::fmt;

#[derive(PartialEq, Eq)]
enum Type {
    I32,
    I64,
    F32,
    F64,
}

pub struct AsmValidator<'err, 'a, 'ctx, 'ty> {
    err: &'err mut ErrorHandler,
    checker: &'a mut TypeChecker<'ctx, 'ty>,
    fun_types: &'a HashMap<FunId, TypeVar>,
    names: &'a NameStore,
    funs: &'a Vec<Function>,
}

impl<'err, 'a, 'ctx, 'ty> AsmValidator<'err, 'a, 'ctx, 'ty> {
    pub fn new(
        prog: &'a ResolvedProgram,
        checker: &'a mut TypeChecker<'ctx, 'ty>,
        error_handler: &'err mut ErrorHandler,
    ) -> AsmValidator<'err, 'a, 'ctx, 'ty> {
        AsmValidator {
            err: error_handler,
            checker,
            fun_types: &prog.fun_types,
            names: &prog.names,
            funs: &prog.funs,
        }
    }

    /// Validate all assembly functions of the program. If a function pass
    /// through the validation phase it will be compiled to a valid wasm function.
    pub fn validate_asm(&mut self) {
        for fun in self.funs {
            if let Err(_) = self.validate_function(fun) {
                self.err.silent_report();
            }
        }
    }

    /// Validate a fuction, nothing to be done if it is not an assembly function.
    fn validate_function(&mut self, fun: &Function) -> Result<(), ()> {
        let stmts = match fun.body {
            Body::Asm(ref stmts) => stmts,
            _ => return Ok(()),
        };
        // TODO: skip type checking if 'unreachable' is found.
        let stack = self.interprete(stmts)?;
        let return_type = self.get_fun_type(fun)?;

        if let Some(return_type) = return_type {
            if let Some(actual_return_type) = stack.last() {
                if &return_type != actual_return_type {
                    self.err.report(
                        fun.loc,
                        format!(
                            "Wrong return type: expected {} got {}.",
                            return_type, actual_return_type
                        ),
                    );
                }
            } else {
                self.err.report(
                    fun.loc,
                    String::from(
                        "A value should be returned but the stack is empty after execution.",
                    ),
                );
            }
        }

        Ok(())
    }

    /// Interprete the assembly using an abstract stack and return it.
    /// Raise an error in case of stack malformation.
    fn interprete(&mut self, stmts: &Vec<AsmStatement>) -> Result<Vec<Type>, ()> {
        let mut stack = Vec::new();
        for stmt in stmts {
            match stmt {
                AsmStatement::Const { val, .. } => match val {
                    MirValue::I32(_) => stack.push(Type::I32),
                    MirValue::I64(_) => stack.push(Type::I64),
                    MirValue::F32(_) => stack.push(Type::F32),
                    MirValue::F64(_) => stack.push(Type::F64),
                    MirValue::DataPointer(_) => stack.push(Type::I32),
                },
                AsmStatement::Control { cntrl, .. } => match cntrl {
                    AsmControl::Return => return Ok(stack),
                    AsmControl::Unreachable => return Ok(stack), // TODO: add an "unreachable" flag
                },
                AsmStatement::Parametric { param, loc } => match param {
                    AsmParametric::Drop => self.drop(&mut stack, loc),
                },
                AsmStatement::Local { local, loc } => match local {
                    AsmLocal::Get { var } => match self.get_name_type(var.n_id, loc) {
                        Ok(t) => stack.push(t),
                        Err(_) => self.err.silent_report(),
                    },
                    AsmLocal::Set { var } => match self.get_name_type(var.n_id, loc) {
                        Ok(t) => self.pop_t(&mut stack, t, loc),
                        Err(_) => self.err.silent_report(),
                    },
                },
                AsmStatement::Memory { mem, loc } => match mem {
                    AsmMemory::Size => stack.push(Type::I32),
                    AsmMemory::Grow => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::I32);
                    }
                    // Loads
                    AsmMemory::I32Load { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::I32);
                    }
                    AsmMemory::I64Load { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::I64);
                    }
                    AsmMemory::F32Load { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::F32);
                    }
                    AsmMemory::F64Load { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::F64);
                    }
                    AsmMemory::I32Load8u { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        stack.push(Type::I32);
                    }
                    // Stores
                    AsmMemory::I32Store { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        self.pop_t(&mut stack, Type::I32, loc);
                    }
                    AsmMemory::I64Store { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        self.pop_t(&mut stack, Type::I64, loc);
                    }
                    AsmMemory::F32Store { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        self.pop_t(&mut stack, Type::F32, loc);
                    }
                    AsmMemory::F64Store { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        self.pop_t(&mut stack, Type::F64, loc);
                    }
                    AsmMemory::I32Store8 { .. } => {
                        self.pop_t(&mut stack, Type::I32, loc);
                        self.pop_t(&mut stack, Type::I32, loc);
                    }
                },
            }
        }
        Ok(stack)
    }

    /// Drop the value on top of the stack. Raise an error if no value is found.
    fn drop(&mut self, stack: &mut Vec<Type>, loc: &Location) {
        match stack.pop() {
            Some(_) => (),
            None => self.err.report(
                *loc,
                String::from("Trying to drop a value from an empty stack."),
            ),
        }
    }

    /// Pop a value of type `t` from the stack. Raise an error if no value, or
    /// a value of incorrect type is found on top of the stack.
    fn pop_t(&mut self, stack: &mut Vec<Type>, t: Type, loc: &Location) {
        if let Some(stack_t) = stack.pop() {
            if t != stack_t {
                self.err
                    .report(*loc, format!("Expected a {}, got a {}.", t, stack_t));
            }
        } else {
            self.err.report(
                *loc,
                format!("Trying to pop a {} value from an empty stack.", t),
            );
        }
    }

    /// Return the type associated to a given name ID.
    fn get_name_type(&mut self, n_id: NameId, loc: &Location) -> Result<Type, ()> {
        let name = self.names.get(n_id);
        let t = self.checker.get_t(name.t_var).ok_or(())?;
        self.get_type(&t, loc)
    }

    /// Convert a MIR type into a Wasm type, may rise an error.
    fn get_type(&mut self, t: &HirType, loc: &Location) -> Result<Type, ()> {
        match t {
            HirType::Scalar(HirScalar::I32) => Ok(Type::I32),
            HirType::Scalar(HirScalar::I64) => Ok(Type::I64),
            HirType::Scalar(HirScalar::F32) => Ok(Type::F32),
            HirType::Scalar(HirScalar::F64) => Ok(Type::F64),
            HirType::Scalar(HirScalar::Bool) => Ok(Type::I32),
            _ => {
                self.err
                    .report(*loc, String::from("Invalid type in assembly function."));
                Err(())
            }
        }
    }

    /// Returns the return type of the function.
    fn get_fun_type(&mut self, fun: &Function) -> Result<Option<Type>, ()> {
        let fun_t_id = self.fun_types.get(&fun.fun_id).ok_or(())?;
        // TODO: this is quire expensive, we may want to add a function to get the return type
        // directly
        let fun_t = self.checker.get_t(*fun_t_id).ok_or(())?;
        let fun_t = match fun_t {
            // Only handle single return value
            HirType::Fun(f) => match *f.ret {
                HirType::Scalar(s) => s,
                _ => {
                    self.err.report(
                        fun.loc,
                        String::from("Assembly function must return a scalar type."),
                    );
                    return Err(());
                }
            },
            _ => {
                self.err.report_internal_no_loc(String::from(
                    "Function has a non function return type.",
                ));
                return Err(());
            }
        };
        match fun_t {
            HirScalar::I32 => Ok(Some(Type::I32)),
            HirScalar::I64 => Ok(Some(Type::I64)),
            HirScalar::F32 => Ok(Some(Type::F32)),
            HirScalar::F64 => Ok(Some(Type::F64)),
            HirScalar::Bool => Ok(Some(Type::I32)),
            HirScalar::Null => Ok(None),
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
        }
    }
}
