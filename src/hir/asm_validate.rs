use crate::mir::Value as MirValue;
use super::names::{
    AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement, Body, Function, NameId, NameStore,
};
use super::types::{Type as MirType, TypeStore};
use super::types::TypedProgram;
use crate::error::{ErrorHandler, Location};

use std::fmt;

#[derive(PartialEq, Eq)]
enum Type {
    I32,
    I64,
    F32,
    F64,
}

pub struct AsmValidator<'a, 'b> {
    err: &'a mut ErrorHandler,
    types: &'b TypeStore,
    names: &'b NameStore,
    funs: &'b Vec<Function>,
}

impl<'a, 'b> AsmValidator<'a, 'b> {
    pub fn new(
        prog: &'b TypedProgram,
        error_handler: &'a mut ErrorHandler,
    ) -> AsmValidator<'a, 'b> {
        AsmValidator {
            err: error_handler,
            types: &prog.types,
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
        let t = self.types.get(name.t_id);
        self.get_type(t, loc)
    }

    /// Convert a MIR type into a Wasm type, may rise an error.
    fn get_type(&mut self, t: &MirType, loc: &Location) -> Result<Type, ()> {
        match t {
            MirType::I32 => Ok(Type::I32),
            MirType::I64 => Ok(Type::I64),
            MirType::F32 => Ok(Type::F32),
            MirType::F64 => Ok(Type::F64),
            MirType::Bool => Ok(Type::I32),
            _ => {
                self.err
                    .report(*loc, String::from("Invalid type in assembly function."));
                Err(())
            }
        }
    }

    /// Returns the return type of the function.
    fn get_fun_type(&mut self, fun: &Function) -> Result<Option<Type>, ()> {
        let fun_name = self.names.get(fun.n_id);
        let fun_t = self.types.get(fun_name.t_id);
        let fun_t = match fun_t {
            // Only handle single return value
            MirType::Fun(_, return_t) => match return_t.last() {
                Some(t) => t.clone(),
                None => MirType::Unit,
            },
            _ => {
                self.err.report_internal_no_loc(String::from(
                    "Function has a non function return type.",
                ));
                return Err(());
            }
        };
        match fun_t {
            MirType::I32 => Ok(Some(Type::I32)),
            MirType::I64 => Ok(Some(Type::I64)),
            MirType::F32 => Ok(Some(Type::F32)),
            MirType::F64 => Ok(Some(Type::F64)),
            MirType::Bool => Ok(Some(Type::I32)),
            MirType::Unit => Ok(None),
            _ => {
                self.err.report(
                    fun_name.loc,
                    format!(
                        "The return type of '{}' is not allowed in assembly function.",
                        &fun_name.name
                    ),
                );
                Err(())
            }
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
