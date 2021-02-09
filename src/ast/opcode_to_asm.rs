use super::asm_tokens::Opcode;
use super::ast::*;
use crate::error::Location;
use crate::mir;

/// Opcode argument.
pub enum Argument {
    Integer(u64, Location),
    Identifier(String, Location),
}

/// Tries to convert an opcode into a MIR statement.
/// Return an error message in case of failure.
pub fn opcode_to_asm(
    op: Opcode,
    args: Vec<Argument>,
    loc: Location,
) -> Result<AsmStatement, (String, Location)> {
    match op {
        Opcode::Drop => {
            no_arg(args, "drop")?;
            Ok(AsmStatement::Parametric {
                param: AsmParametric::Drop,
                loc,
            })
        }
        Opcode::Return => {
            no_arg(args, "return")?;
            Ok(AsmStatement::Control {
                cntrl: AsmControl::Return,
                loc,
            })
        }
        Opcode::Unreachable => {
            no_arg(args, "unreachable")?;
            Ok(AsmStatement::Control {
                cntrl: AsmControl::Unreachable,
                loc,
            })
        }
        // Constants
        Opcode::I32Const => Ok(AsmStatement::Const {
            val: mir::Value::I32(integer(args, "i32.const", loc)? as i32),
            loc,
        }),
        Opcode::I64Const => Ok(AsmStatement::Const {
            val: mir::Value::I64(integer(args, "i64.const", loc)?),
            loc,
        }),
        // Locals
        Opcode::LocalGet => {
            let (ident, arg_loc) = identifier(args, "local.get", loc)?;
            Ok(AsmStatement::Local {
                local: AsmLocal::Get {
                    ident,
                    loc: arg_loc,
                },
                loc: loc.merge(arg_loc),
            })
        }
        Opcode::LocalSet => {
            let (ident, arg_loc) = identifier(args, "local.set", loc)?;
            Ok(AsmStatement::Local {
                local: AsmLocal::Set {
                    ident,
                    loc: arg_loc,
                },
                loc: loc.merge(arg_loc),
            })
        }
        // Memory
        Opcode::MemorySize => Ok(AsmStatement::Memory {
            mem: AsmMemory::Size,
            loc,
        }),
        Opcode::MemoryGrow => Ok(AsmStatement::Memory {
            mem: AsmMemory::Grow,
            loc,
        }),
        // Loads
        Opcode::I32Load => {
            let (align, offset) = memarg(args, "i32.load", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I32Load { align, offset },
                loc,
            })
        }
        Opcode::I64Load => {
            let (align, offset) = memarg(args, "i64.load", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I64Load { align, offset },
                loc,
            })
        }
        Opcode::F32Load => {
            let (align, offset) = memarg(args, "f32.load", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::F32Load { align, offset },
                loc,
            })
        }
        Opcode::F64Load => {
            let (align, offset) = memarg(args, "f64.load", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::F64Load { align, offset },
                loc,
            })
        }
        Opcode::I32Load8u => {
            let (align, offset) = memarg(args, "i32.load8_u", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I32Load8u { align, offset },
                loc,
            })
        }
        // Stores
        Opcode::I32Store => {
            let (align, offset) = memarg(args, "i32.store", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I32Store { align, offset },
                loc,
            })
        }
        Opcode::I64Store => {
            let (align, offset) = memarg(args, "i64.store", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I64Store { align, offset },
                loc,
            })
        }
        Opcode::F32Store => {
            let (align, offset) = memarg(args, "f32.store", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::F32Store { align, offset },
                loc,
            })
        }
        Opcode::F64Store => {
            let (align, offset) = memarg(args, "f64.store", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::F64Store { align, offset },
                loc,
            })
        }
        Opcode::I32Store8 => {
            let (align, offset) = memarg(args, "i32.store8", loc)?;
            Ok(AsmStatement::Memory {
                mem: AsmMemory::I32Store8 { align, offset },
                loc,
            })
        }
    }
}

/// Raises an error if there is one or more argument.
fn no_arg(args: Vec<Argument>, opcode: &str) -> Result<(), (String, Location)> {
    if let Some(arg) = args.first() {
        Err((format!("`{}` expects no argument.", opcode), arg.get_loc()))
    } else {
        Ok(())
    }
}

/// Raises an error if the argument is not an integer.
fn integer(args: Vec<Argument>, opcode: &str, loc: Location) -> Result<i64, (String, Location)> {
    if args.len() > 1 {
        return Err((
            format!("Too many arguments: expected 1, got {}.", args.len()),
            loc,
        ));
    }
    if let Some(arg) = args.first() {
        match arg {
            Argument::Integer(n, _) => Ok(*n as i64),
            _ => Err((format!("`{}` expects an integer.", opcode), arg.get_loc())),
        }
    } else {
        Err((format!("`{}` expects an integer as argument.", opcode), loc))
    }
}

/// Expects two integers: an alignment and an offset, rises an error otherwise.
fn memarg(
    args: Vec<Argument>,
    opcode: &str,
    loc: Location,
) -> Result<(u32, u32), (String, Location)> {
    if args.len() > 2 {
        return Err((
            format!("Too many arguments: expected 2, got {}.", args.len()),
            loc,
        ));
    } else if args.len() < 2 {
        return Err((
            format!("Not enough many arguments: expected 2, got {}.", args.len()),
            loc,
        ));
    }
    let arg_1 = &args[0];
    let arg_1 = match arg_1 {
        Argument::Integer(n, _) => *n as u32,
        _ => {
            return Err((
                format!("`{}` expects an integer as first argument.", opcode),
                arg_1.get_loc(),
            ))
        }
    };
    let arg_2 = &args[1];
    let arg_2 = match arg_2 {
        Argument::Integer(n, _) => *n as u32,
        _ => {
            return Err((
                format!("`{}` expects an integer as second argument.", opcode),
                arg_2.get_loc(),
            ))
        }
    };
    Ok((arg_1, arg_2))
}

/// Raises an error if the argument is not an identifier.
fn identifier(
    args: Vec<Argument>,
    opcode: &str,
    loc: Location,
) -> Result<(String, Location), (String, Location)> {
    if args.len() > 1 {
        return Err((
            format!("Too many arguments: expected 1, got {}.", args.len()),
            loc,
        ));
    }
    if let Some(arg) = args.first() {
        let arg_loc = arg.get_loc();
        match arg {
            Argument::Identifier(s, _) => Ok((s.clone(), arg_loc)),
            _ => Err((
                format!("`{}` expects an identifier.", opcode),
                arg.get_loc(),
            )),
        }
    } else {
        Err((
            format!("`{}` expects an identifier as argument.", opcode),
            loc,
        ))
    }
}

impl Argument {
    pub fn get_loc(&self) -> Location {
        match self {
            Argument::Integer(_, loc) => *loc,
            Argument::Identifier(_, loc) => *loc,
        }
    }
}
