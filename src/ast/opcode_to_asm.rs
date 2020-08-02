use super::asm_tokens::Opcode;
use super::ast::*;
use crate::mir;
use crate::error::Location;

/// Opcode argument.
pub enum Argument {
    Integer(u64, Location),
    Identifier(String, Location),
    _OtherTypes(Location), // In prevision of future extension, used to remove warning match base cases
}

/// Tries to convert an opcode into a MIR statement.
/// Return an error message in case of failure.
pub fn opcode_to_asm(op: Opcode, arg: Option<Argument>, loc: Location) -> Result<AsmStatement, (String, Location)> {
    match op {
        Opcode::Drop => {
            no_arg(arg, "drop")?;
            Ok(AsmStatement::Parametric {
                param: AsmParametric::Drop,
                loc: loc,
            })
        }
        Opcode::Return => {
            no_arg(arg, "return")?;
            Ok(AsmStatement::Control {
                cntrl: AsmControl::Return,
                loc: loc,
            })
        }
        Opcode::I32Const => Ok(AsmStatement::Const {
            val: mir::Value::I32(integer(arg, "i32.const", loc)? as i32),
            loc: loc,
        }),
        Opcode::I64Const => Ok(AsmStatement::Const {
            val: mir::Value::I64(integer(arg, "i64.const", loc)?),
            loc: loc,
        }),
        Opcode::LocalGet => {
            let (ident, arg_loc) = identifier(arg, "local.get", loc)?;
            Ok(AsmStatement::Local { local: AsmLocal::Get { ident: ident, loc: arg_loc }, loc: loc.merge(arg_loc) })
        },
        Opcode::LocalSet => {
            let (ident, arg_loc) = identifier(arg, "local.set", loc)?;
            Ok(AsmStatement::Local { local: AsmLocal::Set { ident: ident, loc: arg_loc }, loc: loc.merge(arg_loc) })
        }
    }
}

/// Raises an error if the argument is not None.
fn no_arg(arg: Option<Argument>, opcode: &str) -> Result<(), (String, Location)> {
    match arg {
        None => Ok(()),
        Some(arg) => Err((format!("`{}` expects no argument", opcode), arg.get_loc())),
    }
}

/// Raises an error if the argument is not an integer.
fn integer(arg: Option<Argument>, opcode: &str, loc: Location) -> Result<i64, (String, Location)> {
    if let Some(arg) = arg {
        match arg {
            Argument::Integer(n, _) => Ok(n as i64),
            _ => Err((format!("`{}` expects an integer ", opcode), arg.get_loc())),
        }
    } else {
        Err((format!("`{}` expects an integer as argument", opcode), loc))
    }
}

/// Raises an error if the argument is not an identifier.
fn identifier(arg: Option<Argument>, opcode: &str, loc: Location) -> Result<(String, Location), (String, Location)> {
    if let Some(arg) = arg {
        let arg_loc = arg.get_loc();
        match arg {
            Argument::Identifier(s, _) => Ok((s, arg_loc)),
            _ => Err((format!("`{}` expects an identifier ", opcode), arg.get_loc())),
        }
    } else {
        Err((format!("`{}` expects an identifier as argument", opcode), loc))
    }
}

impl Argument {
    pub fn get_loc(&self) -> Location {
        match self {
            Argument::Integer(_, loc) => *loc,
            Argument::Identifier(_, loc) => *loc,
            Argument::_OtherTypes(loc) => *loc,
        }
    }
}
