use super::tokens::Opcode;
use crate::mir;

/// Opcode argument.
pub enum Argument {
    Integer(u64),
    _OtherTypes, // In prevision of future extension, used to remove warning match base cases
}

/// Tries to convert an opcode into a MIR statement.
/// Return an error message in case of failure.
pub fn opcode_to_mir(op: Opcode, arg: Option<Argument>) -> Result<mir::Statement, String> {
    match op {
        Opcode::Return => {
            no_arg(arg, "return")?;
            Ok(mir::Statement::Control {
                cntrl: mir::Control::Return,
            })
        }
        Opcode::I32Const => Ok(mir::Statement::Const {
            val: mir::Value::I32(integer(arg, "i32.const")? as i32),
        }),
        Opcode::I64Const => Ok(mir::Statement::Const {
            val: mir::Value::I32(integer(arg, "i64.const")? as i32),
        }),
    }
}

/// Raises an error if the argument is not None.
fn no_arg(arg: Option<Argument>, opcode: &str) -> Result<(), String> {
    if arg.is_some() {
        return Err(format!("`{}` expects no argument", opcode));
    } else {
        Ok(())
    }
}

/// Raises an error if the argument is not an integer.
fn integer(arg: Option<Argument>, opcode: &str) -> Result<u64, String> {
    if let Some(arg) = arg {
        match arg {
            Argument::Integer(n) => Ok(n),
            _ => Err(format!("`{}` expects an integer ", opcode)),
        }
    } else {
        return Err(format!("`{}` expects an integer as argument", opcode));
    }
}
