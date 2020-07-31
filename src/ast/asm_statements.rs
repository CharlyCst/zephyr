use std::fmt;

use crate::mir::Value;

pub enum AsmStatement {
    Const { val: Value },
    Control { cntrl: AsmControl },
}

pub enum AsmControl {
    Return,
}

impl fmt::Display for AsmStatement {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmStatement::Const { val } => write!(f, "{}", val),
            AsmStatement::Control { cntrl } => match cntrl {
                AsmControl::Return => write!(f, "return")
            }
        }
    }
}

