use crate::error::Location;
use crate::mir;
use std::fmt;

pub struct Variable {
    pub ident: String,
    pub t: String,
    pub loc: Location,
}

pub struct Function {
    pub ident: String,
    pub params: Vec<Variable>,
    pub result: Option<(String, Location)>,
    pub stmts: Vec<mir::Statement>,
    pub is_pub: bool,
    pub loc: Location,
}

pub struct Exposed {
    pub fun_name: String,
    pub exposed_as: Option<String>,
}

pub struct Program {
    pub package: String,
    pub exposed: Vec<Exposed>,
    pub funs: Vec<Function>,
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut program = String::from("package \"");
        program.push_str(&self.package);
        program.push_str("\"\n\n");

        for expose in &self.exposed {
            program.push_str(&format!("expose {}", expose.fun_name));
            if let Some(ref exposed_as) = expose.exposed_as {
                program.push_str(&format!(" as {}", exposed_as));
            }
            program.push_str("\n");
        }
        program.push_str("\n");
        for fun in &self.funs {
            program.push_str(&format!("{}\n", fun));
        }
        write!(f, "{}", program)
    }
}

impl fmt::Display for Function {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let prefix = if self.is_pub { "pub " } else { "" };
        let params = self
            .params
            .iter()
            .map(|v| {
                let mut param = v.ident.clone();
                param.push_str(" ");
                param.push_str(&v.t);
                param
            })
            .collect::<Vec<String>>()
            .join(", ");
        let result_type = if let Some((ref t, _)) = self.result {
            let mut t = t.clone();
            t.push_str(" ");
            t
        } else {
            String::from("")
        };
        write!(
            f,
            "{}{}({}) {}{{\n}};",
            prefix,
            self.ident,
            params,
            result_type //, self.block
        )
    }
}
