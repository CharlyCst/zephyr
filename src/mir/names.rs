use super::types::TypeId;
use crate::error::Location;
use crate::parse::{Block, Variable};
use std::fmt;

pub type NameId = usize;

pub struct Function {
    pub ident: String,
    pub params: Vec<Variable>,
    pub locals: Vec<NameId>,
    pub block: Block,
    pub exported: bool,
    pub loc: Location,
    pub n_id: NameId,
}

pub struct Name {
    pub n_id: NameId,
    pub name: String,
    pub loc: Location,
    pub t_id: TypeId,
}

pub struct NameStore {
    names: Vec<Name>,
}

impl NameStore {
    pub fn new() -> NameStore {
        NameStore { names: Vec::new() }
    }

    pub fn get(&self, id: NameId) -> &Name {
        &self.names[id]
    }

    pub fn fresh(&mut self, name: String, loc: Location, t_id: TypeId) -> NameId {
        let id = self.names.len();
        let n = Name {
            n_id: id,
            name: name,
            loc: loc,
            t_id: t_id,
        };
        self.names.push(n);
        id
    }
}

impl fmt::Display for NameStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("NameStore {\n");
        store.push_str("    id ~ t_id - name\n\n");
        for (idx, name) in self.names.iter().enumerate() {
            store.push_str(&format!(
                "  {:>4} ~ {:>4} - {:}\n",
                idx, name.t_id, name.name
            ));
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}
