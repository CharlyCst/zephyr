use super::types::TypeId;
use crate::error::Location;
use std::fmt;

pub struct Name {
    pub id: usize,
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

    pub fn get(&self, id: usize) -> &Name {
        &self.names[id]
    }

    pub fn fresh(&mut self, name: String, loc: Location, t_id: usize) -> usize {
        let id = self.names.len();
        let n = Name {
            id: id,
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
