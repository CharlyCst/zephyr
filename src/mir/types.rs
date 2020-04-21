use std::cmp;
use std::fmt;

pub mod id {
    pub const T_ID_BOOL: usize = 0;
    pub const T_ID_INTEGER: usize = 1;
    pub const T_ID_NUMERIC: usize = 2;
    pub const T_ID_BASIC: usize = 3;
}

pub type TypeId = usize;

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone)]
pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Any,
    Unit,
    Fun(Vec<Type>, Vec<Type>),
}

pub enum TypeConstraint {
    Equality(TypeId, TypeId),
    Included(TypeId, TypeId), // Included(t_1, t_2) <=> t_1 ⊂ y_2
    Return(TypeId, TypeId),   // Return(fun_type, returned_type)
}

pub struct ConstraintStore {
    constraints: Vec<TypeConstraint>,
}

impl ConstraintStore {
    pub fn new() -> ConstraintStore {
        let store = Vec::new();
        ConstraintStore { constraints: store }
    }

    pub fn add(&mut self, constr: TypeConstraint) {
        self.constraints.push(constr)
    }
}

pub struct TypeStore {
    types: Vec<Type>,
}

// The store takes the responsibility for sorting type variable's candidates
pub struct TypeVarStore {
    types: Vec<Vec<Type>>,
}

impl TypeVarStore {
    pub fn new() -> TypeVarStore {
        let mut store = TypeVarStore { types: Vec::new() };

        // If you need to change insertion order, update T_ID constants accordingly
        // Tests are provided at the bottom of the file to ensure good ordering
        store.fresh(vec![Type::Bool]);
        store.fresh(vec![Type::I32, Type::I64]);
        store.fresh(vec![Type::F32, Type::F64, Type::I32, Type::I64]);
        store.fresh(vec![Type::Bool, Type::F32, Type::F64, Type::I32, Type::I64]);

        store
    }

    pub fn get(&self, id: TypeId) -> &Vec<Type> {
        &self.types[id]
    }

    pub fn replace(&mut self, id: TypeId, mut new_types: Vec<Type>) {
        std::mem::replace(&mut self.types[id], new_types);
    }

    pub fn fresh(&mut self, mut candidate: Vec<Type>) -> TypeId {
        candidate.sort_unstable(); // Ensure sorted elements
        let id = self.types.len();
        self.types.push(candidate);
        id
    }
}

impl<'a> IntoIterator for &'a ConstraintStore {
    type Item = <std::slice::Iter<'a, TypeConstraint> as IntoIterator>::Item;
    type IntoIter = <std::slice::Iter<'a, TypeConstraint> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        let c = &self.constraints;
        c.into_iter()
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Bool => write!(f, "bool"),
            Type::Any => write!(f, "any"),
            Type::Unit => write!(f, "unit"),
            Type::Fun(params, results) => {
                let p_types = params
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", ");
                let r_types = results
                    .iter()
                    .map(|result| format!("{}", result))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fun({}) {}", p_types, r_types)
            }
        }
    }
}

impl fmt::Display for TypeVarStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("TypeVarStore {\n");
        store.push_str("  t_id - candidates\n\n");
        for (idx, t) in self.types.iter().enumerate() {
            let candidates = t
                .iter()
                .map(|c| format!("{}", c))
                .collect::<Vec<String>>()
                .join(" | ");
            store.push_str("  ");
            store.push_str(&format!("{:>4} - {}", idx, candidates));
            store.push_str("\n");
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}

impl fmt::Display for ConstraintStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("ConstraintStore {\n");
        store.push_str("  t_id ~ t_id\n\n");
        for constr in &self.constraints {
            match constr {
                TypeConstraint::Equality(t_1, t_2) => {
                    store.push_str(&format!("  {:>4} = {:>4}\n", t_1, t_2))
                }
                TypeConstraint::Included(t_1, t_2) => {
                    store.push_str(&format!("  {:>4} ⊂ {:>4}\n", t_1, t_2))
                }
                TypeConstraint::Return(fun_t, ret_t) => {
                    store.push_str(&format!("  {:>4} -> {:>3}\n", fun_t, ret_t))
                }
            };
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}

#[cfg(test)]
mod tests {
    use super::id;
    use super::*;

    #[test]
    fn test_built_in_types_id() {
        let store = TypeVarStore::new();

        assert_eq!(vec![Type::Bool], *store.get(id::T_ID_BOOL));
        assert_eq!(vec![Type::I32, Type::I64], *store.get(id::T_ID_INTEGER));
        assert_eq!(
            vec![Type::F32, Type::F64, Type::I32, Type::I64],
            *store.get(id::T_ID_NUMERIC)
        );
        assert_eq!(
            vec![Type::Bool, Type::F32, Type::F64, Type::I32, Type::I64],
            *store.get(id::T_ID_BASIC)
        );
    }
}
