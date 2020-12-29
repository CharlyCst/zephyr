use super::names;
use crate::driver::PackageDeclarations;
use crate::error::Location;
use crate::hir::Package;

use std::fmt;

pub mod id {
    pub const _T_ID_BUG: usize = 0;
    pub const T_ID_BOOL: usize = 1;
    pub const T_ID_INTEGER: usize = 2;
    pub const T_ID_FLOAT: usize = 3;
    pub const T_ID_NUMERIC: usize = 4;
    pub const T_ID_BASIC: usize = 5;
    pub const T_ID_UNIT: usize = 6;
}
// Please update this const when adding/removing default T_ID
const NB_DEFAULT_T_ID: usize = 7;

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
    Bug, // Used to signal that an error occurred somewhere
    Fun(Vec<Type>, Vec<Type>),
    Struct(names::StructId),
}

pub struct TypedProgram {
    pub funs: Vec<names::Function>,
    pub imports: Vec<names::Imports>,
    pub names: names::NameStore,
    pub types: TypeStore,
    pub pub_decls: PackageDeclarations,
    pub package: Package,
}

pub enum TypeConstraint {
    Arguments(Vec<TypeId>, TypeId, Vec<Location>, Location), // Arguments(args_types, fun_type, args_loc, call_loc)
    Equality(TypeId, TypeId, Location),
    Field(TypeId, TypeId, String, Location), // a: t_1, b: t_2 => a.b: t_2
    Included(TypeId, TypeId, Location),      // Included(t_1, t_2) <=> t_1 ⊂ y_2
    Return(TypeId, TypeId, Location),        // Return(fun_type, returned_type)
    StructLiteral {
        struct_t_id: TypeId,
        fields: Vec<FieldContstraint>, // ident, t_id, loc
        loc: Location,
    },
}

pub type FieldContstraint = (String, TypeId, Location);

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

    pub fn len(&self) -> usize {
        self.constraints.len()
    }
}

pub struct TypeStore {
    types: Vec<Type>, // We may want to switch to a HashMap to get globally unique t_id
}

impl TypeStore {
    pub fn new() -> TypeStore {
        // Padding types to compensate for TypeVarStore default types
        let mut types = Vec::new();
        for _ in 0..NB_DEFAULT_T_ID {
            types.push(Type::Bug)
        }
        types[id::T_ID_BOOL] = Type::Bool; // This one is actually used as a result value
        TypeStore { types }
    }

    pub fn get(&self, id: TypeId) -> &Type {
        &self.types[id]
    }

    pub fn put(&mut self, t: Type) {
        self.types.push(t);
    }

    pub fn _len(&self) -> usize {
        self.types.len()
    }
}

// The store takes the responsibility for sorting type variable's candidates
pub struct TypeVarStore {
    types: Vec<TypeVariable>,
}

pub struct TypeVariable {
    pub loc: Location,
    pub types: Vec<Type>,
}

impl TypeVarStore {
    pub fn new() -> TypeVarStore {
        let mut store = TypeVarStore { types: Vec::new() };

        // If you need to change insertion order, update T_ID constants accordingly
        // Tests are provided at the bottom of the file to ensure good ordering
        // If you need to add an item, create a new constant T_ID_SOMETHING and update
        // TypeStore::new method accordingly, tests will check the good initialization.
        store.fresh(Location::dummy(), vec![Type::Bug]);
        store.fresh(Location::dummy(), vec![Type::Bool]);
        store.fresh(Location::dummy(), vec![Type::I32, Type::I64]);
        store.fresh(Location::dummy(), vec![Type::F32, Type::F64]);
        store.fresh(
            Location::dummy(),
            vec![Type::F32, Type::F64, Type::I32, Type::I64],
        );
        store.fresh(
            Location::dummy(),
            vec![Type::Bool, Type::F32, Type::F64, Type::I32, Type::I64],
        );
        store.fresh(Location::dummy(), vec![Type::Unit]);

        store
    }

    pub fn get(&self, id: TypeId) -> &TypeVariable {
        &self.types[id]
    }

    pub fn replace(&mut self, id: TypeId, new_types: Vec<Type>) {
        self.types[id].types = new_types;
    }

    pub fn fresh(&mut self, loc: Location, mut candidate: Vec<Type>) -> TypeId {
        candidate.sort_unstable(); // Ensure sorted elements
        let id = self.types.len();
        self.types.push(TypeVariable {
            loc,
            types: candidate,
        });
        id
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }
}

impl<'a> IntoIterator for &'a TypeVarStore {
    type Item = <std::slice::Iter<'a, TypeVariable> as IntoIterator>::Item;
    type IntoIter = <std::slice::Iter<'a, TypeVariable> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        let c = &self.types[NB_DEFAULT_T_ID..];
        c.into_iter()
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

impl IntoIterator for ConstraintStore {
    type Item = TypeConstraint;
    type IntoIter = std::vec::IntoIter<TypeConstraint>;

    fn into_iter(self) -> Self::IntoIter {
        let c = self.constraints;
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
            Type::Bug => write!(f, "bug"),
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
            Type::Struct(id) => write!(f, "struct #{}", id),
        }
    }
}

impl fmt::Display for TypeStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("TypeStore {\n");
        store.push_str("  t_id - type\n\n");
        let mut t_id = NB_DEFAULT_T_ID;
        for t in &self.types[NB_DEFAULT_T_ID..] {
            store.push_str(&format!("  {:>4} - {}\n", t_id, t));
            t_id += 1;
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}

impl fmt::Display for TypeVarStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("TypeVarStore {\n");
        store.push_str("  t_id - candidates\n\n");
        for (idx, t) in self.types.iter().enumerate() {
            let candidates = t
                .types
                .iter()
                .map(|c| format!("{}", c))
                .collect::<Vec<String>>()
                .join(" | ");
            store.push_str(&format!("  {:>4} - {}\n", idx, candidates));
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
                TypeConstraint::Equality(t_1, t_2, _) => {
                    store.push_str(&format!("  {:>4} = {:>4}\n", t_1, t_2))
                }
                TypeConstraint::Included(t_1, t_2, _) => {
                    store.push_str(&format!("  {:>4} ⊂ {:>4}\n", t_1, t_2))
                }
                TypeConstraint::Return(fun_t, ret_t, _) => {
                    store.push_str(&format!("  {:>4} -> {:>3}\n", fun_t, ret_t))
                }
                TypeConstraint::Arguments(args_t, fun_t, _, _) => {
                    let args = args_t
                        .iter()
                        .map(|a| format!("{}", a))
                        .collect::<Vec<String>>()
                        .join(", ");
                    store.push_str(&format!("  {:>4} λ {}\n", fun_t, args))
                }
                TypeConstraint::Field(obj_t, field_t, ref field, _) => {
                    store.push_str(&format!("  {:>4} .{} {:>3}\n", obj_t, field, field_t))
                }
                TypeConstraint::StructLiteral {
                    struct_t_id,
                    fields,
                    ..
                } => {
                    for (ref field, t_id, _) in fields {
                        store.push_str(&format!("  {:>4} .{} {:>3}\n", struct_t_id, field, t_id))
                    }
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
    fn built_in_types_id() {
        // Check that Type IDs correspond to the expected type candidates
        let store = TypeVarStore::new();
        assert_eq!(vec![Type::Bug], store.get(id::_T_ID_BUG).types);

        assert_eq!(vec![Type::Bool], store.get(id::T_ID_BOOL).types);

        let mut v = vec![Type::I32, Type::I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_INTEGER).types);

        let mut v = vec![Type::F32, Type::F64, Type::I32, Type::I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_NUMERIC).types);

        let mut v = vec![Type::Bool, Type::F32, Type::F64, Type::I32, Type::I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_BASIC).types);
    }

    #[test]
    fn type_store_init() {
        // Both stores should be initialized with the same number of elements
        let type_var_store = TypeVarStore::new();
        let type_store = TypeStore::new();

        assert_eq!(type_var_store.len(), type_store._len())
    }
}
