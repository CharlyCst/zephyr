use super::hir::{
    FunctionType as HIRFunctionType, ScalarType as HIRScalarType, TupleType as HIRTupleType,
    Type as HIRType,
};
use super::names;
use crate::ctx::ModuleDeclarations;
use crate::error::Location;
use crate::hir::Package;

use std::collections::HashMap;
use std::fmt;

pub use names::{DataId, StructId, TypeId};

pub type TypeVarId = usize;

pub mod id {
    use super::TypeVarId;
    pub const _T_ID_BUG: TypeVarId = 0;
    pub const T_ID_BOOL: TypeVarId = 1;
    pub const T_ID_INTEGER: TypeVarId = 2;
    pub const T_ID_FLOAT: TypeVarId = 3;
    pub const T_ID_NUMERIC: TypeVarId = 4;
    pub const T_ID_BASIC: TypeVarId = 5;
    pub const T_ID_UNIT: TypeVarId = 6;
}
// Please update this const when adding/removing default T_ID
const NB_DEFAULT_T_ID: usize = 7;

// ————————————————————————————————— Types —————————————————————————————————— //

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum Type {
    Scalar(ScalarType),
    Fun(FunctionType),
    Tuple(TupleType),
    Struct(StructId),
    Bug, // Used to signal that an error occurred somewhere
    Any, // Used as a placeholder, the actual type will be infered
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Ord, PartialOrd)]
pub enum ScalarType {
    I32,
    I64,
    F32,
    F64,
    Bool,
    Null,
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct TupleType(pub Vec<Type>);

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct FunctionType {
    pub params: Vec<Type>,
    pub ret: Box<Type>,
}

pub const I32: Type = Type::Scalar(ScalarType::I32);
pub const I64: Type = Type::Scalar(ScalarType::I64);
pub const F32: Type = Type::Scalar(ScalarType::F32);
pub const F64: Type = Type::Scalar(ScalarType::F64);
pub const BOOL: Type = Type::Scalar(ScalarType::Bool);
pub const NULL: Type = Type::Scalar(ScalarType::Null);
pub const BUG: Type = Type::Bug;
#[allow(dead_code)]
pub const ANY: Type = Type::Any;

impl Type {
    /// Tries to lower a type to HIR. Will return an error if the type contains `any` or `null`.
    pub fn lower(&self) -> Result<HIRType, String> {
        match self {
            Type::Scalar(s) => Ok(HIRType::Scalar(s.lower())),
            Type::Fun(fun) => Ok(HIRType::Fun(fun.lower()?)),
            Type::Tuple(tup) => Ok(HIRType::Tuple(tup.lower()?)),
            Type::Struct(s_id) => Ok(HIRType::Struct(*s_id)),
            Type::Bug | Type::Any => Err(format!("Could not lower type '{}'", self)),
        }
    }

    /// Tries to lower a type to an HIR scalar. Will return an error if the type is not a valid
    /// scalar.
    pub fn lower_to_scalar(&self) -> Result<HIRScalarType, String> {
        match self {
            Type::Scalar(s) => Ok(s.lower()),
            _ => Err(format!("Could not lower type '{}' to a scalar", self)),
        }
    }

    /// Ties to lower a type an HIR function. Will return an error if the type is not a valid
    /// function.
    pub fn lower_to_fun(&self) -> Result<HIRFunctionType, String> {
        match self {
            Type::Fun(f) => Ok(f.lower()?),
            _ => Err(format!("Could not lower type '{}' to a function", self)),
        }
    }
}

impl ScalarType {
    pub fn lower(&self) -> HIRScalarType {
        match self {
            ScalarType::I32 => HIRScalarType::I32,
            ScalarType::I64 => HIRScalarType::I64,
            ScalarType::F32 => HIRScalarType::F32,
            ScalarType::F64 => HIRScalarType::F64,
            ScalarType::Bool => HIRScalarType::Bool,
            ScalarType::Null => HIRScalarType::Null,
        }
    }
}

impl FunctionType {
    pub fn lower(&self) -> Result<HIRFunctionType, String> {
        let mut params = Vec::with_capacity(self.params.len());
        for param in &self.params {
            params.push(param.lower()?);
        }
        Ok(HIRFunctionType {
            params,
            ret: Box::new(self.ret.lower()?),
        })
    }
}

impl TupleType {
    pub fn lower(&self) -> Result<HIRTupleType, String> {
        let mut tup = Vec::with_capacity(self.0.len());
        for t in &self.0 {
            tup.push(t.lower()?)
        }
        Ok(HIRTupleType(tup))
    }
}

// ———————————————————————————————— Program ————————————————————————————————— //

pub struct TypedProgram {
    pub funs: Vec<names::Function>,
    pub imports: Vec<names::Imports>,
    pub data: names::DataStore,
    pub types: names::TypeStore,
    pub structs: names::StructStore,
    pub fun_types: HashMap<names::FunId, names::TypeId>,
    pub names: names::NameStore,
    pub type_vars: TypeVarTypes,
    pub pub_decls: ModuleDeclarations,
    pub package: Package,
}

pub enum TypeConstraint {
    Equality(TypeVarId, TypeVarId, Location), // t_1 and t_2 have the same types
    Is(TypeVarId, Type, Location),            // t_1 is t_2
    Included(TypeVarId, TypeVarId, Location), // Included(t_1, t_2) <=> t_1 ⊂ y_2
    Field(TypeVarId, TypeVarId, String, Location), // a: t_1, b: t_2 => a.b: t_2
    Arguments {
        args_t_id: Vec<(TypeVarId, Location)>,
        fun_t_id: TypeVarId,
        loc: Location,
    },
    Return {
        fun_t_id: TypeVarId,
        ret_t_id: TypeVarId,
        loc: Location,
    },
    StructLiteral {
        struct_t_id: TypeVarId,
        fields: Vec<FieldContstraint>,
        loc: Location,
    },
    TupleLiteral {
        tuple_t_id: TypeVarId,
        values_t_ids: Vec<(TypeVarId, Location)>,
        loc: Location,
    },
}

pub type FieldContstraint = (String, TypeVarId, Location); // ident, t_id, loc

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

pub struct TypeVarTypes(HashMap<TypeVarId, Type>);

impl TypeVarTypes {
    pub fn new() -> Self {
        let mut types = HashMap::new();
        types.insert(id::T_ID_BOOL, BOOL);
        types.insert(id::T_ID_UNIT, NULL);
        TypeVarTypes(types)
    }

    pub fn insert(&mut self, var_id: TypeVarId, t: Type) {
        self.0.insert(var_id, t);
    }

    pub fn get(&self, var_id: &TypeVarId) -> Option<&Type> {
        self.0.get(var_id)
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
        store.fresh(Location::dummy(), vec![BUG]);
        store.fresh(Location::dummy(), vec![BOOL]);
        store.fresh(Location::dummy(), vec![I32, I64]);
        store.fresh(Location::dummy(), vec![F32, F64]);
        store.fresh(Location::dummy(), vec![F32, F64, I32, I64]);
        store.fresh(Location::dummy(), vec![BOOL, F32, F64, I32, I64]);
        store.fresh(Location::dummy(), vec![NULL]);

        store
    }

    pub fn get(&self, id: TypeVarId) -> &TypeVariable {
        &self.types[id]
    }

    pub fn replace(&mut self, id: TypeVarId, new_types: Vec<Type>) {
        self.types[id].types = new_types;
    }

    pub fn fresh(&mut self, loc: Location, mut candidate: Vec<Type>) -> TypeVarId {
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
    type Item = (&'a TypeVariable, TypeVarId);
    type IntoIter = std::iter::Zip<std::slice::Iter<'a, TypeVariable>, std::ops::Range<usize>>;

    fn into_iter(self) -> Self::IntoIter {
        let c = &self.types[NB_DEFAULT_T_ID..];
        let ids = NB_DEFAULT_T_ID..(NB_DEFAULT_T_ID + c.len());
        c.into_iter().zip(ids)
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

// —————————————————————————————— impl Display —————————————————————————————— //

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Scalar(s) => match s {
                ScalarType::I32 => write!(f, "i32"),
                ScalarType::I64 => write!(f, "i64"),
                ScalarType::F32 => write!(f, "f32"),
                ScalarType::F64 => write!(f, "f64"),
                ScalarType::Bool => write!(f, "bool"),
                ScalarType::Null => write!(f, "null"),
            },
            Type::Any => write!(f, "any"),
            Type::Bug => write!(f, "bug"),
            Type::Fun(FunctionType { params, ret }) => {
                let p_types = params
                    .iter()
                    .map(|param| format!("{}", param))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "fun({}) {}", p_types, ret)
            }
            Type::Tuple(types) => {
                let types = types
                    .0
                    .iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "({})", types)
            }
            Type::Struct(id) => write!(f, "struct #{}", id),
        }
    }
}

impl fmt::Display for TypeVarTypes {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("TypeVarTypes {\n");
        store.push_str("  t_id - type\n\n");
        for (t_id, t) in &self.0 {
            store.push_str(&format!("  {:>4} - {}\n", t_id, t));
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
                TypeConstraint::Is(t_1, t_2, _) => {
                    store.push_str(&format!("  {:>4} is {:>3}\n", t_1, t_2))
                }
                TypeConstraint::Included(t_1, t_2, _) => {
                    store.push_str(&format!("  {:>4} ⊂ {:>4}\n", t_1, t_2))
                }
                TypeConstraint::Field(obj_t, field_t, ref field, _) => {
                    store.push_str(&format!("  {:>4} .{} {:>3}\n", obj_t, field, field_t))
                }
                TypeConstraint::Arguments {
                    args_t_id,
                    fun_t_id,
                    ..
                } => {
                    let args = args_t_id
                        .iter()
                        .map(|(id, _)| format!("{:>3}", id))
                        .collect::<Vec<String>>()
                        .join(",");
                    store.push_str(&format!("  λ{:>3}.  ({})\n", fun_t_id, args))
                }
                TypeConstraint::Return {
                    fun_t_id, ret_t_id, ..
                } => store.push_str(&format!("  λ{:>3} -> {:>3}\n", fun_t_id, ret_t_id)),
                TypeConstraint::StructLiteral {
                    struct_t_id,
                    fields,
                    ..
                } => {
                    for (ref field, t_id, _) in fields {
                        store.push_str(&format!("  {:>4} .{} {:>3}\n", struct_t_id, field, t_id))
                    }
                }
                TypeConstraint::TupleLiteral {
                    tuple_t_id,
                    values_t_ids,
                    ..
                } => {
                    let mut idx = 0;
                    for (t_id, _) in values_t_ids {
                        store.push_str(&format!("  {:>4}._{} {:>3}\n", tuple_t_id, idx, t_id));
                        idx += 1;
                    }
                }
            };
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}

// —————————————————————————————————— Tests ————————————————————————————————— //

#[cfg(test)]
mod tests {
    use super::id;
    use super::*;

    #[test]
    fn built_in_types_id() {
        // Check that Type IDs correspond to the expected type candidates
        let store = TypeVarStore::new();
        assert_eq!(vec![BUG], store.get(id::_T_ID_BUG).types);

        assert_eq!(vec![BOOL], store.get(id::T_ID_BOOL).types);

        let mut v = vec![I32, I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_INTEGER).types);

        let mut v = vec![F32, F64, I32, I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_NUMERIC).types);

        let mut v = vec![BOOL, F32, F64, I32, I64];
        v.sort_unstable();
        assert_eq!(v, store.get(id::T_ID_BASIC).types);
    }

    #[test]
    fn type_store_init() {
        // Both stores should be initialized with the same number of elements
        let type_var_store = TypeVarStore::new();
        let type_store = TypeVarTypes::new();

        assert_eq!(type_var_store.len(), type_store.0.len())
    }
}
