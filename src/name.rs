use crate::error::{ErrorHandler, Location};
use crate::parse;
use std::fmt;

pub struct Program {
    pub funs: Vec<parse::Function>,
    pub names: NameStore,
    pub types: TypeVarStore,
    pub constraints: Vec<TypeConstraint>,
}

pub struct NameStore {
    names: Vec<Name>,
}

impl NameStore {
    fn new() -> NameStore {
        NameStore { names: Vec::new() }
    }

    fn get(&self, id: usize) -> &Name {
        &self.names[id]
    }

    fn fresh(&mut self, name: String, loc: Location, t_id: usize) -> usize {
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
        let mut store = String::from("Names: {\n");
        for name in self.names.iter() {
            store.push_str("    ");
            store.push_str(&name.name);
            store.push_str("\n");
        }
        store.push_str("}");
        write!(f, "{}", store)
    }
}

pub struct Name {
    id: usize,
    name: String,
    loc: Location,
    t_id: usize,
}

pub struct TypeVarStore {
    types: Vec<TypeVariable>,
}

impl TypeVarStore {
    fn new() -> TypeVarStore {
        TypeVarStore { types: Vec::new() }
    }

    fn get(&self, id: usize) -> &TypeVariable {
        &self.types[id]
    }

    fn fresh(&mut self, candidate: Vec<Type>) -> usize {
        let id = self.types.len();
        let tv = TypeVariable {
            id: id,
            candidate: candidate,
        };
        self.types.push(tv);
        id
    }
}

pub enum Type {
    I32,
    I64,
    F32,
    F64,
    Fun(Vec<Type>, Vec<Type>),
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f364"),
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

pub struct TypeVariable {
    id: usize,
    candidate: Vec<Type>,
}

pub enum TypeConstraint {
    Equality(usize, usize),
}

pub struct ResolverState {
    names: NameStore,
    types: TypeVarStore,
    constraints: Vec<TypeConstraint>,
}

pub struct NameResolver {
    error_handler: ErrorHandler,
}

impl NameResolver {
    pub fn new() -> NameResolver {
        NameResolver {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn resolve(&mut self, funs: Vec<parse::Function>) -> Program {
        let mut state = ResolverState {
            names: NameStore::new(),
            types: TypeVarStore::new(),
            constraints: Vec::new(),
        };

        self.register_functions(&funs, &mut state);

        Program {
            funs: funs,
            names: state.names,
            types: state.types,
            constraints: state.constraints,
        }
    }

    fn register_functions(&mut self, funs: &Vec<parse::Function>, state: &mut ResolverState) {
        for fun in funs {
            let mut params = Vec::new();
            for param in fun.params.iter() {
                if let Some(t) = &param.t {
                    if let Some(known_t) = check_built_in_type(&t) {
                        params.push(known_t);
                    } else {
                        self.error_handler
                            .report(param.loc, "Unknown parameter type");
                    }
                } else {
                    self.error_handler
                        .report_internal(param.loc, "No type associated to function parameter");
                }
            }

            let mut results = Vec::new();
            if let Some((t, loc)) = &fun.result {
                if let Some(known_t) = check_built_in_type(&t) {
                    results.push(known_t);
                } else {
                    self.error_handler.report(*loc, "Unknown result type");
                }
            }

            let t_id = state.types.fresh(vec![Type::Fun(params, results)]);
            state.names.fresh(
                fun.ident.clone(),
                Location {
                    len: 0,
                    line: 0,
                    pos: 0,
                },
                t_id,
            );
        }
    }
}

fn check_built_in_type(t: &str) -> Option<Type> {
    match t {
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "f32" => Some(Type::F32),
        "f64" => Some(Type::F64),
        _ => None,
    }
}
