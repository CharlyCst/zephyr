use crate::error::{ErrorHandler, Location};
use crate::parse;
use std::collections::HashMap;
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
        let mut store = String::from("NameStore {\n");
        store.push_str("    id ~ t_id - name\n\n");
        for (idx, name) in self.names.iter().enumerate() {
            store.push_str("  ");
            store.push_str(&format!("{:>4} ~ {:>4} - {:} ", idx, name.t_id, name.name));
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

impl fmt::Display for TypeVarStore {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut store = String::from("TypeVarStore {\n");
        store.push_str("  t_id - candidates\n\n");
        for (idx, t) in self.types.iter().enumerate() {
            let candidates = t
                .candidate
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
    contexts: Vec<HashMap<String, usize>>,
    constraints: Vec<TypeConstraint>,
}

impl ResolverState {
    pub fn new() -> ResolverState {
        let contexts = vec![HashMap::new()];
        ResolverState {
            names: NameStore::new(),
            types: TypeVarStore::new(),
            contexts: contexts,
            constraints: Vec::new(),
        }
    }

    pub fn new_scope(&mut self) {
        self.contexts.push(HashMap::new());
    }

    pub fn exit_scope(&mut self) {
        self.contexts.pop();
    }

    pub fn declare(
        &mut self,
        ident: String,
        type_candidates: Vec<Type>,
        loc: Location,
    ) -> Result<(), Location> {
        if let Some(n) = self.find_in_context(&ident) {
            return Err(n.loc);
        }

        let ident_key = ident.clone();
        let t_id = self.types.fresh(type_candidates);
        let id = self.names.fresh(ident, loc, t_id);
        self.add_in_context(ident_key, id);
        Ok(())
    }

    fn find_in_context(&self, ident: &str) -> Option<&Name> {
        for ctx in self.contexts.iter().rev() {
            match ctx.get(ident) {
                Some(id) => return Some(self.names.get(*id)),
                None => (),
            }
        }
        None
    }

    fn add_in_context(&mut self, name: String, id: usize) {
        match self.contexts.last_mut() {
            Some(ctx) => ctx.insert(name, id),
            None => panic!("Empty context in name resolution"),
        };
    }
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
        let mut state = ResolverState::new();

        self.register_functions(&funs, &mut state);

        for fun in &funs {
            self.resolve_function(fun, &mut state);
        }

        Program {
            funs: funs,
            names: state.names,
            types: state.types,
            constraints: state.constraints,
        }
    }

    fn resolve_function(&mut self, fun: &parse::Function, state: &mut ResolverState) {
        state.new_scope();

        for param in &fun.params {
            let t = match get_var_type(&param) {
                Some(t) => t,
                None => {
                    self.error_handler
                        .report(param.loc, "Missing or unrecognized type");
                    continue;
                }
            };

            if let Err(decl_loc) = state.declare(param.ident.clone(), t, param.loc) {
                let error = format!(
                    "Name {} already defined in current context at line {}",
                    fun.ident, decl_loc.line
                );
                self.error_handler.report(fun.loc, &error);
            }
        }

        state.exit_scope();
    }

    // must be called first to bring all global function declarations in scope
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

            if let Err(decl_loc) =
                state.declare(fun.ident.clone(), vec![Type::Fun(params, results)], fun.loc)
            {
                let error = format!(
                    "Function {} already declared line {}",
                    fun.ident, decl_loc.line
                );
                self.error_handler.report(fun.loc, &error);
            }
        }
    }
}

fn get_var_type(var: &parse::Variable) -> Option<Vec<Type>> {
    if let Some(ref t) = var.t {
        if let Some(known_t) = check_built_in_type(&t) {
            Some(vec![known_t])
        } else {
            None
        }
    } else {
        None
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
