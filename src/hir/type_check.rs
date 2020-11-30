use super::names::{Declaration, Function, NameStore, ResolvedProgram};
use super::types::id::{T_ID_FLOAT, T_ID_INTEGER};
use super::types::{ConstraintStore, Type, TypeConstraint, TypeStore, TypeVarStore, TypedProgram};
use crate::error::{ErrorHandler, Location};

use std::cmp::Ordering;
use std::collections::HashMap;

enum Progress {
    Some,
    None,
    Error,
}

pub struct TypeChecker<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> TypeChecker<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> TypeChecker {
        TypeChecker { err: error_handler }
    }

    /// Type check a ResolvedProgram.
    pub fn check(&mut self, prog: ResolvedProgram) -> TypedProgram {
        let mut type_vars = prog.types;
        let mut constraints = prog.constraints;
        let mut progress = true;

        while constraints.len() > 0 && progress {
            let mut new_constraints = ConstraintStore::new();
            progress = false;
            for constr in &constraints {
                match self.apply_constr(constr, &mut type_vars, &mut new_constraints) {
                    Progress::Some => progress = true,
                    Progress::None => (),
                    Progress::Error => (),
                }
            }
            constraints = new_constraints;
        }

        let store = self.build_store(&type_vars);
        let pub_decls = self.get_pub_decls(&store, &prog.names, &prog.funs);

        TypedProgram {
            funs: prog.funs,
            imported: prog.imported,
            names: prog.names,
            types: store,
            pub_decls,
            package: prog.package,
        }
    }

    /// Build a `TypeStore` from a `TypeVarStore`, should be called once constraints have been
    /// satisfyied.
    fn build_store(&mut self, var_store: &TypeVarStore) -> TypeStore {
        let integers = var_store.get(T_ID_INTEGER);
        let floats = var_store.get(T_ID_FLOAT);
        let mut store = TypeStore::new();
        for var in var_store {
            if var.types.len() == 1 {
                store.put(var.types[0].clone())
            } else if var.types.len() == 0 {
                // TODO: add location
                self.err.report(
                    var.loc,
                    String::from("Variable type does not satisfy constraints"),
                )
            } else {
                // Choose arbitrary type if applicable
                if var.types == integers.types {
                    store.put(Type::I64);
                } else if var.types == floats.types {
                    store.put(Type::F64);
                } else {
                    // TODO: improve error handling...
                    println!("{:?}", floats.types);
                    println!("{:?}", var.types);
                    self.err
                        .report(var.loc, String::from("Could not infer type"))
                }
            }
        }

        store
    }

    /// Returns a map of the public types (from public declaration with `pub` keyword).
    fn get_pub_decls(
        &mut self,
        types: &TypeStore,
        names: &NameStore,
        funs: &Vec<Function>,
    ) -> HashMap<String, Declaration> {
        let mut pub_decls = HashMap::new();
        for fun in funs {
            if fun.is_pub {
                let name = names.get(fun.n_id);
                let t = types.get(name.t_id);
                pub_decls.insert(
                    fun.ident.clone(),
                    Declaration::Function {
                        t: t.clone(),
                        fun_id: fun.fun_id,
                    },
                );
            }
        }

        pub_decls
    }

    /// Apply a constraints, it may modify the type variable store `store`.
    /// Progress can be made or not, depending on the state of the store.
    fn apply_constr(
        &mut self,
        constr: &TypeConstraint,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Progress {
        match *constr {
            TypeConstraint::Equality(t_id_1, t_id_2, loc) => {
                self.constr_equality(t_id_1, t_id_2, loc, store, constraints)
            }
            TypeConstraint::Included(t_id_1, t_id_2, loc) => {
                self.constr_included(t_id_1, t_id_2, loc, store, constraints)
            }
            TypeConstraint::Return(t_id_fun, t_id, loc) => {
                self.constr_return(t_id_fun, t_id, loc, store, constraints)
            }
            TypeConstraint::Arguments(ref args_t_id, fun_t_id, ref locs, loc) => {
                self.constr_arguments(&args_t_id, fun_t_id, &locs, loc, store, constraints)
            }
        }
    }

    fn constr_equality(
        &mut self,
        t_id_1: usize,
        t_id_2: usize,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Progress {
        let t_1 = &store.get(t_id_1).types;
        let t_2 = &store.get(t_id_2).types;
        let c = TypeConstraint::Equality(t_id_1, t_id_2, loc);

        // Special cases
        if t_1.len() > 0 && t_1[0] == Type::Any {
            if t_2.len() > 0 && t_2[0] == Type::Any {
                constraints.add(c);
                return Progress::None;
            }
            let t = t_2.clone();
            store.replace(t_id_1, t);
            constraints.add(c);
            return Progress::Some;
        } else if t_2.len() > 0 && t_2[0] == Type::Any {
            let t = t_1.clone();
            store.replace(t_id_2, t);
            constraints.add(c);
            return Progress::Some;
        }

        // Can not infer types
        if t_1.len() == 0 || t_2.len() == 0 {
            self.err.report(
                loc,
                String::from("Could not infer a type satisfying constraints"),
            );
            return Progress::Error;
        }

        let mut t = Vec::new();
        let mut idx_1 = 0;
        let mut idx_2 = 0;
        let mut progress = false || t_1.len() != t_2.len();
        while idx_1 < t_1.len() && idx_2 < t_2.len() {
            match t_1[idx_1].cmp(&t_2[idx_2]) {
                Ordering::Less => {
                    idx_1 += 1;
                    progress = true;
                }
                Ordering::Greater => {
                    idx_2 += 1;
                    progress = true;
                }
                Ordering::Equal => {
                    t.push(t_1[idx_1].clone());
                    idx_1 += 1;
                    idx_2 += 1;
                }
            }
        }

        if t.len() != 1 {
            constraints.add(c);
        }
        store.replace(t_id_1, t.clone());
        store.replace(t_id_2, t);
        if progress {
            Progress::Some
        } else {
            Progress::None
        }
    }

    fn constr_included(
        &mut self,
        t_id_1: usize,
        t_id_2: usize,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Progress {
        let t_1 = &store.get(t_id_1).types;
        let t_2 = &store.get(t_id_2).types;
        let c = TypeConstraint::Included(t_id_1, t_id_2, loc);

        // Special case
        if t_1.len() > 0 && t_1[0] == Type::Any {
            if t_2.len() > 0 && t_2[0] == Type::Any {
                constraints.add(c);
                return Progress::None;
            }
            let t = t_2.clone();
            store.replace(t_id_1, t);
            return Progress::Some;
        }

        let mut t = Vec::new();
        let mut idx_1 = 0;
        let mut idx_2 = 0;
        let mut progress = false;
        while idx_1 < t_1.len() && idx_2 < t_2.len() {
            match t_1[idx_1].cmp(&t_2[idx_2]) {
                Ordering::Less => {
                    idx_1 += 1;
                    progress = true;
                }
                Ordering::Greater => {
                    idx_2 += 1;
                }
                Ordering::Equal => {
                    t.push(t_1[idx_1].clone());
                    idx_1 += 1;
                    idx_2 += 1;
                }
            }
        }

        if t.len() == 0 {
            self.err.report(
                loc,
                String::from("Could not infer a type satisfying constraints"),
            );
        }

        store.replace(t_id_1, t);
        if progress {
            Progress::Some
        } else {
            Progress::None
        }
    }

    fn constr_return(
        &mut self,
        t_id_fun: usize,
        t_id: usize,
        _loc: Location,
        store: &mut TypeVarStore,
        _constraints: &mut ConstraintStore,
    ) -> Progress {
        let t_fun = store.get(t_id_fun);
        let ts = store.get(t_id);

        if t_fun.types.len() != 1 {
            self.err.report_internal(
                t_fun.loc,
                String::from("Return type constraint with ambiguous fun type"),
            );
            return Progress::Error;
        }

        let ret_t = match &t_fun.types[0] {
            Type::Fun(_, ret_t) => ret_t,
            _ => {
                self.err.report_internal(
                    t_fun.loc,
                    String::from("Return type constraint used on a non function type"),
                );
                return Progress::Error;
            }
        };

        if ret_t.len() == 0 {
            if ts.types.len() == 0 {
                return Progress::Some;
            } else if ts.types[0] == Type::Any {
                // TODO: do we accept type any?
                return Progress::Some;
            } else {
                self.err
                    .report(t_fun.loc, String::from("Function returns no value"));
                return Progress::Error;
            }
        } else if ret_t.len() != 1 {
            self.err.report_internal(
                t_fun.loc,
                String::from("Function returning multiple values are not yet supported"),
            );
            return Progress::Error;
        }

        let ret_t = &ret_t[0];
        for t in &ts.types {
            if t == ret_t || *t == Type::Any {
                let typ = vec![ret_t.clone()];
                store.replace(t_id, typ);
                return Progress::Some;
            }
        }

        self.err
            .report(ts.loc, String::from("Return value has wrong type"));
        return Progress::Error;
    }

    // TODO
    fn constr_arguments(
        &mut self,
        args_t_id: &Vec<usize>,
        fun_t_id: usize,
        locs: &Vec<Location>, // Arguments location
        loc: Location,        // Function location
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Progress {
        let t_fun = store.get(fun_t_id);

        if t_fun.types.len() != 1 {
            self.err.report(loc, String::from("Call a non-function"));
            return Progress::Error;
        }

        if let Type::Fun(ref f_args, _) = t_fun.types[0].clone() {
            if f_args.len() != args_t_id.len() {
                self.err.report(
                    loc,
                    format!(
                        "Expected {} arguments, got {}",
                        f_args.len(),
                        args_t_id.len()
                    ),
                );
                return Progress::Error;
            }
            if f_args.len() != locs.len() {
                self.err.report_internal(
                    loc,
                    format!("Expected {} locations but got {}", f_args.len(), locs.len()),
                )
            }
            for ((f_arg, t_arg), loc) in f_args.iter().zip(args_t_id.iter()).zip(locs.iter()) {
                let candidate = vec![f_arg.clone()];
                let fresh_t_id = store.fresh(*loc, candidate);
                constraints.add(TypeConstraint::Equality(*t_arg, fresh_t_id, *loc));
            }
            Progress::Some
        } else {
            self.err.report(loc, String::from("Call a non-function"));
            Progress::Error
        }
    }
}
