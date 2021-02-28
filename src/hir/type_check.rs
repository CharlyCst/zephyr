// TODO: remove that
#![allow(dead_code)]
use super::hir;
use super::hir::ScalarType;
use super::names::{
    Function, Imports, ResolvedProgram, StructStore, TypeDeclaration, ValueDeclaration,
};
use super::store::StructId;
use super::types;
use super::types::id::{T_ID_FLOAT, T_ID_INTEGER};
use super::types::{
    ConstraintStore, FieldContstraint, FunctionType, TupleType, Type, TypeConstraint, TypeVarId,
    TypeVarStore, TypeVarTypes, TypedProgram,
};
use crate::arena::Arena;
use crate::ctx::{Ctx, ModId, ModuleDeclarations};
use crate::error::{ErrorHandler, Location};

use std::cmp::Ordering;
use std::collections::HashMap;

/// Indicates if some progress has been made.
#[derive(Eq, PartialEq)]
enum Progress {
    Some,
    None,
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct TypeVar(usize);

enum NewTypeConstraint {
    Access {
        object: TypeVar,
        field: TypeVar,
        field_name: String,
        loc: Location,
    },
    Call {
        fun: TypeVar,
        args: Vec<TypeVar>,
        loc: Location,
    },
    Return {
        fun: TypeVar,
        ret: TypeVar,
        loc: Location,
    },
}

/// A type placeholder, the role of the type checker is to infer all the type variables given a set
/// of constraints.
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum Ty {
    Base(ScalarType),
    Var(TypeVar),
    Composite(CompositeKind, Vec<TypeVar>),
    OneOf(TypeVar, Vec<ScalarType>),
}

/// The kind of an applicative type.
#[derive(Hash, Eq, PartialEq, Clone, Debug)]
enum CompositeKind {
    Tuple,
    Fun,
    Struct(StructId),
}

pub struct TyStore {
    arena: Arena<Ty>,
}

impl TyStore {
    pub fn new() -> Self {
        Self {
            arena: Arena::new(),
        }
    }

    /// Store an object in the store and return a reference to it.
    fn store(&self, ty: Ty) -> &Ty {
        self.arena.alloc(ty)
    }
}

/// A substitution is a function that maps a Ty to another Ty, it is implemented upon the
/// union-find algorithm.
struct Substitution<'ty> {
    subs: HashMap<TypeVar, &'ty Ty>,
    store: &'ty TyStore,
}

impl<'ty> Substitution<'ty> {
    pub fn new(store: &'ty TyStore) -> Self {
        Self {
            subs: HashMap::new(),
            store,
        }
    }

    /// Insert a new mapping.
    fn insert(&mut self, t_var: TypeVar, ty: Ty) {
        let ty = self.store.store(ty);
        self.subs.insert(t_var, ty);
    }

    /// Insert a new mapping referencing an already registered Ty.
    fn insert_ref(&mut self, t_var: TypeVar, ty: &'ty Ty) {
        self.subs.insert(t_var, ty);
    }

    /// Return the substitution for `t_var`.
    fn substitute(&mut self, t_var: TypeVar) -> &'ty Ty {
        let ty = self.subs.get(&t_var);
        match ty {
            None => {
                // Insert the Ty::Var
                let ty = self.store.store(Ty::Var(t_var));
                self.subs.insert(t_var, ty);
                self.subs.get(&t_var).unwrap()
            }
            Some(ty) => match &ty {
                Ty::Var(next_t_var) => {
                    if t_var == *next_t_var {
                        // Isn't that case an error?
                        ty
                    } else {
                        let next_t_var = *next_t_var;
                        let new_ty = self.substitute(next_t_var);
                        self.subs.insert(t_var, new_ty);
                        new_ty
                    }
                }
                _ => ty,
            },
        }
    }
}

pub struct NewTypeChecker<'ctx, 'ty> {
    ctx: &'ctx Ctx,
    type_var_counter: usize,
    constraints: Vec<NewTypeConstraint>,
    subs: Substitution<'ty>,

    // Scalar types
    t_i32: TypeVar,
    t_i64: TypeVar,
    t_f32: TypeVar,
    t_f64: TypeVar,
    t_bool: TypeVar,
    t_null: TypeVar,
}

impl<'ctx, 'ty> NewTypeChecker<'ctx, 'ty> {
    pub fn new(ctx: &'ctx Ctx, store: &'ty TyStore) -> Self {
        let mut subs = Substitution::new(store);
        let t_i32 = TypeVar(0);
        let t_i64 = TypeVar(1);
        let t_f32 = TypeVar(2);
        let t_f64 = TypeVar(3);
        let t_bool = TypeVar(4);
        let t_null = TypeVar(5);
        subs.insert(t_i32, Ty::Base(ScalarType::I32));
        subs.insert(t_i64, Ty::Base(ScalarType::I64));
        subs.insert(t_f32, Ty::Base(ScalarType::F32));
        subs.insert(t_f64, Ty::Base(ScalarType::F64));
        subs.insert(t_bool, Ty::Base(ScalarType::Bool));
        subs.insert(t_null, Ty::Base(ScalarType::Null));
        Self {
            ctx,
            subs,
            t_i32,
            t_i64,
            t_f32,
            t_f64,
            t_bool,
            t_null,
            type_var_counter: 6, // !IMPORTANT: must be (strictly) higher than highest scalar t_var
            constraints: Vec::new(),
        }
    }

    /// Return a fresh type variable.
    pub fn fresh(&mut self) -> TypeVar {
        let t_var = TypeVar(self.type_var_counter);
        self.type_var_counter += 1;
        t_var
    }

    /// Return a type variable corresponding to a scalar.
    pub fn scalar(&self, t: ScalarType) -> TypeVar {
        match t {
            ScalarType::I32 => self.t_i32,
            ScalarType::I64 => self.t_i64,
            ScalarType::F32 => self.t_f32,
            ScalarType::F64 => self.t_f64,
            ScalarType::Bool => self.t_bool,
            ScalarType::Null => self.t_null,
        }
    }

    /// Gives a type to a type variable, will raise an error if the type variable already has a
    /// different type.
    pub fn set_type(
        &mut self,
        t_var: TypeVar,
        t: ScalarType,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        let t_var_t = self.fresh();
        self.subs.insert(t_var_t, Ty::Base(t));
        let _ = self.unify_var_var(t_var, t_var_t, err, loc);
    }

    /// Apply a 'one of' type constraint to `t_var`, that is `t_var` will be restricted to one of
    /// the types in `types`.
    pub fn set_one_of(
        &mut self,
        t_var: TypeVar,
        mut types: Vec<ScalarType>,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        let t_var_ts = self.fresh();
        types.sort();
        self.subs.insert(t_var_ts, Ty::OneOf(t_var_ts, types));
        let _ = self.unify_var_var(t_var, t_var_ts, err, loc);
    }

    /// Recursively apply all remaining constraints, will return an error if an unification failed
    /// or if some remaining constraint can't make further progress.
    pub fn type_check(&mut self, err: &mut ErrorHandler) -> Result<(), ()> {
        let mut progress;
        let mut error = false;
        loop {
            progress = Progress::None;
            let mut constraints = Vec::new();
            std::mem::swap(&mut self.constraints, &mut constraints);
            for constr in constraints {
                let result = match constr {
                    NewTypeConstraint::Access {
                        object,
                        field,
                        field_name,
                        loc,
                    } => self.unify_field(object, field, field_name, err, loc),
                    NewTypeConstraint::Call { fun, args, loc } => {
                        self.unify_call(fun, args, err, loc)
                    }
                    NewTypeConstraint::Return { fun, ret, loc } => {
                        self.unify_return(fun, ret, err, loc)
                    }
                };
                match result {
                    Ok(Progress::Some) => progress = Progress::Some,
                    Ok(Progress::None) => {}
                    Err(()) => error = true,
                }
            }

            if progress == Progress::None || self.constraints.len() == 0 {
                break;
            }
        }
        if error || self.constraints.len() > 0 {
            Err(())
        } else {
            Ok(())
        }
    }

    pub fn get_t(&mut self, t_var: TypeVar) -> Result<hir::Type, ()> {
        let ty = self.subs.substitute(t_var);
        match ty {
            Ty::Var(_) => Err(()),
            Ty::Base(t) => Ok(hir::Type::Scalar(*t)),
            Ty::OneOf(_, ts) => {
                // Types are assumed to be sorted!
                if let Some(t) = ts.first() {
                    Ok(hir::Type::Scalar(*t))
                } else {
                    Err(())
                }
            }
            Ty::Composite(kind, ts) => match kind {
                CompositeKind::Struct(s_id) => Ok(hir::Type::Struct(*s_id)),
                CompositeKind::Fun => {
                    let (ret_t_var, param_t_vars) = ts.split_last().ok_or(())?;
                    let ret = Box::new(self.get_t(*ret_t_var)?);
                    let mut params = Vec::with_capacity(param_t_vars.len());
                    for t in param_t_vars {
                        params.push(self.get_t(*t)?);
                    }
                    Ok(hir::Type::Fun(hir::FunctionType { params, ret }))
                }
                CompositeKind::Tuple => {
                    let mut types = Vec::with_capacity(ts.len());
                    for t in ts {
                        types.push(self.get_t(*t)?);
                    }
                    Ok(hir::Type::Tuple(hir::TupleType(types)))
                }
            },
        }
    }

    // ———————————————————————————————— Solver —————————————————————————————————— //

    fn unify_var_var(
        &mut self,
        t_var_1: TypeVar,
        t_var_2: TypeVar,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let ty_1 = self.subs.substitute(t_var_1);
        let ty_2 = self.subs.substitute(t_var_2);
        if ty_1 == ty_2 {
            return Ok(Progress::None);
        }
        match (ty_1, ty_2) {
            (Ty::Var(t_1), ty) => self.unify_var_ty(t_1, ty, err, loc),
            (Ty::Base(t_1), Ty::Base(t_2)) => self.unify_base_base(t_1, t_2, err, loc),
            (Ty::Base(t), Ty::OneOf(t_var, ts)) => {
                self.unify_base_oneof(ty_1, t, t_var, ts, err, loc)
            }
            (Ty::Base(t), Ty::Composite(_, _)) => {
                err.report(loc, format!("Incompatible type: {} and advanced type", t));
                Err(())
            }
            (Ty::OneOf(t_var_1, ts_1), Ty::OneOf(t_var_2, ts_2)) => {
                self.unify_oneof_oneof(*t_var_1, ts_1, *t_var_2, ts_2, err, loc)
            }
            (Ty::OneOf(_, _), Ty::Composite(_, _)) => {
                err.report(loc, String::from("Incompatible types"));
                Err(())
            }
            (Ty::Composite(kind_1, tys_1), Ty::Composite(kind_2, tys_2)) => {
                self.unify_composite_composite(kind_1, tys_1, kind_2, tys_2, err, loc)
            }
            _ => return self.unify_var_var(t_var_2, t_var_1, err, loc),
        }
    }

    fn unify_field(
        &mut self,
        object: TypeVar,
        t_var: TypeVar,
        field_name: String,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let ty_obj = self.subs.substitute(object);
        match ty_obj {
            Ty::Var(_) => {
                // We can't do anything for now, re-insert the constraint
                self.constraints.push(NewTypeConstraint::Access {
                    object,
                    field: t_var,
                    field_name,
                    loc,
                });
                Ok(Progress::None)
            }
            Ty::Base(_) | Ty::OneOf(_, _) => {
                err.report(
                    loc,
                    format!("Can't access field '{}' of basic a type", &field_name),
                );
                Err(())
            }
            Ty::Composite(kind, _) => {
                match kind {
                    CompositeKind::Struct(s_id) => {
                        // TODO: the field can come either from Ctx or StructStore!
                        let field = get_field(*s_id, &field_name, self.ctx, err, loc)?;
                        let t_var_field = self.lift_t(&field.t);
                        self.unify_var_var(t_var, t_var_field, err, loc)
                        // Note: once generics are implemented they should be checked here
                    }
                    CompositeKind::Tuple => {
                        err.report(loc, String::from("Can't access field of a tuple"));
                        Err(())
                    }
                    CompositeKind::Fun => {
                        err.report(loc, String::from("Can't access field of a function"));
                        Err(())
                    }
                }
            }
        }
    }

    fn unify_call(
        &mut self,
        t_var_fun: TypeVar,
        t_var_args: Vec<TypeVar>,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let ty_fun = self.subs.substitute(t_var_fun);
        match ty_fun {
            Ty::Var(_) => {
                // We can't do anything for now, re-insert the constraint
                self.constraints.push(NewTypeConstraint::Call {
                    fun: t_var_fun,
                    args: t_var_args,
                    loc,
                });
                Ok(Progress::None)
            }
            Ty::Composite(CompositeKind::Fun, types) => {
                let n_args = if types.len() > 0 { types.len() - 1 } else { 0 };
                if n_args != t_var_args.len() {
                    err.report(
                        loc,
                        format!(
                            "Expected {} argument{}, got {}",
                            n_args,
                            n_args > 1,
                            t_var_args.len()
                        ),
                    );
                    Err(())
                } else {
                    let mut progress = Progress::None;
                    for (arg_t_var, param_t_var) in t_var_args.iter().zip(types.iter()) {
                        if let Ok(Progress::Some) =
                            self.unify_var_var(*arg_t_var, *param_t_var, err, loc)
                        {
                            progress = Progress::Some;
                        }
                    }
                    Ok(progress)
                }
            }
            _ => {
                err.report(loc, String::from("Only function can be called"));
                Err(())
            }
        }
    }

    fn unify_return(
        &mut self,
        t_var_fun: TypeVar,
        t_var_ret: TypeVar,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let ty_fun = self.subs.substitute(t_var_fun);
        match ty_fun {
            Ty::Var(_) => {
                // Wa can't do anything for now, re-insert the constraint
                self.constraints.push(NewTypeConstraint::Return {
                    fun: t_var_fun,
                    ret: t_var_ret,
                    loc,
                });
                Ok(Progress::None)
            }
            Ty::Composite(CompositeKind::Fun, types) => {
                if let Some(fun_ret_t_var) = types.last() {
                    self.unify_var_var(t_var_ret, *fun_ret_t_var, err, loc)
                } else {
                    err.report_internal(
                        loc,
                        String::from("Function kind must have a return type as last type"),
                    );
                    Err(())
                }
            }
            _ => {
                err.report(loc, String::from("Couldn't evaluate return type"));
                Err(())
            }
        }
    }

    fn unify_var_ty(
        &mut self,
        t_var: &TypeVar,
        ty: &'ty Ty,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        match ty {
            Ty::Var(_) | Ty::OneOf(_, _) | Ty::Base(_) => self.subs.insert_ref(*t_var, ty),
            Ty::Composite(_kind, t_vars) => {
                if let Ok(()) = self.occurs_check(*t_var, t_vars, err, loc) {
                    self.subs.insert_ref(*t_var, ty);
                }
            }
        }
        Ok(Progress::Some)
    }

    fn unify_base_base(
        &mut self,
        t_1: &ScalarType,
        t_2: &ScalarType,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        if t_1 != t_2 {
            err.report(loc, format!("Expected type {}, got {}", t_1, t_2));
            Err(())
        } else {
            Ok(Progress::None)
        }
    }

    fn unify_base_oneof(
        &mut self,
        ty_ref: &'ty Ty,
        t: &ScalarType,
        t_var: &TypeVar,
        ts: &Vec<ScalarType>,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        if ts.contains(t) {
            self.subs.insert_ref(*t_var, ty_ref);
            Ok(Progress::Some)
        } else {
            err.report(
                loc,
                format!(
                    "Incompatible types: can be one of {} but got {}",
                    ts.iter()
                        .map(|t| format!("{}", t))
                        .collect::<Vec<String>>()
                        .join(", "),
                    t
                ),
            );
            Err(())
        }
    }

    fn unify_oneof_oneof(
        &mut self,
        t_var_1: TypeVar,
        ts_1: &Vec<ScalarType>,
        t_var_2: TypeVar,
        ts_2: &Vec<ScalarType>,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        // TODO: improve algorithm complexity (n_max=7, we can deal with that for now)
        let mut intersection = Vec::new();
        for t in ts_1 {
            if ts_2.contains(t) {
                intersection.push(*t);
            }
        }
        if intersection.len() != 0 {
            if intersection.len() == 1 {
                self.subs.insert(t_var_1, Ty::Base(intersection[0]));
            } else {
                self.subs.insert(t_var_1, Ty::OneOf(t_var_1, intersection));
            }
            self.subs.insert(t_var_2, Ty::Var(t_var_1));
            Ok(Progress::Some)
        } else {
            err.report(loc, String::from("Incompatible types"));
            Err(())
        }
    }

    fn unify_composite_composite(
        &mut self,
        kind_1: &CompositeKind,
        tys_1: &Vec<TypeVar>,
        kind_2: &CompositeKind,
        tys_2: &Vec<TypeVar>,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        if kind_1 == kind_2 && tys_1.len() == tys_2.len() {
            let mut progress = Progress::None;
            for (t_var_1, t_var_2) in tys_1.iter().zip(tys_2.iter()) {
                if let Ok(Progress::Some) = self.unify_var_var(*t_var_1, *t_var_2, err, loc) {
                    progress = Progress::Some;
                }
            }
            Ok(progress)
        } else {
            err.report(loc, String::from("Incompatible types"));
            Err(())
        }
    }

    // ————————————————————————————————— Helpers ———————————————————————————————— //

    /// Check if `t_var_base` appears in any of the `t_vars`.
    ///
    /// !Caution: `t_var_base` is assumed to be the representative of its class, that is it was
    /// obtained by a substitution.
    fn occurs_check(
        &mut self,
        t_var_base: TypeVar,
        t_vars: &Vec<TypeVar>,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<(), ()> {
        for t_var_aux in t_vars {
            let ty_aux = self.subs.substitute(*t_var_aux);
            // For now this check is not recursive, and thus will not catch all the cases, but it
            // will do the job for a first draft (missing an occurence check will simply cause a
            // stack overflow, which is not ideal but not critical either).
            if let Ty::Var(t_var_aux) = ty_aux {
                if &t_var_base == t_var_aux {
                    err.report(loc, String::from("Infinite recursive type detected"));
                    return Err(());
                }
            }
        }
        Ok(())
    }

    /// Lift an HIR type (typically obtained through the Ctx and from another module) into a type
    /// variable.
    ///
    /// !TODO: add memoisation to avoid unnecessary allocations.
    fn lift_t(&mut self, t: &hir::Type) -> TypeVar {
        match t {
            hir::Type::Struct(s_id) => {
                let t_var = self.fresh();
                self.subs
                    .insert(t_var, Ty::Composite(CompositeKind::Struct(*s_id), vec![]));
                t_var
            }
            hir::Type::Fun(fun) => {
                let mut types = Vec::with_capacity(fun.params.len() + 1);
                for t in &fun.params {
                    types.push(self.lift_t(t));
                }
                types.push(self.lift_t(&*fun.ret));
                let t_var = self.fresh();
                self.subs
                    .insert(t_var, Ty::Composite(CompositeKind::Fun, types));
                t_var
            }
            hir::Type::Tuple(tup) => {
                let types = tup.0.iter().map(|t| self.lift_t(t)).collect();
                let t_var = self.fresh();
                self.subs
                    .insert(t_var, Ty::Composite(CompositeKind::Tuple, types));
                t_var
            }
            hir::Type::Scalar(x) => self.scalar(*x),
        }
    }
}

// ————————————————————————————————— Helpers ———————————————————————————————— //

/// Return an HIR struct field from a struct ID.
fn get_field<'ctx>(
    s_id: StructId,
    field: &str,
    ctx: &'ctx Ctx,
    err: &mut ErrorHandler,
    loc: Location,
) -> Result<&'ctx hir::StructField, ()> {
    if let Some(struc) = ctx.get_struct(s_id) {
        if let Some(field) = struc.fields.get(field) {
            Ok(field)
        } else {
            err.report(
                loc,
                format!("No field '{}' on struct '{}'", field, &struc.ident),
            );
            Err(())
        }
    } else {
        err.report_internal(loc, format!("Struct with id {} is not in context", s_id));
        Err(())
    }
}

// —————————————————————————————————— Tests ————————————————————————————————— //

mod tests {
    #![allow(unused_imports)]
    use super::*;
    use std::collections::HashSet;
    use crate::hir::Type;

    #[test]
    fn scalars() {
        let store = TyStore::new();
        let ctx = Ctx::new();
        let mut checker = NewTypeChecker::new(&ctx, &store);
        let mut scalars = HashSet::new();

        // Scalar type variables must all be different
        assert!(scalars.insert(checker.scalar(ScalarType::I32)));
        assert!(scalars.insert(checker.scalar(ScalarType::I64)));
        assert!(scalars.insert(checker.scalar(ScalarType::F32)));
        assert!(scalars.insert(checker.scalar(ScalarType::F64)));
        assert!(scalars.insert(checker.scalar(ScalarType::Bool)));
        assert!(scalars.insert(checker.scalar(ScalarType::Null)));

        // A fresh variable should not return a scalar type variable
        assert!(!scalars.contains(&checker.fresh()));
    }

    #[test]
    fn type_check() {
        let store = TyStore::new();
        let ctx = Ctx::new();
        let loc = Location::dummy();
        let mut err = ErrorHandler::new_no_file();
        let mut checker = NewTypeChecker::new(&ctx, &store);

        let t_var_1 = checker.fresh();
        let t_var_2 = checker.fresh();
        let t_var_3 = checker.fresh();
        checker.set_one_of(
            t_var_1,
            vec![
                ScalarType::I32,
                ScalarType::I64,
                ScalarType::F32,
                ScalarType::F64,
            ],
            &mut err,
            loc,
        );
        checker.set_one_of(
            t_var_2,
            vec![ScalarType::I32, ScalarType::I64],
            &mut err,
            loc,
        );
        checker.set_type(t_var_3, ScalarType::I32, &mut err, loc);
        checker
            .unify_var_var(t_var_1, t_var_2, &mut err, loc)
            .unwrap();
        checker
            .unify_var_var(t_var_1, t_var_3, &mut err, loc)
            .unwrap();
        checker.type_check(&mut err).unwrap();

        assert!(!err.has_error());

        let t = Type::Scalar(ScalarType::I32);
        assert_eq!(checker.get_t(t_var_1).unwrap(), t);
        assert_eq!(checker.get_t(t_var_2).unwrap(), t);
        assert_eq!(checker.get_t(t_var_3).unwrap(), t);
    }
}

pub struct TypeChecker<'a> {
    err: &'a mut ErrorHandler,
    ctx: &'a Ctx,
}

impl<'a> TypeChecker<'a> {
    pub fn new(error_handler: &'a mut ErrorHandler, ctx: &'a Ctx) -> TypeChecker<'a> {
        TypeChecker {
            err: error_handler,
            ctx,
        }
    }

    /// Type check a ResolvedProgram.
    pub fn check(&mut self, prog: ResolvedProgram) -> TypedProgram {
        let mut type_vars = prog.type_vars;
        let mut constraints = prog.constraints;
        let mut progress = true;

        while constraints.len() > 0 && progress {
            let mut new_constraints = ConstraintStore::new();
            progress = false;
            for constr in constraints.into_iter() {
                match self.apply_constr(constr, &mut type_vars, &mut new_constraints, &prog.structs)
                {
                    Ok(p) => match p {
                        Progress::Some => progress = true,
                        Progress::None => (),
                    },
                    Err(_) => self.err.silent_report(),
                }
            }
            constraints = new_constraints;
        }

        let store = self.build_store(&type_vars);
        let pub_decls =
            self.get_pub_decls(prog.package.id, &prog.funs, &prog.imports, &prog.structs);

        TypedProgram {
            pub_decls,
            funs: prog.funs,
            imports: prog.imports,
            structs: prog.structs,
            data: prog.data,
            names: prog.names,
            types: prog.types,
            type_vars: store,
            fun_types: prog.fun_types,
            package: prog.package,
        }
    }

    /// Build a `TypeStore` from a `TypeVarStore`, should be called once constraints have been
    /// satisfyied.
    fn build_store(&mut self, var_store: &TypeVarStore) -> TypeVarTypes {
        let integers = var_store.get(T_ID_INTEGER);
        let floats = var_store.get(T_ID_FLOAT);
        let mut store = TypeVarTypes::new();
        for (var, var_id) in var_store {
            if var.types.len() == 1 {
                store.insert(var_id, var.types[0].clone());
            } else if var.types.len() == 0 {
                // TODO: add location
                self.err.report(
                    var.loc,
                    String::from("Variable type does not satisfy constraints"),
                )
            } else {
                // Choose arbitrary type if applicable
                if var.types == integers.types {
                    store.insert(var_id, types::I64);
                } else if var.types == floats.types {
                    store.insert(var_id, types::F64);
                } else {
                    // TODO: improve error handling...
                    self.err
                        .report(var.loc, String::from("Could not infer type"))
                }
            }
        }

        store
    }

    /// Returns the public declarations of the package, this include public functions and
    /// imported runtime module.
    fn get_pub_decls(
        &mut self,
        mod_id: ModId,
        funs: &Vec<Function>,
        imports: &Vec<Imports>,
        type_namespace: &StructStore,
    ) -> ModuleDeclarations {
        let mut pub_decls = ModuleDeclarations::new(mod_id);
        // Functions
        for fun in funs {
            if fun.is_pub {
                pub_decls
                    .val_decls
                    .insert(fun.ident.clone(), ValueDeclaration::Function(fun.fun_id));
            }
        }
        // Imports
        for import in imports {
            pub_decls.runtime_modules.insert(import.from.clone());
            for fun in &import.prototypes {
                pub_decls
                    .val_decls
                    .insert(fun.ident.clone(), ValueDeclaration::Function(fun.fun_id));
            }
        }
        // Types
        for (t_id, t) in type_namespace.iter() {
            if t.is_pub {
                pub_decls
                    .type_decls
                    .insert(t.ident.clone(), TypeDeclaration::Struct(*t_id));
            }
        }

        pub_decls
    }

    /// Apply a constraint, it may modify the type variable store `store`.
    /// Progress can be made or not, depending on the state of the store.
    fn apply_constr(
        &mut self,
        constr: TypeConstraint,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
        structs: &StructStore,
    ) -> Result<Progress, ()> {
        match constr {
            TypeConstraint::Is(t_id, ref t, loc) => self.constr_is(t_id, t, loc, store),
            TypeConstraint::Equality(t_id_1, t_id_2, loc) => {
                self.constr_equality(t_id_1, t_id_2, loc, store, constraints)
            }
            TypeConstraint::Included(t_id_1, t_id_2, loc) => {
                self.constr_included(t_id_1, t_id_2, loc, store, constraints)
            }
            TypeConstraint::Return {
                fun_t_id,
                ret_t_id,
                loc,
            } => self.constr_return(fun_t_id, ret_t_id, loc, store, constraints),
            TypeConstraint::Field(obj_t_id, field_t_id, ref field, loc) => self.constr_field(
                obj_t_id,
                field_t_id,
                field,
                loc,
                store,
                constraints,
                structs,
            ),
            TypeConstraint::Arguments {
                ref args_t_id,
                fun_t_id,
                loc,
            } => self.constr_arguments(args_t_id, fun_t_id, loc, store, constraints),
            TypeConstraint::StructLiteral {
                struct_t_id,
                fields,
                loc,
            } => self.constr_struct(struct_t_id, fields, loc, store, constraints, structs),
            TypeConstraint::TupleLiteral {
                tuple_t_id,
                values_t_ids,
                loc,
            } => self.constr_tuple(tuple_t_id, values_t_ids, loc, store, constraints),
        }
    }

    fn constr_is(
        &mut self,
        t_id: TypeVarId,
        t: &Type,
        loc: Location,
        store: &mut TypeVarStore,
    ) -> Result<Progress, ()> {
        let candidates = &store.get(t_id).types;

        for candidate in candidates {
            if candidate == &Type::Any || t == candidate {
                store.replace(t_id, vec![t.clone()]);
                return Ok(Progress::Some);
            }
        }

        self.err
            .report(loc, String::from("Type constraint can not be satisfied"));
        Err(())
    }

    fn constr_equality(
        &mut self,
        t_id_1: TypeVarId,
        t_id_2: TypeVarId,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Result<Progress, ()> {
        let t_1 = &store.get(t_id_1).types;
        let t_2 = &store.get(t_id_2).types;
        let c = TypeConstraint::Equality(t_id_1, t_id_2, loc);

        // Special cases
        if t_1.len() > 0 && t_1[0] == Type::Any {
            if t_2.len() > 0 && t_2[0] == Type::Any {
                constraints.add(c);
                return Ok(Progress::None);
            }
            let t = t_2.clone();
            store.replace(t_id_1, t);
            constraints.add(c);
            return Ok(Progress::Some);
        } else if t_2.len() > 0 && t_2[0] == Type::Any {
            let t = t_1.clone();
            store.replace(t_id_2, t);
            constraints.add(c);
            return Ok(Progress::Some);
        }

        // Can not infer types
        if t_1.len() == 0 || t_2.len() == 0 {
            self.err.report(
                loc,
                String::from("Could not infer a type satisfying constraints"),
            );
            return Err(());
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
            Ok(Progress::Some)
        } else {
            Ok(Progress::None)
        }
    }

    fn constr_included(
        &mut self,
        t_id_1: TypeVarId,
        t_id_2: TypeVarId,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Result<Progress, ()> {
        let t_1 = &store.get(t_id_1).types;
        let t_2 = &store.get(t_id_2).types;
        let c = TypeConstraint::Included(t_id_1, t_id_2, loc);

        // Special case
        if t_1.len() > 0 && t_1[0] == Type::Any {
            if t_2.len() > 0 && t_2[0] == Type::Any {
                constraints.add(c);
                return Ok(Progress::None);
            }
            let t = t_2.clone();
            store.replace(t_id_1, t);
            return Ok(Progress::Some);
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
            Ok(Progress::Some)
        } else {
            Ok(Progress::None)
        }
    }

    fn constr_return(
        &mut self,
        t_id_fun: TypeVarId,
        t_id: TypeVarId,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Result<Progress, ()> {
        let t_fun = store.get(t_id_fun);
        if t_fun.types.len() != 1 {
            self.err.report_internal(
                t_fun.loc,
                String::from("Return type constraint with ambiguous fun type"),
            );
            return Err(());
        }

        let ret_t = match &t_fun.types[0] {
            Type::Fun(FunctionType { ret, .. }) => (**ret).clone(),
            _ => {
                self.err.report_internal(
                    t_fun.loc,
                    String::from("Return type constraint used on a non function type"),
                );
                return Err(());
            }
        };

        constraints.add(TypeConstraint::Is(t_id, ret_t, loc));
        Ok(Progress::Some)
    }

    fn constr_field(
        &mut self,
        struct_t_id: TypeVarId,
        field_t_id: TypeVarId,
        field: &String,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
        structs: &StructStore,
    ) -> Result<Progress, ()> {
        let field_t = if let Ok(t) = self.get_struct_field_t(
            struct_t_id,
            field,
            loc,
            "Can not access field of a non struct type",
            structs,
            store,
        ) {
            t
        } else {
            return Err(());
        };
        constraints.add(TypeConstraint::Is(field_t_id, field_t, loc));
        Ok(Progress::Some)
    }

    fn constr_struct(
        &mut self,
        struct_t_id: TypeVarId,
        fields: Vec<FieldContstraint>,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
        structs: &StructStore,
    ) -> Result<Progress, ()> {
        let mut ok = true;
        let fields_len = fields.len();

        // type check each field
        for (field, f_t_id, field_loc) in &fields {
            let field_t = if let Ok(t) = self.get_struct_field_t(
                struct_t_id,
                field,
                *field_loc,
                "Can not access field of a non struct type",
                structs,
                store,
            ) {
                t
            } else {
                ok = false;
                continue;
            };
            let t_id = store.fresh(*field_loc, vec![field_t]);
            constraints.add(TypeConstraint::Equality(t_id, *f_t_id, *field_loc));
        }

        // Missing fields
        let n_fields = self.get_struct_fields_len(struct_t_id, loc, structs, store)?;
        if fields_len < n_fields {
            // Rare event, simple but not very efficient code
            let fields = fields
                .iter()
                .map(|(field, _, _)| field.as_str())
                .collect::<Vec<&str>>();
            let mut missing_fields = Vec::new();
            let struc_fields = self
                .get_struct_fields(struct_t_id, loc, structs, store)
                .unwrap(); // TODO: replace by `?`
            for field in struc_fields {
                if !fields.contains(&field.as_str()) {
                    missing_fields.push(field);
                }
            }
            missing_fields.sort();
            self.err.report(
                loc,
                format!("Missing fields: '{}'", missing_fields.join("', '")),
            );
            return Err(());
        }

        if ok {
            Ok(Progress::Some)
        } else {
            Err(())
        }
    }

    fn constr_arguments(
        &mut self,
        args_t_id: &Vec<(TypeVarId, Location)>,
        fun_t_id: TypeVarId,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Result<Progress, ()> {
        let t_fun = store.get(fun_t_id);

        if t_fun.types.len() != 1 {
            self.err.report(loc, String::from("Call a non-function"));
            return Err(());
        }

        if let Type::Fun(FunctionType {
            params: ref f_args, ..
        }) = t_fun.types[0].clone()
        {
            if f_args.len() != args_t_id.len() {
                self.err.report(
                    loc,
                    format!(
                        "Expected {} arguments, got {}",
                        f_args.len(),
                        args_t_id.len()
                    ),
                );
                return Err(());
            }
            for (f_arg, (t_arg, loc)) in f_args.iter().zip(args_t_id.iter()) {
                constraints.add(TypeConstraint::Is(*t_arg, f_arg.clone(), *loc));
            }
            Ok(Progress::Some)
        } else {
            self.err.report(loc, String::from("Call a non-function"));
            Err(())
        }
    }

    fn constr_tuple(
        &mut self,
        tuple_t_id: TypeVarId,
        values_t_ids: Vec<(TypeVarId, Location)>,
        loc: Location,
        store: &mut TypeVarStore,
        constraints: &mut ConstraintStore,
    ) -> Result<Progress, ()> {
        // Two cases here:
        //
        // 1) The type of the tuple is fixed
        // 2) The types of all the elements of the tuple are fixed
        //
        // This mean that the current algorithm does not handle cases where some of the values have
        // an undetermined types, such as integers that can be either i32 or i64.
        let t_tup = store.get(tuple_t_id);

        if t_tup.types.len() == 1 && t_tup.types[0] != Type::Any {
            let tup_loc = t_tup.loc;
            let t_tup = &t_tup.types[0];
            match t_tup {
                Type::Tuple(tup) => {
                    if tup.0.len() != values_t_ids.len() {
                        self.err
                            .report(loc, String::from("Tuple sizes do not match"));
                        return Err(());
                    }
                    for (t, (t_id, loc)) in tup.0.iter().zip(values_t_ids) {
                        constraints.add(TypeConstraint::Is(t_id, t.clone(), loc));
                    }
                    Ok(Progress::Some)
                }
                Type::Any => {
                    panic!(); // Must be unreachable, type guard for any inf `if`
                }
                _ => {
                    self.err.report(tup_loc, String::from("Expected a tuple"));
                    Err(())
                }
            }
        } else {
            let mut types = Vec::with_capacity(values_t_ids.len());
            for (val_t_id, _) in &values_t_ids {
                let t_val = store.get(*val_t_id);
                if t_val.types.len() == 1 {
                    types.push(t_val.types[0].clone());
                } else {
                    // One of the type is still unknow, re-insert the tuple constraint
                    constraints.add(TypeConstraint::TupleLiteral {
                        tuple_t_id,
                        values_t_ids,
                        loc,
                    });
                    return Ok(Progress::None);
                }
            }
            // The tuple type has been fully determined
            constraints.add(TypeConstraint::Is(
                tuple_t_id,
                Type::Tuple(TupleType(types)),
                loc,
            ));
            Ok(Progress::Some)
        }
    }

    /// Return the type of a field from a t_id (that must resolve to a struct) and the field. An
    /// error will be raise if the t_id does not map to a struct, if the field does not exist or if
    /// the type can not be infered (multiple candidates).
    fn get_struct_field_t(
        &mut self,
        struct_t_id: TypeVarId,
        field: &str,
        loc: Location,
        error: &str,
        structs: &StructStore,
        store: &mut TypeVarStore,
    ) -> Result<Type, ()> {
        let s_id = self.get_struct_id_from_t_id(struct_t_id, loc, store, error)?;
        if let Some(struc) = structs.get(s_id) {
            // Defined in the current package
            match struc.fields.get(field) {
                Some(f) => Ok(f.t.clone()),
                None => {
                    self.err.report(
                        loc,
                        format!("No such field '{}' in {}", field, &struc.ident),
                    );
                    Err(())
                }
            }
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            // Defined in the global context
            match struc.fields.get(field) {
                Some(f) => Ok(f.t.lift()),
                None => {
                    self.err.report(
                        loc,
                        format!("No such field '{}' in {}", field, &struc.ident),
                    );
                    Err(())
                }
            }
        } else {
            self.err
                .report_internal(loc, format!("No type with id '{}'", s_id));
            Err(())
        }
    }

    /// Return the number of fields in the struct.
    fn get_struct_fields_len(
        &mut self,
        struct_t_id: TypeVarId,
        loc: Location,
        structs: &StructStore,
        store: &mut TypeVarStore,
    ) -> Result<usize, ()> {
        let s_id = self.get_struct_id_from_t_id(
            struct_t_id,
            loc,
            store,
            "Could not get number of fields",
        )?;
        if let Some(struc) = structs.get(s_id) {
            // Defined in the current package
            Ok(struc.fields.len())
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            // Defined in the global context
            Ok(struc.fields.len())
        } else {
            self.err
                .report_internal(loc, format!("No type with id '{}'", s_id));
            Err(())
        }
    }

    /// Return a vec of fields names for this struct.
    fn get_struct_fields(
        &mut self,
        struct_t_id: TypeVarId,
        loc: Location,
        structs: &StructStore,
        store: &mut TypeVarStore,
    ) -> Result<Vec<String>, ()> {
        let s_id = self.get_struct_id_from_t_id(
            struct_t_id,
            loc,
            store,
            "Could not get number of fields",
        )?;
        if let Some(struc) = structs.get(s_id) {
            // Defined in the current package
            Ok(struc
                .fields
                .iter()
                .map(|(ident, _)| ident.clone())
                .collect())
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            // Defined in the global context
            Ok(struc
                .fields
                .iter()
                .map(|(ident, _)| ident.clone())
                .collect())
        } else {
            self.err
                .report_internal(loc, format!("No type with id '{}'", s_id));
            Err(())
        }
    }

    /// Retrieve a struct_id from a type_id, raise an error if type_id does not map to a struct.
    fn get_struct_id_from_t_id(
        &mut self,
        struct_t_id: TypeVarId,
        loc: Location,
        store: &mut TypeVarStore,
        error: &str,
    ) -> Result<StructId, ()> {
        let t_struct = store.get(struct_t_id);
        if t_struct.types.len() > 1 {
            self.err
                .report(t_struct.loc, String::from("Ambiguous type"));
        }
        let t_struct = if let Some(t) = t_struct.types.first() {
            t
        } else {
            self.err.report(loc, String::from("Could not infer type"));
            return Err(());
        };
        match t_struct {
            Type::Struct(s_id) => Ok(*s_id),
            _ => {
                self.err.report(loc, String::from(error));
                Err(())
            }
        }
    }
}
