use super::hir;
use super::hir::ScalarType;
use super::names::{StructId, StructStore};
use crate::arena::Arena;
use crate::ctx::Ctx;
use crate::error::{ErrorHandler, Location};

use std::collections::{HashMap, HashSet};
use std::fmt;

// —————————————————————————————————— Types ————————————————————————————————— //

/// Indicates if some progress has been made.
#[derive(Eq, PartialEq)]
enum Progress {
    Some,
    None,
}

#[derive(Hash, Eq, PartialEq, Copy, Clone, Debug)]
pub struct TypeVar(usize);

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

enum TypeConstraint {
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
    StructLiteral {
        t_var: TypeVar,
        fields: Vec<(TypeVar, String, Location)>,
        loc: Location,
    },
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

// —————————————————————————————— Type Checker —————————————————————————————— //

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
                        let new_ty = self.substitute(*next_t_var);
                        self.subs.insert(t_var, new_ty);
                        new_ty
                    }
                }
                Ty::OneOf(next_t_var, _) => {
                    if t_var == *next_t_var {
                        // This one is definitely not an error, though
                        // OneOf Tys explicitely keep a back-reference
                        ty
                    } else {
                        let new_ty = self.substitute(*next_t_var);
                        self.subs.insert(t_var, new_ty);
                        new_ty
                    }
                }
                _ => ty,
            },
        }
    }
}

pub struct TypeChecker<'ctx, 'ty> {
    ctx: &'ctx Ctx,
    type_var_counter: usize,
    constraints: Vec<TypeConstraint>,
    subs: Substitution<'ty>,

    // Scalar types
    t_i32: TypeVar,
    t_i64: TypeVar,
    t_f32: TypeVar,
    t_f64: TypeVar,
    t_bool: TypeVar,
    t_null: TypeVar,
}

impl<'ctx, 'ty> TypeChecker<'ctx, 'ty> {
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
        let t_var_t = self.scalar(t);
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

    /// Set a type variable to the type of a struct with given StructId.
    pub fn set_struct(
        &mut self,
        t_var: TypeVar,
        s_id: StructId,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        let struct_ty = self
            .subs
            .store
            .store(Ty::Composite(CompositeKind::Struct(s_id), Vec::new()));
        let _ = self.unify_var_ty(&t_var, struct_ty, err, loc);
    }

    /// Validate a struct literal initialization by setting appropriate constraints on all fields.
    pub fn set_struct_literal(
        &mut self,
        t_var: TypeVar,
        fields: Vec<(TypeVar, String, Location)>,
        _err: &mut ErrorHandler,
        loc: Location,
    ) {
        self.constraints
            .push(TypeConstraint::StructLiteral { t_var, fields, loc });
    }

    /// Set a type variable to the type of a function with given parameters and return type.
    pub fn set_fun(
        &mut self,
        t_var: TypeVar,
        mut params: Vec<TypeVar>,
        ret: TypeVar,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        params.push(ret);
        let fun_ty = self
            .subs
            .store
            .store(Ty::Composite(CompositeKind::Fun, params));
        let _t = self.unify_var_ty(&t_var, fun_ty, err, loc);
    }

    /// Set a type variable to the type of a tuple with given members' types.
    pub fn set_tuple(
        &mut self,
        t_var: TypeVar,
        types: Vec<TypeVar>,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        let tuple_ty = self
            .subs
            .store
            .store(Ty::Composite(CompositeKind::Tuple, types));
        let _ = self.unify_var_ty(&t_var, tuple_ty, err, loc);
    }

    /// Apply an 'equal' type constraint on `t_var_1` and `t_var_2`.
    pub fn set_equal(
        &mut self,
        t_var_1: TypeVar,
        t_var_2: TypeVar,
        err: &mut ErrorHandler,
        loc: Location,
    ) {
        let _ = self.unify_var_var(t_var_1, t_var_2, err, loc);
    }

    /// Apply a 'call' constraint between a function and its arguments.
    pub fn set_call(&mut self, t_var_fun: TypeVar, t_var_args: Vec<TypeVar>, loc: Location) {
        self.constraints.push(TypeConstraint::Call {
            fun: t_var_fun,
            args: t_var_args,
            loc,
        })
    }

    /// Apply a 'return' constraint between a function and its return type.
    pub fn set_return(&mut self, t_var_fun: TypeVar, t_var_ret: TypeVar, loc: Location) {
        self.constraints.push(TypeConstraint::Return {
            fun: t_var_fun,
            ret: t_var_ret,
            loc,
        })
    }

    /// Apply an 'access' constraint between an object and a field.
    pub fn set_access(
        &mut self,
        t_var_object: TypeVar,
        t_var_field: TypeVar,
        field_name: String,
        loc: Location,
    ) {
        self.constraints.push(TypeConstraint::Access {
            object: t_var_object,
            field: t_var_field,
            field_name,
            loc,
        })
    }

    /// Recursively apply all remaining constraints, will return an error if an unification failed
    /// or if some remaining constraint can't make further progress.
    pub fn type_check(&mut self, structs: &StructStore, err: &mut ErrorHandler) -> Result<(), ()> {
        let mut progress;
        let mut error = false;
        loop {
            progress = Progress::None;
            let mut constraints = Vec::new();
            std::mem::swap(&mut self.constraints, &mut constraints);
            for constr in constraints {
                let result = match constr {
                    TypeConstraint::Access {
                        object,
                        field,
                        field_name,
                        loc,
                    } => self.unify_field(object, field, field_name, structs, err, loc),
                    TypeConstraint::Call { fun, args, loc } => self.unify_call(fun, args, err, loc),
                    TypeConstraint::Return { fun, ret, loc } => {
                        self.unify_return(fun, ret, err, loc)
                    }
                    TypeConstraint::StructLiteral { t_var, fields, loc } => {
                        self.unify_struct_literal(t_var, fields, structs, err, loc)
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

    /// Try to convert a type variable into an HIR type. This should be called after `type_check`
    /// to ensure that all constraints have been taken into account.
    /// Will return a none in case the type can't be determined.
    pub fn get_t(&mut self, t_var: TypeVar) -> Option<hir::Type> {
        let ty = self.subs.substitute(t_var);
        match ty {
            Ty::Var(_) => None,
            Ty::Base(t) => Some(hir::Type::Scalar(*t)),
            Ty::OneOf(_, ts) => {
                // Types are assumed to be sorted!
                if let Some(t) = ts.first() {
                    Some(hir::Type::Scalar(*t))
                } else {
                    None
                }
            }
            Ty::Composite(kind, ts) => match kind {
                CompositeKind::Struct(s_id) => Some(hir::Type::Struct(*s_id)),
                CompositeKind::Fun => {
                    let (ret_t_var, param_t_vars) = ts.split_last()?;
                    let ret = Box::new(self.get_t(*ret_t_var)?);
                    let mut params = Vec::with_capacity(param_t_vars.len());
                    for t in param_t_vars {
                        params.push(self.get_t(*t)?);
                    }
                    Some(hir::Type::Fun(hir::FunctionType { params, ret }))
                }
                CompositeKind::Tuple => {
                    let mut types = Vec::with_capacity(ts.len());
                    for t in ts {
                        types.push(self.get_t(*t)?);
                    }
                    Some(hir::Type::Tuple(hir::TupleType(types)))
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
        structs: &StructStore,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let ty_obj = self.subs.substitute(object);
        match ty_obj {
            Ty::Var(_) => {
                // We can't do anything for now, re-insert the constraint
                self.constraints.push(TypeConstraint::Access {
                    object,
                    field: t_var,
                    field_name,
                    loc,
                });
                Ok(Progress::None)
            }
            Ty::Base(_) | Ty::OneOf(_, _) => {
                err.report(loc, format!("No field '{}' on basic types", &field_name));
                Err(())
            }
            Ty::Composite(kind, _) => {
                match kind {
                    CompositeKind::Struct(s_id) => {
                        let t_var_field = self.get_field(*s_id, &field_name, structs, err, loc)?;
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
                self.constraints.push(TypeConstraint::Call {
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
                            if n_args > 1 { "s" } else { "" },
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
                self.constraints.push(TypeConstraint::Return {
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

    fn unify_struct_literal(
        &mut self,
        t_var: TypeVar,
        fields: Vec<(TypeVar, String, Location)>,
        structs: &StructStore,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<Progress, ()> {
        let s_id = match self.subs.substitute(t_var) {
            Ty::Composite(CompositeKind::Struct(s_id), _) => *s_id,
            Ty::Var(_) => {
                self.constraints
                    .push(TypeConstraint::StructLiteral { t_var, fields, loc });
                return Ok(Progress::None);
            }
            _ => {
                err.report(loc, format!("Struct literal of non struct type"));
                return Err(());
            }
        };
        self.set_struct(t_var, s_id, err, loc);
        let nb_fields = if let Ok(nb_fields) = self.get_nb_fields_in_struct(s_id, structs, err, loc)
        {
            nb_fields
        } else {
            return Err(());
        };

        // Check for missing fields
        if nb_fields != fields.len() {
            if let Ok(mut field_set) = self.get_struct_fields(s_id, structs, err, loc) {
                for (_, field_name, _) in &fields {
                    field_set.remove(field_name);
                }
                if field_set.len() > 0 {
                    let mut missing_fields = field_set
                        .iter()
                        .map(|f| format!("'{}'", f))
                        .collect::<Vec<String>>();
                    missing_fields.sort();
                    err.report(
                        loc,
                        format!(
                            "Missing field{}: {}",
                            if field_set.len() > 1 { "s" } else { "" },
                            missing_fields.join(", ")
                        ),
                    )
                }
            } else {
                return Err(());
            }
        }

        // Add type constraints for existing fields
        for (t_var_field, field_name, field_loc) in fields {
            self.set_access(t_var, t_var_field, field_name, field_loc);
        }
        Ok(Progress::Some)
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

    /// Lift an HIR type (typically obtained through the Ctx and from another module) into a type
    /// variable.
    ///
    /// !TODO: add memoisation to avoid unnecessary allocations.
    pub fn lift_t(&mut self, t: &hir::Type) -> TypeVar {
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

    /// Lift an HIR function type (typically obtained through the Ctx and from another module) into
    /// a type variable.
    ///
    /// !TODO: add memoisation to avoid unnecessary allocations.
    pub fn lift_t_fun(&mut self, fun_t: &hir::FunctionType) -> TypeVar {
        let mut types = Vec::with_capacity(fun_t.params.len() + 1);
        for t in &fun_t.params {
            types.push(self.lift_t(t));
        }
        types.push(self.lift_t(&*fun_t.ret));
        let t_var = self.fresh();
        self.subs
            .insert(t_var, Ty::Composite(CompositeKind::Fun, types));
        t_var
    }

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

    /// Return a type variable for a field from a struct ID.
    fn get_field(
        &mut self,
        s_id: StructId,
        field: &str,
        structs: &StructStore,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<TypeVar, ()> {
        if let Some(struc) = structs.get(s_id) {
            if let Some(field) = struc.fields.get(field) {
                Ok(field.t_var)
            } else {
                err.report(
                    loc,
                    format!("No field '{}' on struct '{}'", field, &struc.ident),
                );
                Err(())
            }
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            if let Some(field) = struc.fields.get(field) {
                Ok(self.lift_t(&field.t))
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

    /// Return the number of fields in a given struct.
    fn get_nb_fields_in_struct(
        &self,
        s_id: StructId,
        structs: &StructStore,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<usize, ()> {
        if let Some(struc) = structs.get(s_id) {
            Ok(struc.fields.len())
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            Ok(struc.fields.len())
        } else {
            err.report_internal(loc, format!("Struct with id {} is not in context", s_id));
            Err(())
        }
    }

    /// return a set of fields present in a struct.
    fn get_struct_fields(
        &self,
        s_id: StructId,
        structs: &StructStore,
        err: &mut ErrorHandler,
        loc: Location,
    ) -> Result<HashSet<String>, ()> {
        let mut set = HashSet::new();
        if let Some(struc) = structs.get(s_id) {
            for (field, _) in &struc.fields {
                set.insert(field.clone());
            }
            Ok(set)
        } else if let Some(struc) = self.ctx.get_struct(s_id) {
            for (field, _) in &struc.fields {
                set.insert(field.clone());
            }
            Ok(set)
        } else {
            err.report_internal(loc, format!("Struct with id {} is not in context", s_id));
            Err(())
        }
    }
}

// —————————————————————————————————— Tests ————————————————————————————————— //

mod tests {
    #![allow(unused_imports)]
    use super::*;
    use crate::hir::Type;
    use std::collections::HashSet;

    #[test]
    fn scalars() {
        let store = TyStore::new();
        let ctx = Ctx::new();
        let mut checker = TypeChecker::new(&ctx, &store);
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
        let structs = StructStore::new(1);
        let mut err = ErrorHandler::new_no_file();
        let mut checker = TypeChecker::new(&ctx, &store);

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
        checker.type_check(&structs, &mut err).unwrap();

        assert!(!err.has_error());

        let t = Type::Scalar(ScalarType::I32);
        assert_eq!(checker.get_t(t_var_1).unwrap(), t);
        assert_eq!(checker.get_t(t_var_2).unwrap(), t);
        assert_eq!(checker.get_t(t_var_3).unwrap(), t);
    }
}

// ———————————————————————————————— Display ————————————————————————————————— //

impl fmt::Display for TypeVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl fmt::Display for TypeConstraint {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeConstraint::Call { fun, args, .. } => write!(
                f,
                "call #{} with [{}]",
                fun,
                args.iter()
                    .map(|arg| format!("#{}", arg))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
            TypeConstraint::Return { fun, ret, .. } => write!(f, "return #{} into #{}", fun, ret),
            TypeConstraint::Access {
                object,
                field,
                field_name,
                ..
            } => write!(f, "access #{}.{} as #{}", object, field_name, field),
            TypeConstraint::StructLiteral { t_var, fields, .. } => {
                write!(
                    f,
                    "struct #{} with {{ {} }}",
                    t_var,
                    fields
                        .iter()
                        .map(|(t_var, name, _)| format!("{}: #{}", name, t_var))
                        .collect::<Vec<String>>()
                        .join(", ")
                )
            }
        }
    }
}

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Ty::Var(t_var) => write!(f, "#{}", t_var),
            Ty::Base(t) => write!(f, "{}", t),
            Ty::OneOf(t_var, ts) => write!(
                f,
                "{} as #{}",
                ts.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(" | "),
                t_var
            ),
            Ty::Composite(kind, ts) => write!(
                f,
                "{}<{}>",
                kind,
                ts.iter()
                    .map(|t| format!("{}", t))
                    .collect::<Vec<String>>()
                    .join(", ")
            ),
        }
    }
}

impl fmt::Display for CompositeKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            CompositeKind::Tuple => write!(f, "Tuple"),
            CompositeKind::Fun => write!(f, "Fun"),
            CompositeKind::Struct(s_id) => write!(f, "Struct({})", s_id),
        }
    }
}

impl<'ty> fmt::Display for Substitution<'ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buff = String::from("Substitution {\n");
        let mut sorted_subs = self.subs.iter().collect::<Vec<(&TypeVar, &&Ty)>>();
        sorted_subs.sort_by(|(t_var_a, _), (t_var_b, _)| t_var_a.0.cmp(&t_var_b.0));
        for (t_var, ty) in &sorted_subs {
            buff.push_str(&format!("  {:>4} -> {}\n", &format!("{}", t_var), ty));
        }
        buff.push_str("}\n");
        write!(f, "{}", buff)
    }
}

impl<'ctx, 'ty> fmt::Display for TypeChecker<'ctx, 'ty> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut buff = String::from("Constraints {\n");
        for constraint in &self.constraints {
            buff.push_str(&format!("    {}\n", constraint));
        }
        buff.push_str("}\n");
        write!(f, "{}\n{}", &self.subs, buff)
    }
}
