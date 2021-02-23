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
use crate::ctx::{Ctx, ModId, ModuleDeclarations};
use crate::error::{ErrorHandler, Location};

use std::cmp::Ordering;

/// Indicates if some progress has been made.
enum Progress {
    Some,
    None,
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
        } else if let Some(struc) = self.ctx.get_s(s_id) {
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
        } else if let Some(struc) = self.ctx.get_s(s_id) {
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
        } else if let Some(struc) = self.ctx.get_s(s_id) {
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
