use super::types::{ConstraintStore, Type, TypeConstraint, TypeId, TypeVarStore};
use super::Program;
use crate::error::{ErrorHandler, Location};

use std::cmp::Ordering;

pub struct TypeChecker {
    error_handler: ErrorHandler,
}

impl TypeChecker {
    pub fn new() -> TypeChecker {
        TypeChecker {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn check(&mut self, mut prog: Program) {
        let mut type_vars = prog.types;
        let constraints = prog.constraints;

        for constr in &constraints {
            self.apply_constr(constr, &mut type_vars)
        }

        println!("{}", type_vars);
    }

    fn apply_constr(&mut self, constr: &TypeConstraint, store: &mut TypeVarStore) {
        match constr {
            TypeConstraint::Equality(t_id_1, t_id_2) => (),
            TypeConstraint::Included(t_id_1, t_id_2) => {
                let mut t = Vec::new();
                let t_1 = store.get(*t_id_1);
                let t_2 = store.get(*t_id_2);
                let mut idx_1 = 0;
                let mut idx_2 = 0;

                // Special case
                if t_1.len() > 0 && t_1[0] == Type::Any {
                    if t_2.len() > 0 && t_2[0] == Type::Any {
                        return;
                    }
                    let t = t_2.clone();
                    store.replace(*t_id_1, t);
                    return;
                }

                while idx_1 < t_1.len() && idx_2 < t_2.len() {
                    match t_1[idx_1].cmp(&t_2[idx_2]) {
                        Ordering::Less => {
                            idx_1 += 1;
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

                store.replace(*t_id_1, t);
            }
            TypeConstraint::Return(t_id_fun, t_id) => (),
        }
    }
}
