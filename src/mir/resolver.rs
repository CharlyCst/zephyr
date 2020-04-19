use super::names::{Name, NameId, NameStore};
use super::types::id::*;
use super::types::{ConstraintStore, Type, TypeConstraint, TypeId, TypeVarStore};
use crate::error::{ErrorHandler, Location};
use crate::parse;

use super::Program;

use std::collections::HashMap;

pub struct ResolverState {
    names: NameStore,
    types: TypeVarStore,
    contexts: Vec<HashMap<String, usize>>,
    constraints: ConstraintStore,
}

impl ResolverState {
    pub fn new() -> ResolverState {
        let contexts = vec![HashMap::new()];
        ResolverState {
            names: NameStore::new(),
            types: TypeVarStore::new(),
            contexts: contexts,
            constraints: ConstraintStore::new(),
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
    ) -> Result<(NameId, TypeId), Location> {
        if let Some(n) = self.find_in_context(&ident) {
            return Err(n.loc);
        }

        let ident_key = ident.clone();
        let t_id = self.types.fresh(type_candidates);
        let n_id = self.names.fresh(ident, loc, t_id);
        self.add_in_context(ident_key, n_id);
        Ok((n_id, t_id))
    }

    pub fn new_constraint(&mut self, constraint: TypeConstraint) {
        self.constraints.add(constraint)
    }

    pub fn find_in_context(&self, ident: &str) -> Option<&Name> {
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

    pub fn resolve(&mut self, mut funs: Vec<parse::Function>) -> Program {
        let mut state = ResolverState::new();

        self.register_functions(&funs, &mut state);

        for fun in &mut funs {
            self.resolve_function(fun, &mut state);
        }

        Program {
            funs: funs,
            names: state.names,
            types: state.types,
            constraints: state.constraints,
        }
    }

    fn resolve_function(&mut self, fun: &mut parse::Function, state: &mut ResolverState) {
        state.new_scope();

        let fun_t_id = if let Some(name) = state.find_in_context(&fun.ident) {
            name.t_id
        } else {
            self.error_handler
                .report_internal(fun.loc, "Function name is not yet in context");
            state.exit_scope();
            return;
        };

        for param in &mut fun.params {
            let t = match get_var_type(&param) {
                Some(t) => t,
                None => {
                    self.error_handler
                        .report(param.loc, "Missing or unrecognized type");
                    continue;
                }
            };

            match state.declare(param.ident.clone(), t, param.loc) {
                Ok((n_id, _)) => {
                    param.n_id = n_id;
                }
                Err(decl_loc) => {
                    let error = format!(
                        "Name {} already defined in current context at line {}",
                        fun.ident, decl_loc.line
                    );
                    self.error_handler.report(fun.loc, &error);
                }
            }
        }

        self.resolve_block(&mut fun.block, state, fun_t_id);
        state.exit_scope();
    }

    fn resolve_block(
        &mut self,
        block: &mut parse::Block,
        state: &mut ResolverState,
        fun_t_id: TypeId,
    ) {
        state.new_scope();

        for stmt in block.stmts.iter_mut() {
            match stmt {
                parse::Statement::AssignStmt { var, expr } => {
                    let right_t_id = if let Some(name) = state.find_in_context(&var.ident) {
                        var.n_id = name.n_id;
                        name.t_id
                    } else {
                        self.error_handler.report(
                            var.loc,
                            &format!("Variable name {} is not defined", var.ident),
                        );
                        continue;
                    };
                    let left_t_id = self.resolve_expression(expr, state);
                    state.new_constraint(TypeConstraint::Equality(right_t_id, left_t_id));
                }
                parse::Statement::LetStmt { var, expr } => {
                    match state.declare(var.ident.clone(), vec![Type::Any], var.loc) {
                        Ok((n_id, right_t_id)) => {
                            var.n_id = n_id;
                            let left_t_id = self.resolve_expression(expr, state);
                            state.new_constraint(TypeConstraint::Equality(right_t_id, left_t_id));
                        }
                        Err(decl_loc) => {
                            let error = format!(
                                "Name {} already defined in current context at line {}",
                                var.ident, decl_loc.line
                            );
                            self.error_handler.report(var.loc, &error);
                            continue;
                        }
                    }
                }
                parse::Statement::IfStmt { expr, block } => {
                    let expr_t_id = self.resolve_expression(expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    self.resolve_block(block, state, fun_t_id);
                }
                parse::Statement::WhileStmt { expr, block } => {
                    let expr_t_id = self.resolve_expression(expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    self.resolve_block(block, state, fun_t_id);
                }
                parse::Statement::ReturnStmt { expr } => {
                    if let Some(ret_expr) = expr {
                        let ret_t_id = self.resolve_expression(ret_expr, state);
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id));
                    } else {
                        let ret_t_id = state.types.fresh(vec![Type::Unit]);
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id));
                    }
                }
                parse::Statement::ExprStmt { expr } => {
                    self.resolve_expression(expr, state);
                }
            };
        }

        state.exit_scope();
    }

    fn resolve_expression(
        &mut self,
        expr: &mut parse::Expression,
        state: &mut ResolverState,
    ) -> TypeId {
        match expr {
            parse::Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let left_t_id = self.resolve_expression(expr_left, state);
                let right_t_id = self.resolve_expression(expr_right, state);
                match binop {
                    parse::BinaryOperator::Plus
                    | parse::BinaryOperator::Multiply
                    | parse::BinaryOperator::Minus
                    | parse::BinaryOperator::Divide => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        left_t_id
                    }
                    parse::BinaryOperator::Greater
                    | parse::BinaryOperator::GreaterEqual
                    | parse::BinaryOperator::Less
                    | parse::BinaryOperator::LessEqual => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        T_ID_BOOL
                    }
                    parse::BinaryOperator::Equal => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_BASIC));
                        T_ID_BOOL
                    }
                    _ => panic!("Binop not implemented yet"), // TODO
                }
            }
            parse::Expression::Literal { value } => match value {
                parse::Value::Integer { mut t_id, .. } => {
                    let fresh_t_id = state.types.fresh(vec![Type::I32, Type::I64]);
                    t_id = fresh_t_id;
                    fresh_t_id
                }
                parse::Value::Boolean { mut t_id, .. } => {
                    t_id = T_ID_BOOL;
                    T_ID_BOOL
                }
            },
            parse::Expression::Variable { var } => {
                if let Some(name) = state.find_in_context(&var.ident) {
                    var.n_id = name.n_id;
                    name.t_id
                } else {
                    self.error_handler.report(
                        var.loc,
                        &format!("Variable {} used but not declared", var.ident),
                    );
                    return 0;
                }
            }
            _ => panic!("Expression not implemented"), // TODO
        }
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
