use super::names::*;
use super::types::id::*;
use super::types::{ConstraintStore, Type, TypeConstraint, TypeId, TypeVarStore};
use crate::error::{ErrorHandler, Location};
use crate::parse;

use super::ResolvedProgram;

use std::collections::HashMap;

struct State {
    names: NameStore,
    types: TypeVarStore,
    contexts: Vec<HashMap<String, usize>>,
    constraints: ConstraintStore,
}

impl State {
    pub fn new() -> State {
        let contexts = vec![HashMap::new()];
        State {
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
        let t_id = self.types.fresh(loc, type_candidates);
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

    pub fn resolve(&mut self, funs: Vec<parse::Function>) -> ResolvedProgram {
        let mut state = State::new();
        let mut named_funs = Vec::with_capacity(funs.len());

        self.register_functions(&funs, &mut state);

        for fun in funs.into_iter() {
            if let Some(named_fun) = self.resolve_function(fun, &mut state) {
                named_funs.push(named_fun);
            }
        }

        ResolvedProgram {
            funs: named_funs,
            names: state.names,
            types: state.types,
            constraints: state.constraints,
        }
    }

    fn resolve_function(&mut self, fun: parse::Function, state: &mut State) -> Option<Function> {
        state.new_scope();
        let mut locals = Vec::new();
        let mut fun_params = Vec::new();

        // Get the function name from global context. All functions must have been registered at that point.
        let fun_name = if let Some(name) = state.find_in_context(&fun.ident) {
            name
        } else {
            self.error_handler
                .report_internal_loc(fun.loc, "Function name is not yet in context");
            state.exit_scope();
            return None;
        };
        let fun_t_id = fun_name.t_id;
        let fun_n_id = fun_name.n_id;

        for param in fun.params.into_iter() {
            let t = match get_var_type(&param) {
                Some(t) => t,
                None => {
                    self.error_handler
                        .report(param.loc, "Missing or unrecognized type");
                    continue;
                }
            };

            match state.declare(param.ident.clone(), t, param.loc) {
                Ok((n_id, _)) => fun_params.push(Variable {
                    ident: param.ident,
                    loc: param.loc,
                    n_id: n_id,
                }),
                Err(decl_loc) => {
                    let error = format!(
                        "Name {} already defined in current context at line {}",
                        fun.ident, decl_loc.line
                    );
                    self.error_handler.report(fun.loc, &error);
                }
            }
        }

        let block = self.resolve_block(fun.block, state, &mut locals, fun_t_id);
        state.exit_scope();

        return Some(Function {
            ident: fun.ident,
            params: fun_params,
            locals: locals,
            block: block,
            exported: fun.exported,
            loc: fun.loc,
            n_id: fun_n_id,
        });
    }

    fn resolve_block(
        &mut self,
        block: parse::Block,
        state: &mut State,
        locals: &mut Vec<NameId>,
        fun_t_id: TypeId,
    ) -> Block {
        state.new_scope();
        let mut stmts = Vec::new();

        for stmt in block.stmts.into_iter() {
            let named_stmt = match stmt {
                parse::Statement::AssignStmt { var, mut expr } => {
                    let (var, var_t_id) = if let Some(name) = state.find_in_context(&var.ident) {
                        let var = Variable {
                            ident: var.ident,
                            loc: var.loc,
                            n_id: name.n_id,
                        };
                        (var, name.t_id)
                    } else {
                        self.error_handler.report(
                            var.loc,
                            &format!(
                                "Variable name {} is not defined in current scope",
                                var.ident
                            ),
                        );
                        continue;
                    };
                    let (expr, t_id) = self.resolve_expression(&mut expr, state);
                    state.new_constraint(TypeConstraint::Equality(var_t_id, t_id));
                    Statement::AssignStmt {
                        var: Box::new(var),
                        expr: Box::new(expr),
                    }
                }
                parse::Statement::LetStmt { var, mut expr } => {
                    match state.declare(var.ident.clone(), vec![Type::Any], var.loc) {
                        Ok((n_id, var_t_id)) => {
                            locals.push(n_id);
                            let (expr, expr_t_id) = self.resolve_expression(&mut expr, state);
                            state.new_constraint(TypeConstraint::Equality(var_t_id, expr_t_id));
                            Statement::LetStmt {
                                var: Box::new(Variable {
                                    ident: var.ident,
                                    loc: var.loc,
                                    n_id,
                                }),
                                expr: Box::new(expr),
                            }
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
                parse::Statement::IfStmt { mut expr, block } => {
                    let (expr, expr_t_id) = self.resolve_expression(&mut expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    let block = self.resolve_block(block, state, locals, fun_t_id);
                    Statement::IfStmt {
                        expr: Box::new(expr),
                        block: block,
                    }
                }
                parse::Statement::WhileStmt { mut expr, block } => {
                    let (expr, expr_t_id) = self.resolve_expression(&mut expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    let block = self.resolve_block(block, state, locals, fun_t_id);
                    Statement::WhileStmt {
                        expr: Box::new(expr),
                        block: block,
                    }
                }
                parse::Statement::ReturnStmt { expr, loc } => {
                    if let Some(mut ret_expr) = expr {
                        let (expr, ret_t_id) = self.resolve_expression(&mut ret_expr, state);
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id));
                        Statement::ReturnStmt {
                            expr: Some(expr),
                            loc: loc,
                        }
                    } else {
                        let ret_t_id = state.types.fresh(loc, vec![Type::Unit]);
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id));
                        Statement::ReturnStmt {
                            expr: None,
                            loc: loc,
                        }
                    }
                }
                parse::Statement::ExprStmt { mut expr } => {
                    let (expr, _) = self.resolve_expression(&mut expr, state);
                    Statement::ExprStmt {
                        expr: Box::new(expr),
                    }
                }
            };
            stmts.push(named_stmt);
        }

        state.exit_scope();
        Block { stmts: stmts }
    }

    fn resolve_expression(
        &mut self,
        expr: &mut parse::Expression,
        state: &mut State,
    ) -> (Expression, TypeId) {
        match expr {
            parse::Expression::Unary { unop, expr } => {
                let (expr, t_id) = self.resolve_expression(expr, state);
                match unop {
                    parse::UnaryOperator::Minus => {
                        state.new_constraint(TypeConstraint::Included(t_id, T_ID_NUMERIC));
                        let expr = Expression::Unary {
                            expr: Box::new(expr),
                            unop: *unop,
                            t_id: t_id,
                        };
                        (expr, t_id)
                    }
                    parse::UnaryOperator::Not => {
                        state.new_constraint(TypeConstraint::Included(t_id, T_ID_BOOL));
                        let expr = Expression::Unary {
                            expr: Box::new(expr),
                            unop: *unop,
                            t_id: t_id,
                        };
                        (expr, t_id)
                    }
                }
            }
            parse::Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let (left_expr, left_t_id) = self.resolve_expression(expr_left, state);
                let (right_expr, right_t_id) = self.resolve_expression(expr_right, state);
                match binop {
                    parse::BinaryOperator::Plus
                    | parse::BinaryOperator::Multiply
                    | parse::BinaryOperator::Minus
                    | parse::BinaryOperator::Divide => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: left_t_id,
                        };
                        (expr, left_t_id)
                    }
                    parse::BinaryOperator::Greater
                    | parse::BinaryOperator::GreaterEqual
                    | parse::BinaryOperator::Less
                    | parse::BinaryOperator::LessEqual => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: T_ID_BOOL,
                        };
                        (expr, T_ID_BOOL)
                    }
                    parse::BinaryOperator::Equal => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_BASIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: T_ID_BOOL,
                        };
                        (expr, T_ID_BOOL)
                    }
                    _ => panic!("Binop not implemented yet"), // TODO
                }
            }
            parse::Expression::Literal { value } => match value {
                parse::Value::Integer { val, loc } => {
                    let fresh_t_id = state.types.fresh(*loc, vec![Type::I32, Type::I64]);
                    let expr = Expression::Literal {
                        value: Value::Integer {
                            val: *val,
                            loc: *loc,
                            t_id: fresh_t_id,
                        },
                    };
                    (expr, fresh_t_id)
                }
                parse::Value::Boolean { val, loc } => {
                    let expr = Expression::Literal {
                        value: Value::Boolean {
                            val: *val,
                            loc: *loc,
                            t_id: T_ID_BOOL,
                        },
                    };
                    (expr, T_ID_BOOL)
                }
            },
            parse::Expression::Variable { var } => {
                if let Some(name) = state.find_in_context(&var.ident) {
                    let expr = Expression::Variable {
                        var: Variable {
                            ident: var.ident.clone(),
                            loc: var.loc,
                            n_id: name.n_id,
                        },
                    };
                    (expr, name.t_id)
                } else {
                    self.error_handler.report(
                        var.loc,
                        &format!("Variable {} used but not declared", var.ident),
                    );
                    let dummy_expr = Expression::Literal {
                        value: Value::Boolean {
                            loc: var.loc,
                            val: false,
                            t_id: T_ID_BOOL,
                        },
                    };
                    return (dummy_expr, T_ID_BOOL);
                }
            }
            _ => panic!("Expression not implemented"), // TODO
        }
    }

    // must be called first to bring all global function declarations in scope
    fn register_functions(&mut self, funs: &Vec<parse::Function>, state: &mut State) {
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
                        .report_internal_loc(param.loc, "No type associated to function parameter");
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
