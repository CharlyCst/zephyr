use super::names::*;
use super::types::id::*;
use super::types::{ConstraintStore, Type, TypeConstraint, TypeId, TypeVarStore};
use crate::ast;
use crate::error::{ErrorHandler, Location};

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

pub struct NameResolver<'a, 'b> {
    err: &'b mut ErrorHandler<'a>,
}

impl<'a, 'b> NameResolver<'a, 'b> {
    pub fn new(error_handler: &'b mut ErrorHandler<'a>) -> NameResolver<'a, 'b> {
        NameResolver { err: error_handler }
    }

    pub fn resolve(&mut self, funs: Vec<ast::Function>) -> ResolvedProgram {
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

    fn resolve_function(&mut self, fun: ast::Function, state: &mut State) -> Option<Function> {
        state.new_scope();
        let mut locals = Vec::new();
        let mut fun_params = Vec::new();

        // Get the function name from global context. All functions must have been registered at that point.
        let fun_name = if let Some(name) = state.find_in_context(&fun.ident) {
            name
        } else {
            self.err
                .report_internal(fun.loc, String::from("Function name is not yet in context"));
            state.exit_scope();
            return None;
        };
        let fun_t_id = fun_name.t_id;
        let fun_n_id = fun_name.n_id;

        for param in fun.params.into_iter() {
            let t = match get_var_type(&param) {
                Some(t) => t,
                None => {
                    self.err
                        .report(param.loc, String::from("Missing or unrecognized type"));
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
                    // TODO: find a way to indicate line of definition
                    let error = format!("Name {} already defined in current context", fun.ident);
                    self.err.report(fun.loc, error);
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
        block: ast::Block,
        state: &mut State,
        locals: &mut Vec<NameId>,
        fun_t_id: TypeId,
    ) -> Block {
        state.new_scope();
        let mut stmts = Vec::new();

        for stmt in block.stmts.into_iter() {
            let named_stmt = match stmt {
                ast::Statement::AssignStmt { var, mut expr } => {
                    let (var, var_t_id) = if let Some(name) = state.find_in_context(&var.ident) {
                        let var = Variable {
                            ident: var.ident,
                            loc: var.loc,
                            n_id: name.n_id,
                        };
                        (var, name.t_id)
                    } else {
                        self.err.report(
                            var.loc,
                            format!(
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
                ast::Statement::LetStmt { var, mut expr } => {
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
                            // TODO: find a way to indicate line of duplicate
                            let error =
                                format!("Name {} already defined in current context", var.ident,);
                            self.err.report(var.loc, error);
                            continue;
                        }
                    }
                }
                ast::Statement::IfStmt { mut expr, block } => {
                    let (expr, expr_t_id) = self.resolve_expression(&mut expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    let block = self.resolve_block(block, state, locals, fun_t_id);
                    Statement::IfStmt {
                        expr: Box::new(expr),
                        block: block,
                    }
                }
                ast::Statement::WhileStmt { mut expr, block } => {
                    let (expr, expr_t_id) = self.resolve_expression(&mut expr, state);
                    state.new_constraint(TypeConstraint::Equality(expr_t_id, T_ID_BOOL));
                    let block = self.resolve_block(block, state, locals, fun_t_id);
                    Statement::WhileStmt {
                        expr: Box::new(expr),
                        block: block,
                    }
                }
                ast::Statement::ReturnStmt { expr, loc } => {
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
                ast::Statement::ExprStmt { mut expr } => {
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
        expr: &mut ast::Expression,
        state: &mut State,
    ) -> (Expression, TypeId) {
        match expr {
            ast::Expression::Unary { unop, expr } => {
                let (expr, t_id) = self.resolve_expression(expr, state);
                match unop {
                    ast::UnaryOperator::Minus => {
                        state.new_constraint(TypeConstraint::Included(t_id, T_ID_NUMERIC));
                        let expr = Expression::Unary {
                            expr: Box::new(expr),
                            unop: *unop,
                            t_id: t_id,
                        };
                        (expr, t_id)
                    }
                    ast::UnaryOperator::Not => {
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
            ast::Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let (left_expr, left_t_id) = self.resolve_expression(expr_left, state);
                let (right_expr, right_t_id) = self.resolve_expression(expr_right, state);
                match binop {
                    ast::BinaryOperator::Remainder => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_INTEGER));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: left_t_id,
                            op_t_id: left_t_id,
                        };
                        (expr, left_t_id)
                    }
                    ast::BinaryOperator::Plus
                    | ast::BinaryOperator::Multiply
                    | ast::BinaryOperator::Minus
                    | ast::BinaryOperator::Divide => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: left_t_id,
                            op_t_id: left_t_id,
                        };
                        (expr, left_t_id)
                    }
                    ast::BinaryOperator::Greater
                    | ast::BinaryOperator::GreaterEqual
                    | ast::BinaryOperator::Less
                    | ast::BinaryOperator::LessEqual => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_NUMERIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: T_ID_BOOL,
                            op_t_id: left_t_id,
                        };
                        (expr, T_ID_BOOL)
                    }
                    ast::BinaryOperator::Equal => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id));
                        state.new_constraint(TypeConstraint::Included(left_t_id, T_ID_BASIC));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            t_id: T_ID_BOOL,
                            op_t_id: left_t_id,
                        };
                        (expr, T_ID_BOOL)
                    }
                    _ => panic!("Binop not implemented yet"), // TODO
                }
            }
            ast::Expression::Literal { value } => match value {
                ast::Value::Integer { val, loc } => {
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
                ast::Value::Boolean { val, loc } => {
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
            ast::Expression::Variable { var } => {
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
                    self.err.report(
                        var.loc,
                        format!("Variable {} used but not declared", var.ident),
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
    fn register_functions(&mut self, funs: &Vec<ast::Function>, state: &mut State) {
        for fun in funs {
            let mut params = Vec::new();
            for param in fun.params.iter() {
                if let Some(t) = &param.t {
                    if let Some(known_t) = check_built_in_type(&t) {
                        params.push(known_t);
                    } else {
                        self.err
                            .report(param.loc, format!("Unknown parameter type: {}", t));
                    }
                } else {
                    self.err.report_internal(
                        param.loc,
                        String::from("No type associated to function parameter"),
                    );
                }
            }

            let mut results = Vec::new();
            if let Some((t, loc)) = &fun.result {
                if let Some(known_t) = check_built_in_type(&t) {
                    results.push(known_t);
                } else {
                    self.err.report(*loc, format!("Unknown result type: {}", t));
                }
            }

            if let Err(decl_loc) =
                state.declare(fun.ident.clone(), vec![Type::Fun(params, results)], fun.loc)
            {
                let error = format!("Function {} declared multiple times", fun.ident);
                self.err.report(fun.loc, error);
            }
        }
    }
}

fn get_var_type(var: &ast::Variable) -> Option<Vec<Type>> {
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
