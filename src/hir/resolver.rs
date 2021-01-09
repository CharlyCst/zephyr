use super::names::*;
use super::types::id::*;
use super::types::{ConstraintStore, Type, TypeConstraint, TypeId, TypeVarStore};
use crate::ast;
use crate::driver::PublicDeclarations;
use crate::error::{ErrorHandler, Location};

use std::collections::HashMap;

struct State {
    names: NameStore,
    types: TypeVarStore,
    contexts: Vec<HashMap<String, usize>>,
    value_namespace: HashMap<String, ValueKind>,
    type_namespace: HashMap<String, TypeKind>,
    used_namespace: PublicDeclarations,
    constraints: ConstraintStore,
    fun_counter: u32,
    struct_counter: u32,
    package_id: u32,
}

impl State {
    pub fn new(package_id: u32, used_namespace: PublicDeclarations) -> State {
        let contexts = vec![HashMap::new()];
        State {
            names: NameStore::new(),
            types: TypeVarStore::new(),
            contexts,
            value_namespace: HashMap::new(),
            type_namespace: HashMap::new(),
            used_namespace,
            constraints: ConstraintStore::new(),
            fun_counter: 0,
            struct_counter: 0,
            package_id,
        }
    }

    /// Starts a new scope.
    pub fn new_scope(&mut self) {
        self.contexts.push(HashMap::new());
    }

    /// Exit the current scope.
    pub fn exit_scope(&mut self) {
        self.contexts.pop();
    }

    /// Return a fresh function ID.
    pub fn fresh_f_id(&mut self) -> FunId {
        let f_id = (self.fun_counter as u64) + ((self.package_id as u64) << 32);
        self.fun_counter += 1;
        f_id
    }

    /// Return a fresh Struct ID.
    pub fn fresh_s_id(&mut self) -> StructId {
        let s_id = (self.struct_counter as u64) + ((self.package_id as u64) << 32);
        self.struct_counter += 1;
        s_id
    }

    /// Declare a name, will fail if the name already exists in the current context or corresponds
    /// to an import alias.
    pub fn declare(
        &mut self,
        ident: String,
        type_candidates: Vec<Type>,
        loc: Location,
    ) -> Result<(NameId, TypeId), Location> {
        if let Some(n) = self.find_in_context(&ident) {
            return Err(n.loc);
        } else if let Some(_) = self.used_namespace.get(&ident) {
            return Err(Location::dummy()); // TODO: get the location of the corresponding `use` statement.
        }

        let ident_key = ident.clone();
        let t_id = self.types.fresh(loc, type_candidates);
        let n_id = self.names.fresh(ident, loc, t_id);
        self.add_in_context(ident_key, n_id);
        Ok((n_id, t_id))
    }

    /// Adds a new contraints, will be used in the type checking stage.
    pub fn new_constraint(&mut self, constraint: TypeConstraint) {
        self.constraints.add(constraint)
    }

    /// Return the corresponding Name if it is in context. This does not include used alias.
    pub fn find_in_context(&self, ident: &str) -> Option<&Name> {
        for ctx in self.contexts.iter().rev() {
            match ctx.get(ident) {
                Some(id) => return Some(self.names.get(*id)),
                None => (),
            }
        }
        None
    }

    /// Adds a new declaration in the current context.
    fn add_in_context(&mut self, name: String, id: usize) {
        match self.contexts.last_mut() {
            Some(ctx) => ctx.insert(name, id),
            None => panic!("Empty context in name resolution"),
        };
    }
}

pub struct NameResolver<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> NameResolver<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> NameResolver {
        NameResolver { err: error_handler }
    }

    pub fn resolve(
        &mut self,
        ast_program: ast::Program,
        used_namespace: PublicDeclarations,
    ) -> ResolvedProgram {
        let funs = ast_program.funs;
        let mut state = State::new(ast_program.package.id, used_namespace);
        let mut named_funs = Vec::with_capacity(funs.len());

        // Register functions names and signatures
        let imports = self.register_and_resolve_imports(
            ast_program.imports,
            ast_program.package.kind,
            &mut state,
        );
        let structs = self.register_and_resolve_structs(ast_program.structs, &mut state);
        self.register_functions(&funs, &mut state);

        // Resolve exposed funs
        let exposed_funs = self.resolve_exports(ast_program.exposed, &state);

        // Resolve function bodies
        for fun in funs.into_iter() {
            if let Some(named_fun) = self.resolve_function(fun, &exposed_funs, &mut state) {
                named_funs.push(named_fun);
            }
        }

        ResolvedProgram {
            funs: named_funs,
            structs,
            imports,
            names: state.names,
            types: state.types,
            constraints: state.constraints,
            package: ast_program.package,
        }
    }

    /// Check that each names used inside the function are correctly defined.
    /// Also responsible for checking if the function is exposed.
    fn resolve_function(
        &mut self,
        fun: ast::Function,
        exposed_funs: &HashMap<FunId, String>,
        state: &mut State,
    ) -> Option<Function> {
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
        let f_id = if let Some(ValueKind::Function(f_id, _)) = state.value_namespace.get(&fun.ident)
        {
            *f_id
        } else {
            self.err.report_internal(
                fun.loc,
                format!(
                    "Function name '{}' is not registered in resolver state.",
                    &fun.ident
                ),
            );
            state.exit_scope();
            return None;
        };

        for param in fun.params.into_iter() {
            let t = vec![self.get_type(&param.t, param.loc, state)];

            match state.declare(param.ident.clone(), t, param.loc) {
                Ok((n_id, _)) => fun_params.push(Variable {
                    ident: param.ident,
                    loc: param.loc,
                    n_id,
                }),
                Err(decl_loc) => {
                    let error = format!("Name {} already defined in current context", param.ident);
                    self.err.report(fun.loc, error);
                    let error = format!("Name {} already defined in current context", param.ident);
                    self.err.report(decl_loc, error);
                }
            }
        }

        let exposed = if let Some(exposed_name) = exposed_funs.get(&f_id) {
            Some(exposed_name.clone())
        } else {
            None
        };

        match fun.body {
            ast::Body::Zephyr(block) => {
                let block = self.resolve_block(block, state, &mut locals, fun_t_id);
                state.exit_scope();

                Some(Function {
                    ident: fun.ident,
                    params: fun_params,
                    locals,
                    body: Body::Zephyr(block),
                    is_pub: fun.is_pub,
                    exposed,
                    loc: fun.loc,
                    n_id: fun_n_id,
                    fun_id: f_id,
                })
            }
            ast::Body::Asm(stmts) => {
                let stmts = self.resolve_asm(stmts, state);
                state.exit_scope();

                Some(Function {
                    ident: fun.ident,
                    params: fun_params,
                    locals,
                    body: Body::Asm(stmts),
                    is_pub: fun.is_pub,
                    exposed,
                    loc: fun.loc,
                    n_id: fun_n_id,
                    fun_id: f_id,
                })
            }
        }
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
            let named_stmt = match self.resolve_stmt(stmt, state, locals, fun_t_id) {
                Ok(stmt) => stmt,
                Err(()) => {
                    self.err.silent_report();
                    continue;
                }
            };
            stmts.push(named_stmt);
        }

        state.exit_scope();
        Block { stmts }
    }

    fn resolve_stmt(
        &mut self,
        stmt: ast::Statement,
        state: &mut State,
        locals: &mut Vec<NameId>,
        fun_t_id: TypeId,
    ) -> Result<Statement, ()> {
        let stmt = match stmt {
            ast::Statement::AssignStmt {
                mut target,
                mut expr,
            } => {
                let (target, target_t_id) = self.resolve_expression(&mut target, state)?;
                let (expr, expr_id) = self.resolve_expression(&mut expr, state)?;
                let loc = target.get_loc().merge(expr.get_loc());
                state.new_constraint(TypeConstraint::Equality(target_t_id, expr_id, loc));
                Statement::AssignStmt {
                    target: Box::new(target),
                    expr: Box::new(expr),
                }
            }
            ast::Statement::LetStmt { var, mut expr } => {
                match state.declare(var.ident.clone(), vec![Type::Any], var.loc) {
                    Ok((n_id, var_t_id)) => {
                        locals.push(n_id);
                        let (expr, expr_t_id) = self.resolve_expression(&mut expr, state)?;
                        let loc = var.loc.merge(expr.get_loc());
                        state.new_constraint(TypeConstraint::Equality(var_t_id, expr_t_id, loc));
                        Statement::LetStmt {
                            var: Box::new(Variable {
                                ident: var.ident,
                                loc: var.loc,
                                n_id,
                            }),
                            expr: Box::new(expr),
                        }
                    }
                    Err(_decl_loc) => {
                        // TODO: find a way to indicate line of duplicate
                        let error =
                            format!("Name {} already defined in current context", var.ident,);
                        self.err.report(var.loc, error);
                        return Err(());
                    }
                }
            }
            ast::Statement::IfStmt {
                mut expr,
                block,
                else_block,
            } => {
                let (expr, expr_t_id) = self.resolve_expression(&mut expr, state)?;
                state.new_constraint(TypeConstraint::Equality(
                    expr_t_id,
                    T_ID_BOOL,
                    expr.get_loc(),
                ));
                let block = self.resolve_block(block, state, locals, fun_t_id);
                let else_block = if let Some(else_block) = else_block {
                    let else_block = self.resolve_block(else_block, state, locals, fun_t_id);
                    Some(else_block)
                } else {
                    None
                };
                Statement::IfStmt {
                    expr: Box::new(expr),
                    block,
                    else_block,
                }
            }
            ast::Statement::WhileStmt { mut expr, block } => {
                let (expr, expr_t_id) = self.resolve_expression(&mut expr, state)?;
                state.new_constraint(TypeConstraint::Equality(
                    expr_t_id,
                    T_ID_BOOL,
                    expr.get_loc(),
                ));
                let block = self.resolve_block(block, state, locals, fun_t_id);
                Statement::WhileStmt {
                    expr: Box::new(expr),
                    block,
                }
            }
            ast::Statement::ReturnStmt { expr, loc } => {
                if let Some(mut ret_expr) = expr {
                    let (expr, ret_t_id) = self.resolve_expression(&mut ret_expr, state)?;
                    state.new_constraint(TypeConstraint::Return(
                        fun_t_id,
                        ret_t_id,
                        expr.get_loc(),
                    ));
                    Statement::ReturnStmt {
                        expr: Some(expr),
                        loc,
                    }
                } else {
                    let ret_t_id = state.types.fresh(loc, vec![Type::Unit]);
                    state.new_constraint(TypeConstraint::Return(
                        fun_t_id,
                        ret_t_id,
                        Location::dummy(), // TODO: how to deal with empty expressions?
                    ));
                    Statement::ReturnStmt { expr: None, loc }
                }
            }
            ast::Statement::ExprStmt { mut expr } => {
                let (expr, _) = self.resolve_expression(&mut expr, state)?;
                Statement::ExprStmt {
                    expr: Box::new(expr),
                }
            }
        };
        Ok(stmt)
    }

    fn resolve_expression(
        &mut self,
        expr: &mut ast::Expression,
        state: &mut State,
    ) -> Result<(Expression, TypeId), ()> {
        match expr {
            ast::Expression::Unary { unop, expr } => {
                let (expr, op_t_id) = self.resolve_expression(expr, state)?;
                match unop {
                    ast::UnaryOperator::Minus => {
                        let loc = expr.get_loc();
                        state.new_constraint(TypeConstraint::Included(op_t_id, T_ID_NUMERIC, loc));
                        let expr = Expression::Unary {
                            expr: Box::new(expr),
                            unop: *unop,
                            loc,
                            op_t_id,
                        };
                        Ok((expr, op_t_id))
                    }
                    ast::UnaryOperator::Not => {
                        let loc = expr.get_loc();
                        state.new_constraint(TypeConstraint::Included(op_t_id, T_ID_BOOL, loc));
                        let expr = Expression::Unary {
                            expr: Box::new(expr),
                            unop: *unop,
                            loc,
                            op_t_id,
                        };
                        Ok((expr, op_t_id))
                    }
                }
            }
            ast::Expression::Binary {
                expr_left,
                binop,
                expr_right,
            } => {
                let (left_expr, left_t_id) = self.resolve_expression(expr_left, state)?;
                let (right_expr, right_t_id) = self.resolve_expression(expr_right, state)?;
                let loc = left_expr.get_loc().merge(right_expr.get_loc());
                match binop {
                    ast::BinaryOperator::Remainder
                    | ast::BinaryOperator::BitwiseOr
                    | ast::BinaryOperator::BitwiseAnd
                    | ast::BinaryOperator::BitwiseXor => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id, loc));
                        state.new_constraint(TypeConstraint::Included(
                            left_t_id,
                            T_ID_INTEGER,
                            left_expr.get_loc(),
                        ));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            loc,
                            t_id: left_t_id,
                            op_t_id: left_t_id,
                        };
                        Ok((expr, left_t_id))
                    }
                    ast::BinaryOperator::Plus
                    | ast::BinaryOperator::Multiply
                    | ast::BinaryOperator::Minus
                    | ast::BinaryOperator::Divide => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id, loc));
                        state.new_constraint(TypeConstraint::Included(
                            left_t_id,
                            T_ID_NUMERIC,
                            left_expr.get_loc(),
                        ));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            loc,
                            t_id: left_t_id,
                            op_t_id: left_t_id,
                        };
                        Ok((expr, left_t_id))
                    }
                    ast::BinaryOperator::Greater
                    | ast::BinaryOperator::GreaterEqual
                    | ast::BinaryOperator::Less
                    | ast::BinaryOperator::LessEqual => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id, loc));
                        state.new_constraint(TypeConstraint::Included(
                            left_t_id,
                            T_ID_NUMERIC,
                            left_expr.get_loc(),
                        ));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            loc,
                            t_id: T_ID_BOOL,
                            op_t_id: left_t_id,
                        };
                        Ok((expr, T_ID_BOOL))
                    }
                    ast::BinaryOperator::Equal | ast::BinaryOperator::NotEqual => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id, loc));
                        state.new_constraint(TypeConstraint::Included(
                            left_t_id,
                            T_ID_BASIC,
                            left_expr.get_loc(),
                        ));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            loc,
                            t_id: T_ID_BOOL,
                            op_t_id: left_t_id,
                        };
                        Ok((expr, T_ID_BOOL))
                    }
                    ast::BinaryOperator::And | ast::BinaryOperator::Or => {
                        state.new_constraint(TypeConstraint::Equality(left_t_id, right_t_id, loc));
                        state.new_constraint(TypeConstraint::Equality(
                            left_t_id,
                            T_ID_BOOL,
                            left_expr.get_loc(),
                        ));
                        let expr = Expression::Binary {
                            expr_left: Box::new(left_expr),
                            binop: *binop,
                            expr_right: Box::new(right_expr),
                            loc,
                            t_id: T_ID_BOOL,
                            op_t_id: left_t_id,
                        };
                        Ok((expr, T_ID_BOOL))
                    }
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
                    Ok((expr, fresh_t_id))
                }
                ast::Value::Float { val, loc } => {
                    let fresh_t_id = state.types.fresh(*loc, vec![Type::F32, Type::F64]);
                    let expr = Expression::Literal {
                        value: Value::Float {
                            val: *val,
                            loc: *loc,
                            t_id: fresh_t_id,
                        },
                    };
                    Ok((expr, fresh_t_id))
                }
                ast::Value::Boolean { val, loc } => {
                    let expr = Expression::Literal {
                        value: Value::Boolean {
                            val: *val,
                            loc: *loc,
                            t_id: T_ID_BOOL,
                        },
                    };
                    Ok((expr, T_ID_BOOL))
                }
                ast::Value::Struct { ident, fields, loc } => {
                    let t = self.get_type(&ident, *loc, state);
                    let t_id = state.types.fresh(*loc, vec![t]);
                    let n = fields.len();
                    let mut hir_fields = Vec::with_capacity(n);
                    let mut field_constraints = Vec::with_capacity(n);
                    for field in fields {
                        let (expr, field_t_id) = self.resolve_expression(&mut field.expr, state)?;
                        hir_fields.push(FieldValue {
                            ident: field.ident.clone(),
                            expr: Box::new(expr),
                            t_id: field_t_id,
                            loc: field.loc,
                        });
                        field_constraints.push((field.ident.clone(), field_t_id, field.loc));
                    }
                    state.new_constraint(TypeConstraint::StructLiteral {
                        struct_t_id: t_id,
                        fields: field_constraints,
                        loc: *loc,
                    });
                    let expr = Expression::Literal {
                        value: Value::Struct {
                            ident: ident.clone(),
                            loc: *loc,
                            fields: hir_fields,
                            t_id,
                        },
                    };
                    Ok((expr, t_id))
                }
            },
            ast::Expression::Variable { var } => {
                if let Some(value) = state.value_namespace.get(&var.ident) {
                    match value {
                        ValueKind::Function(fun_id, n_id) => {
                            let expr = Expression::Function {
                                fun_id: *fun_id,
                                loc: var.loc,
                            };
                            let t_id = state.names.get(*n_id).t_id;
                            Ok((expr, t_id))
                        }
                    }
                } else if let Some(name) = state.find_in_context(&var.ident) {
                    let expr = Expression::Variable {
                        var: Variable {
                            ident: var.ident.clone(),
                            loc: var.loc,
                            n_id: name.n_id,
                        },
                    };
                    Ok((expr, name.t_id))
                } else if state.used_namespace.get(&var.ident).is_some() {
                    let expr = Expression::Namespace {
                        ident: var.ident.clone(),
                        loc: var.loc,
                    };
                    Ok((expr, T_ID_UNIT))
                } else {
                    self.err.report(
                        var.loc,
                        format!("Variable {} used but not declared", var.ident),
                    );
                    return Err(());
                }
            }
            ast::Expression::Call { fun, args } => {
                let n = args.len();
                let mut resolved_args = Vec::with_capacity(n);
                let mut args_t = Vec::with_capacity(n);
                let mut args_loc = Vec::with_capacity(n);
                for arg in args {
                    let (arg, arg_t) = self.resolve_expression(arg, state)?;
                    args_loc.push(arg.get_loc());
                    resolved_args.push(arg);
                    args_t.push(arg_t);
                }
                let (fun, fun_t_id) = self.resolve_expression(fun, state)?;
                let loc = if n > 0 {
                    fun.get_loc().merge(resolved_args[n - 1].get_loc())
                } else {
                    fun.get_loc()
                };
                match fun {
                    Expression::Function { fun_id, .. } => {
                        // Direct call
                        let ret_t_id = state.types.fresh(fun.get_loc(), vec![Type::Any]);
                        state.new_constraint(TypeConstraint::Arguments(
                            args_t, fun_t_id, args_loc, loc,
                        ));
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id, loc));
                        let expr = Expression::CallDirect {
                            fun_id,
                            loc,
                            args: resolved_args,
                            fun_t_id,
                            ret_t_id,
                        };
                        Ok((expr, ret_t_id))
                    }
                    _ => {
                        // Indirect call
                        let ret_t_id = state.types.fresh(fun.get_loc(), vec![Type::Any]);
                        state.new_constraint(TypeConstraint::Arguments(
                            args_t, fun_t_id, args_loc, loc,
                        ));
                        state.new_constraint(TypeConstraint::Return(fun_t_id, ret_t_id, loc));
                        let expr = Expression::CallIndirect {
                            loc,
                            fun: Box::new(fun),
                            args: resolved_args,
                            fun_t_id,
                            ret_t_id,
                        };
                        Ok((expr, ret_t_id))
                    }
                }
            }
            ast::Expression::Access { namespace, field } => {
                let (field, loc_field) = match &(**field) {
                    ast::Expression::Variable { var } => (var.ident.clone(), var.loc),
                    _ => {
                        let (expr, _) = self.resolve_expression(field, state)?;
                        self.err.report(
                            expr.get_loc(),
                            String::from("The right operand of an access must be an identifier."),
                        );
                        return Err(());
                    }
                };
                let (expr, struct_t_id) = self.resolve_expression(namespace, state)?;
                match expr {
                    Expression::Variable { .. }
                    | Expression::Access { .. }
                    | Expression::Literal {
                        value: Value::Struct { .. },
                    } => {
                        // Access to a struct field
                        let field_t_id = state.types.fresh(loc_field, vec![Type::Any]);
                        state.new_constraint(TypeConstraint::Field(
                            struct_t_id,
                            field_t_id,
                            field.clone(),
                            loc_field,
                        ));
                        let expr = Expression::Access {
                            expr: Box::new(expr),
                            loc: loc_field,
                            t_id: field_t_id,
                            struct_t_id,
                            field,
                        };
                        Ok((expr, field_t_id))
                    }
                    Expression::Namespace { ref ident, loc } => {
                        // Namespace imported from another package
                        if let Some(namespace) = state.used_namespace.get(ident) {
                            let loc = loc_field;
                            match namespace.get_val(&field) {
                                Some(ValueDeclaration::Function { fun_id, t }) => {
                                    let expr = Expression::Function {
                                        fun_id: *fun_id,
                                        loc,
                                    };
                                    let t_id = state.types.fresh(loc, vec![t.clone()]);
                                    Ok((expr, t_id))
                                }
                                _ => {
                                    self.err.report(
                                        loc_field,
                                        format!("'{}' is not a member of '{}'.", &field, &ident),
                                    );
                                    return Err(());
                                }
                            }
                        } else {
                            self.err.report_internal(
                                loc,
                                format!("Namespace '{}' does not exist", ident),
                            );
                            return Err(());
                        }
                    }
                    _ => {
                        self.err.report(
                            expr.get_loc(),
                            String::from("The left operand of an access must be an identifier or struct literal."),
                        );
                        return Err(());
                    }
                }
            }
        }
    }

    fn resolve_asm(
        &mut self,
        stmts: Vec<ast::AsmStatement>,
        state: &mut State,
    ) -> Vec<AsmStatement> {
        let mut resolved_stmts = Vec::with_capacity(stmts.len());

        for stmt in stmts {
            match self.resolve_asm_statement(stmt, state) {
                Ok(stmt) => resolved_stmts.push(stmt),
                Err(_) => self.err.silent_report(),
            }
        }

        resolved_stmts
    }

    fn resolve_asm_statement(
        &mut self,
        stmt: ast::AsmStatement,
        state: &mut State,
    ) -> Result<AsmStatement, ()> {
        match stmt {
            ast::AsmStatement::Control { cntrl, loc } => Ok(AsmStatement::Control { cntrl, loc }),
            ast::AsmStatement::Memory { mem, loc } => Ok(AsmStatement::Memory { mem, loc }),
            ast::AsmStatement::Const { val, loc } => Ok(AsmStatement::Const { val, loc }),
            ast::AsmStatement::Parametric { param, loc } => {
                Ok(AsmStatement::Parametric { param, loc })
            }
            ast::AsmStatement::Local { local, loc } => match local {
                ast::AsmLocal::Get {
                    ident,
                    loc: arg_loc,
                } => match state.find_in_context(&ident) {
                    Some(name) => {
                        let var = Variable {
                            ident,
                            loc: arg_loc,
                            n_id: name.n_id,
                        };
                        Ok(AsmStatement::Local {
                            local: AsmLocal::Get { var },
                            loc,
                        })
                    }
                    None => {
                        self.err.report(
                            arg_loc,
                            format!("No variable '{}' in current scope.", &ident),
                        );
                        Err(())
                    }
                },
                ast::AsmLocal::Set {
                    ident,
                    loc: arg_loc,
                } => match state.find_in_context(&ident) {
                    Some(name) => {
                        let var = Variable {
                            ident,
                            loc: arg_loc,
                            n_id: name.n_id,
                        };
                        Ok(AsmStatement::Local {
                            local: AsmLocal::Set { var },
                            loc,
                        })
                    }
                    None => {
                        self.err.report(
                            arg_loc,
                            format!("No variable '{}' in current scope.", &ident),
                        );
                        Err(())
                    }
                },
            },
        }
    }

    /// Register top level functions into the global state (`state`).
    fn register_functions(&mut self, funs: &Vec<ast::Function>, state: &mut State) {
        for fun in funs {
            let mut params = Vec::new();
            for param in fun.params.iter() {
                params.push(self.get_type(&param.t, param.loc, state));
            }

            let mut results = Vec::new();
            if let Some((t, loc)) = &fun.result {
                let t = self.get_type(t, *loc, state);
                results.push(t);
            }

            let declaration =
                state.declare(fun.ident.clone(), vec![Type::Fun(params, results)], fun.loc);
            if let Err(_decl_loc) = declaration {
                let error = format!("Function {} declared multiple times", fun.ident);
                self.err.report(fun.loc, error);
            } else if let Ok((n_id, _)) = declaration {
                let fun_id = state.fresh_f_id();
                state
                    .value_namespace
                    .insert(fun.ident.clone(), ValueKind::Function(fun_id, n_id));
            }
        }
    }

    /// Register top level imports into the global state (`state`) and return resolved
    /// functions.
    /// Will rise an error if imports are declared in an unappropriate package.
    fn register_and_resolve_imports(
        &mut self,
        imports: Vec<ast::Imports>,
        package_kind: ast::PackageKind,
        state: &mut State,
    ) -> Vec<Imports> {
        let mut resolved_imports = Vec::with_capacity(imports.len());
        if package_kind != ast::PackageKind::Runtime && !imports.is_empty() {
            let loc = imports.first().unwrap().loc;
            self.err.report(
                loc,
                String::from("Function imports are only permitted in 'runtime' packages."),
            );
        }
        for import in imports {
            resolved_imports.push(Imports {
                from: import.from,
                prototypes: self.register_and_resolve_prototypes(import.prototypes, state),
                loc: import.loc,
            })
        }
        resolved_imports
    }

    /// Register top level prototypes definition into the global state (`state`) and return
    /// resolved functions.
    fn register_and_resolve_prototypes(
        &mut self,
        prototypes: Vec<ast::FunctionPrototype>,
        state: &mut State,
    ) -> Vec<FunctionPrototype> {
        let mut resolved_protos = Vec::with_capacity(prototypes.len());
        for proto in prototypes {
            let mut params = Vec::new();
            for param in proto.params.iter() {
                if let Some(known_t) = check_base_type(&param.t) {
                    params.push(known_t);
                } else {
                    self.err.report(param.loc, format!("Unexpected parameter type: {}. Only i32, i64, f32 and f64 can be used in import prototypes.", &param.t));
                }
            }

            let mut results = Vec::new();
            if let Some((t, loc)) = &proto.result {
                if let Some(known_t) = check_base_type(&t) {
                    results.push(known_t);
                } else {
                    self.err.report(*loc, format!("Unexpected return type: {}. Only i32, i64, f32 and f64 can be returned by imported functions.", t));
                }
            }

            let ident = if let Some(ref alias) = proto.alias {
                alias.clone()
            } else {
                proto.ident.clone()
            };

            match state.declare(ident.clone(), vec![Type::Fun(params, results)], proto.loc) {
                Ok((n_id, _)) => {
                    let fun_id = state.fresh_f_id();
                    state
                        .value_namespace
                        .insert(ident, ValueKind::Function(fun_id, n_id));
                    resolved_protos.push(FunctionPrototype {
                        ident: proto.ident,
                        is_pub: proto.is_pub,
                        alias: proto.alias,
                        fun_id,
                        n_id,
                        loc: proto.loc,
                    })
                }
                Err(_decl_loc) => {
                    let error = format!("Function {} declared multiple times", ident);
                    self.err.report(proto.loc, error);
                }
            }
        }
        resolved_protos
    }

    /// Register the top level structs in the Type namespace, then resolve the structs fields.
    fn register_and_resolve_structs(
        &mut self,
        structs: Vec<ast::Struct>,
        state: &mut State,
    ) -> HashMap<StructId, Struct> {
        let mut resolved_structs = HashMap::with_capacity(structs.len());
        for struc in &structs {
            self.register_struct(struc, state);
        }
        for struc in structs {
            if let Some(s) = self.resolve_struct(struc, state) {
                resolved_structs.insert(s.s_id, s);
            }
        }
        resolved_structs
    }

    /// Register a struct in the Type namespace.
    fn register_struct(&mut self, struc: &ast::Struct, state: &mut State) {
        let s_id = state.fresh_s_id();
        let exists = state
            .type_namespace
            .insert(struc.ident.clone(), TypeKind::Struct(s_id))
            .is_some();
        if exists {
            self.err.report(
                struc.loc,
                format!("Type {} is already defined", struc.ident),
            );
        }
    }

    /// Resolve the struct fields.
    fn resolve_struct(&mut self, struc: ast::Struct, state: &State) -> Option<Struct> {
        let mut fields = HashMap::with_capacity(struc.fields.len());
        let s_id = if let Some(TypeKind::Struct(s_id)) = state.type_namespace.get(&struc.ident) {
            *s_id
        } else {
            self.err.report_internal(
                struc.loc,
                format!(
                    "Struct {} has not been registered or has been overriden",
                    &struc.ident
                ),
            );
            return None;
        };

        for field in struc.fields {
            let loc = field.loc;
            let is_pub = field.is_pub;
            let t = self.get_type(&field.t, loc, state);
            fields.insert(field.ident, StructField { t, loc, is_pub });
        }

        Some(Struct {
            fields,
            s_id,
            ident: struc.ident,
            is_pub: struc.is_pub,
            loc: struc.loc,
        })
    }

    /// Resolve the exposed functions and return a map of function ID to their name.
    fn resolve_exports(
        &mut self,
        exposed: Vec<ast::Expose>,
        state: &State,
    ) -> HashMap<FunId, String> {
        let mut exposed_funs = HashMap::with_capacity(exposed.len());
        for fun in exposed {
            if let Some(ValueKind::Function(f_id, _)) = state.value_namespace.get(&fun.ident) {
                let exposed_name = if let Some(alias) = fun.alias {
                    alias
                } else {
                    fun.ident
                };
                exposed_funs.insert(*f_id, exposed_name);
            } else {
                self.err.report(
                    fun.loc,
                    format!("Exposed function '{}' is not defined.", &fun.ident),
                )
            }
        }
        exposed_funs
    }

    fn get_type(&mut self, t: &str, loc: Location, state: &State) -> Type {
        if let Some(t) = check_built_in_type(t) {
            t
        } else {
            if let Some(t) = state.type_namespace.get(t) {
                match t {
                    TypeKind::Struct(s_id) => Type::Struct(*s_id),
                }
            } else {
                self.err.report(loc, format!("Unknown type: '{}'", t));
                Type::Bug
            }
        }
    }
}

fn check_built_in_type(t: &str) -> Option<Type> {
    match t {
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "f32" => Some(Type::F32),
        "f64" => Some(Type::F64),
        "bool" => Some(Type::Bool),
        _ => None,
    }
}

/// Return the corresponding base type, if any.
/// Base types are i32, i64, f32, f64 and are the only types that
/// can be imported/exported at the time (i.e. before interface types)
fn check_base_type(t: &str) -> Option<Type> {
    match t {
        "i32" => Some(Type::I32),
        "i64" => Some(Type::I64),
        "f32" => Some(Type::F32),
        "f64" => Some(Type::F64),
        _ => None,
    }
}
