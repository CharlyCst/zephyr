use super::hir::*;
use super::names::{
    Block as NameBlock, Body as NameBody, Expression as Expr, Function as NameFun,
    FunctionPrototype as NameFunProto, Imports as NameImports, NameStore, Statement as S,
    Value as V, Variable as NameVariable,
};
use super::types::{Type as ASTTypes, TypeStore, TypedProgram};

use crate::ast::{BinaryOperator as ASTBinop, UnaryOperator as ASTUnop};
use crate::error::{ErrorHandler, Location};

struct State {
    pub names: NameStore,
    pub types: TypeStore,
}

impl State {
    pub fn new(names: NameStore, types: TypeStore) -> State {
        State { names, types }
    }
}

pub struct HirProducer<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> HirProducer<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> HirProducer {
        HirProducer { err: error_handler }
    }

    /// Lower a typed program to HIR
    pub fn reduce(&mut self, prog: TypedProgram) -> Program {
        let mut state = State::new(prog.names, prog.types);
        let mut funs = Vec::with_capacity(prog.funs.len());
        let mut imports = Vec::with_capacity(prog.imports.len());

        for fun in prog.funs {
            match self.reduce_fun(fun, &mut state) {
                Ok(fun) => funs.push(fun),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        for import in prog.imports {
            match self.reduce_import(import, &mut state) {
                Ok(proto) => imports.push(proto),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        Program {
            funs,
            imports: imports,
            pub_decls: prog.pub_decls,
            package: prog.package,
        }
    }

    fn reduce_fun(&mut self, fun: NameFun, s: &mut State) -> Result<Function, String> {
        let fun_name = s.names.get(fun.n_id);
        let (param_t, ret_t) = if let ASTTypes::Fun(param_t, ret_t) = s.types.get(fun_name.t_id) {
            let param_t: Result<Vec<Type>, String> =
                param_t.into_iter().map(|t| to_hir_t(t)).collect();
            let ret_t: Result<Vec<Type>, String> = ret_t.into_iter().map(|t| to_hir_t(t)).collect();
            (param_t?, ret_t?)
        } else {
            self.err.report_internal(
                fun.loc,
                String::from("Function does not have function type"),
            );
            (vec![], vec![])
        };
        let t = FunctionType::new(param_t, ret_t);
        let params = fun.params.iter().map(|p| p.n_id).collect();
        let locals = self.get_locals(&fun, s)?;
        let body = match fun.body {
            NameBody::Zephyr(block) => Body::Zephyr(self.reduce_block(block, s)?),
            NameBody::Asm(stmts) => Body::Asm(stmts),
        };

        Ok(Function {
            ident: fun.ident,
            params,
            locals,
            body,
            t,
            loc: fun.loc,
            is_pub: fun.is_pub,
            exposed: fun.exposed,
            fun_id: fun.fun_id,
        })
    }

    /// Return a vector of local variables given a function name.
    fn get_locals(&mut self, fun: &NameFun, s: &State) -> Result<Vec<LocalVariable>, String> {
        let mut locals = Vec::new();
        for local_name in &fun.locals {
            let local = s.names.get(*local_name);
            let t_id = local.t_id;
            let loc = local.loc;
            let t = match s.types.get(t_id) {
                ASTTypes::I32 => Type::Scalar(ScalarType::I32),
                ASTTypes::I64 => Type::Scalar(ScalarType::I64),
                ASTTypes::F32 => Type::Scalar(ScalarType::F32),
                ASTTypes::F64 => Type::Scalar(ScalarType::F64),
                ASTTypes::Bool => Type::Scalar(ScalarType::I32),
                _ => return Err(format!("Invalid parameter type for t_id {}", t_id)),
            };
            locals.push(LocalVariable {
                id: *local_name,
                t,
                loc,
            })
        }

        Ok(locals)
    }

    fn reduce_block(&mut self, block: NameBlock, s: &State) -> Result<Block, String> {
        let mut stmts = Vec::new();
        for stmt in block.stmts {
            stmts.push(self.reduce_stmt(stmt, s)?);
        }
        let reduced_block = Block { stmts };
        Ok(reduced_block)
    }

    fn reduce_stmt(&mut self, stmt: S, s: &State) -> Result<Statement, String> {
        match stmt {
            S::AssignStmt { var, expr } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                let var = Box::new(self.reduce_var(*var, s)?);
                Ok(Statement::AssignStmt { var, expr })
            }
            S::LetStmt { var, expr } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                let var = Box::new(self.reduce_var(*var, s)?);
                Ok(Statement::LetStmt { expr, var })
            }
            S::ExprStmt { expr } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                Ok(Statement::ExprStmt { expr })
            }
            S::ReturnStmt { expr, loc } => {
                let expr = if let Some(expr) = expr {
                    Some(self.reduce_expr(expr, s)?)
                } else {
                    None
                };
                Ok(Statement::ReturnStmt { expr, loc })
            }
            S::WhileStmt { expr, block } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                let block = self.reduce_block(block, s)?;
                Ok(Statement::WhileStmt { expr, block })
            }
            S::IfStmt {
                expr,
                block,
                else_block,
            } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                let block = self.reduce_block(block, s)?;
                let else_block = if let Some(else_block) = else_block {
                    Some(self.reduce_block(else_block, s)?)
                } else {
                    None
                };
                Ok(Statement::IfStmt {
                    expr,
                    block,
                    else_block,
                })
            }
        }
    }

    fn reduce_expr(&mut self, expression: Expr, s: &State) -> Result<Expression, String> {
        match expression {
            Expr::Literal { value } => Ok(Expression::Literal {
                value: match value {
                    V::Integer { val, t_id, loc } => match s.types.get(t_id) {
                        ASTTypes::I32 => Value::I32(val as i32, loc),
                        ASTTypes::I64 => Value::I64(val as i64, loc),
                        _ => return Err(String::from("Integer constant of non integer type.")),
                    },
                    V::Float { val, t_id, loc } => match s.types.get(t_id) {
                        ASTTypes::F32 => Value::F32(val as f32, loc),
                        ASTTypes::F64 => Value::F64(val, loc),
                        _ => return Err(String::from("Float constant of non float type.")),
                    },
                    V::Boolean { val, t_id, loc } => match s.types.get(t_id) {
                        ASTTypes::Bool => Value::Bool(val, loc),
                        _ => return Err(String::from("Boolean constant of non boolean type.")),
                    },
                },
            }),
            Expr::Variable { var } => {
                let name = s.names.get(var.n_id);
                let t = s.types.get(name.t_id);
                Ok(Expression::Variable {
                    var: Variable {
                        ident: var.ident,
                        loc: var.loc,
                        n_id: var.n_id,
                        t: to_hir_t(t)?,
                    },
                })
            }
            Expr::Function { .. } => Err(String::from(
                "Function as expression are not yet supported.",
            )),
            Expr::Binary {
                expr_left,
                binop,
                expr_right,
                op_t_id,
                loc,
                ..
            } => {
                let t = s.types.get(op_t_id);
                let t = to_hir_scalar(t)?;
                let expr_left = Box::new(self.reduce_expr(*expr_left, s)?);
                let expr_right = Box::new(self.reduce_expr(*expr_right, s)?);
                Ok(Expression::Binary {
                    expr_right,
                    expr_left,
                    loc,
                    binop: match binop {
                        ASTBinop::Plus => Binop::Add(self.t_as_numeric(&t, loc)),
                        ASTBinop::Minus => Binop::Sub(self.t_as_numeric(&t, loc)),
                        ASTBinop::Multiply => Binop::Mul(self.t_as_numeric(&t, loc)),
                        ASTBinop::Divide => Binop::Div(self.t_as_numeric(&t, loc)),
                        ASTBinop::Equal => Binop::Eq(t),
                        ASTBinop::NotEqual => Binop::Ne(t),
                        ASTBinop::Greater => Binop::Gt(self.t_as_numeric(&t, loc)),
                        ASTBinop::GreaterEqual => Binop::Ge(self.t_as_numeric(&t, loc)),
                        ASTBinop::Less => Binop::Lt(self.t_as_numeric(&t, loc)),
                        ASTBinop::LessEqual => Binop::Le(self.t_as_numeric(&t, loc)),
                        ASTBinop::Remainder => Binop::Rem(self.t_as_integer(&t, loc)),
                        ASTBinop::BitwiseOr => Binop::BinaryOr(self.t_as_integer(&t, loc)),
                        ASTBinop::BitwiseAnd => Binop::BinaryAnd(self.t_as_integer(&t, loc)),
                        ASTBinop::BitwiseXor => Binop::Xor(self.t_as_integer(&t, loc)),
                        ASTBinop::Or => {
                            self.t_is_bool(&t, loc);
                            Binop::LogicalOr
                        }
                        ASTBinop::And => {
                            self.t_is_bool(&t, loc);
                            Binop::LogicalAnd
                        }
                    },
                })
            }
            Expr::Unary {
                unop,
                expr,
                op_t_id,
                loc,
            } => {
                let t = s.types.get(op_t_id);
                let t = to_hir_scalar(t)?;
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                Ok(Expression::Unary {
                    expr,
                    unop: match unop {
                        ASTUnop::Not => {
                            self.t_is_bool(&t, loc);
                            Unop::Not
                        }
                        ASTUnop::Minus => Unop::Neg(self.t_as_numeric(&t, loc)),
                    },
                    loc,
                })
            }
            Expr::CallDirect {
                fun_id,
                args,
                loc,
                fun_t_id,
                ..
            } => {
                let t = s.types.get(fun_t_id);
                let t = to_hir_fun(t)?;
                let mut hir_args = Vec::new();
                for arg in args {
                    hir_args.push(self.reduce_expr(arg, s)?);
                }
                Ok(Expression::CallDirect {
                    fun_id,
                    t,
                    loc,
                    args: hir_args,
                })
            }
            Expr::CallIndirect { .. } => {
                Err(String::from("Indirect calls are not yet implemented."))
            }
        }
    }

    fn reduce_var(&self, var: NameVariable, s: &State) -> Result<Variable, String> {
        let name = s.names.get(var.n_id);
        let t = s.types.get(name.t_id);
        let t = to_hir_t(t)?;
        Ok(Variable {
            ident: var.ident,
            loc: var.loc,
            n_id: var.n_id,
            t,
        })
    }

    fn reduce_import(&mut self, imports: NameImports, s: &mut State) -> Result<Imports, String> {
        let mut prototypes = Vec::with_capacity(imports.prototypes.len());
        for proto in imports.prototypes {
            prototypes.push(self.reduce_prototype(proto, s)?);
        }
        Ok(Imports {
            from: imports.from,
            prototypes,
            loc: imports.loc,
        })
    }

    fn reduce_prototype(
        &mut self,
        proto: NameFunProto,
        s: &mut State,
    ) -> Result<FunctionPrototype, String> {
        let fun_name = s.names.get(proto.n_id);
        let (param_t, ret_t) = if let ASTTypes::Fun(param_t, ret_t) = s.types.get(fun_name.t_id) {
            let param_t: Result<Vec<Type>, String> =
                param_t.into_iter().map(|t| to_hir_t(t)).collect();
            let ret_t: Result<Vec<Type>, String> = ret_t.into_iter().map(|t| to_hir_t(t)).collect();
            (param_t?, ret_t?)
        } else {
            self.err.report_internal(
                proto.loc,
                String::from("Imported function does not have function type"),
            );
            (vec![], vec![])
        };
        let t = FunctionType::new(param_t, ret_t);
        Ok(FunctionPrototype {
            ident: proto.ident,
            alias: proto.alias,
            is_pub: proto.is_pub,
            loc: proto.loc,
            fun_id: proto.fun_id,
            t,
        })
    }

    /// Verify that t is a boolean type, raises an error otherwhise.
    fn t_is_bool(&mut self, t: &ScalarType, loc: Location) {
        match t {
            ScalarType::Bool => (),
            _ => self
                .err
                .report_internal(loc, format!("Expected boolean, got {}.", t)),
        }
    }

    /// Verify that t is an integer type, raises an error otherwhise and
    /// return an arbitrary integer type.
    fn t_as_integer(&mut self, t: &ScalarType, loc: Location) -> IntegerType {
        match t {
            ScalarType::I32 => IntegerType::I32,
            ScalarType::I64 => IntegerType::I64,
            _ => {
                self.err
                    .report_internal(loc, format!("Expected an integer, got {}.", t));
                IntegerType::I32
            }
        }
    }

    /// Verify that t is a numeric type, raises an error otherwhise and
    /// return an arbitrary numeric type.
    fn t_as_numeric(&mut self, t: &ScalarType, loc: Location) -> NumericType {
        match t {
            ScalarType::I32 => NumericType::I32,
            ScalarType::I64 => NumericType::I64,
            ScalarType::F32 => NumericType::F32,
            ScalarType::F64 => NumericType::F64,
            _ => {
                self.err
                    .report_internal(loc, format!("Expected a number, got {}.", t));
                NumericType::I32
            }
        }
    }
}

/// Convert an AST Type into its HIR equivalent.
fn to_hir_t(t: &ASTTypes) -> Result<Type, String> {
    match t {
        ASTTypes::Any | ASTTypes::Bug | ASTTypes::Unit => {
            Err(format!("Invalid type in MIR generation: {}", t))
        }
        ASTTypes::I32 => Ok(Type::Scalar(ScalarType::I32)),
        ASTTypes::I64 => Ok(Type::Scalar(ScalarType::I64)),
        ASTTypes::F32 => Ok(Type::Scalar(ScalarType::F32)),
        ASTTypes::F64 => Ok(Type::Scalar(ScalarType::F64)),
        ASTTypes::Bool => Ok(Type::Scalar(ScalarType::Bool)),
        ASTTypes::Fun(_, _) => Err(String::from("Function as a value are not yet implemented")),
    }
}

/// Convert an AST Type into its scalar HIR equivalent.
fn to_hir_scalar(t: &ASTTypes) -> Result<ScalarType, String> {
    match t {
        ASTTypes::Any | ASTTypes::Bug | ASTTypes::Unit => {
            Err(format!("Invalid type in MIR generation: {}", t))
        }
        ASTTypes::I32 => Ok(ScalarType::I32),
        ASTTypes::I64 => Ok(ScalarType::I64),
        ASTTypes::F32 => Ok(ScalarType::F32),
        ASTTypes::F64 => Ok(ScalarType::F64),
        ASTTypes::Bool => Ok(ScalarType::Bool),
        ASTTypes::Fun(_, _) => Err(String::from("Function as a value are not yet implemented")),
    }
}

/// Convert an AST Type into its equivalent HIR type, if possible.
fn to_hir_fun(t: &ASTTypes) -> Result<FunctionType, String> {
    match t {
        ASTTypes::Fun(args, ret) => {
            let mut hir_args = Vec::new();
            let mut hir_ret = Vec::new();
            for t in args {
                hir_args.push(to_hir_t(t)?);
            }
            for t in ret {
                hir_ret.push(to_hir_t(t)?);
            }
            Ok(FunctionType {
                params: hir_args,
                ret: hir_ret,
            })
        }
        t => Err(format!("Expected function type, got {}", t)),
    }
}
