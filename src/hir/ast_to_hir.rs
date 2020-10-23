use super::hir::*;
use super::names::{
    Block as NameBlock, Body as NameBody, Expression as Expr, Function as NameFun, NameStore,
    Statement as S, Value as V, Variable as NameVariable,
};
use super::types::{Type as ASTTypes, TypeStore};
use super::TypedProgram;

use crate::ast::{BinaryOperator as ASTBinop, UnaryOperator as ASTUnop};
use crate::error::ErrorHandler;

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

        for fun in prog.funs.into_iter() {
            match self.reduce_fun(fun, &mut state) {
                Ok(fun) => funs.push(fun),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        Program {
            funs,
            pub_decls: prog.pub_decls,
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
                t_id,
                loc,
                ..
            } => {
                let t = s.types.get(t_id);
                let t = to_hir_scalar(t)?;
                let expr_left = Box::new(self.reduce_expr(*expr_left, s)?);
                let expr_right = Box::new(self.reduce_expr(*expr_right, s)?);
                Ok(Expression::Binary {
                    expr_right,
                    expr_left,
                    loc,
                    binop: match binop {
                        ASTBinop::Plus => Binop::Add(t),
                        ASTBinop::Minus => Binop::Sub(t),
                        ASTBinop::Multiply => Binop::Mul(t),
                        ASTBinop::Divide => Binop::Div(t),
                        ASTBinop::Remainder => Binop::Rem(t),
                        ASTBinop::Or => Binop::LogicalOr(t),
                        ASTBinop::And => Binop::LogicalAnd(t),
                        ASTBinop::BitwiseOr => Binop::BinaryOr(t),
                        ASTBinop::BitwiseAnd => Binop::BinaryAnd(t),
                        ASTBinop::Equal => Binop::Eq(t),
                        ASTBinop::NotEqual => Binop::Ne(t),
                        ASTBinop::Greater => Binop::Gt(t),
                        ASTBinop::GreaterEqual => Binop::Ge(t),
                        ASTBinop::Less => Binop::Lt(t),
                        ASTBinop::LessEqual => Binop::Le(t),
                    },
                })
            }
            Expr::Unary {
                unop,
                expr,
                t_id,
                loc,
            } => {
                let t = s.types.get(t_id);
                let t = to_hir_scalar(t)?;
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                Ok(Expression::Unary {
                    expr,
                    loc,
                    unop: match unop {
                        ASTUnop::Not => Unop::Not(t),
                        ASTUnop::Minus => Unop::Neg(t),
                    },
                })
            }
            Expr::CallDirect {
                fun_id,
                args,
                t_id,
                loc,
            } => {
                let t = s.types.get(t_id);
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
            Expr::CallIndirect {  .. } => {
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
