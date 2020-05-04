use super::mir::*;
use super::names::{
    Block as NameBlock, Expression as Expr, Function as NameFun, NameStore, Statement as S,
    Value as V,
};
use super::types::{Type as ASTTypes, TypeId, TypeStore};
use super::TypedProgram;

use crate::ast::{BinaryOperator as ASTBinop, UnaryOperator as ASTUnop};
use crate::error::ErrorHandler;
use std::convert::TryInto;

enum FromBinop {
    Binop(Binop),
    Relop(Relop),
}

struct State {
    pub names: NameStore,
    pub types: TypeStore,
    bb_id: BasicBlockId,
}

impl State {
    pub fn new(names: NameStore, types: TypeStore) -> State {
        State {
            names: names,
            types: types,
            bb_id: 0,
        }
    }

    pub fn fresh_bb_id(&mut self) -> BasicBlockId {
        let id = self.bb_id;
        self.bb_id += 1;
        id
    }
}

pub struct MIRProducer {
    error_handler: ErrorHandler,
}

impl MIRProducer {
    pub fn new() -> MIRProducer {
        MIRProducer {
            error_handler: ErrorHandler::new(),
        }
    }

    pub fn reduce(&mut self, prog: TypedProgram) -> Program {
        let mut state = State::new(prog.names, prog.types);
        let mut funs = Vec::with_capacity(prog.funs.len());

        for fun in prog.funs.into_iter() {
            match self.reduce_fun(fun, &mut state) {
                Ok(fun) => funs.push(fun),
                Err(err) => self.error_handler.report_internal(&err),
            }
        }

        Program { funs: funs }
    }

    fn reduce_fun(&mut self, fun: NameFun, s: &mut State) -> Result<Function, String> {
        let fun_name = s.names.get(fun.n_id);
        let (param_t, ret_t) = if let ASTTypes::Fun(param_t, ret_t) = s.types.get(fun_name.t_id) {
            let param_t: Result<Vec<Type>, String> =
                param_t.into_iter().map(|t| convert_type(t)).collect();
            let ret_t: Result<Vec<Type>, String> =
                ret_t.into_iter().map(|t| convert_type(t)).collect();
            (param_t?, ret_t?)
        } else {
            self.error_handler
                .report_internal_loc(fun.loc, "Function does not have function type");
            (vec![], vec![])
        };

        let params = fun.params.iter().map(|p| p.n_id).collect();
        let locals = self.get_locals(&fun, s)?;
        let block = self.reduce_block(fun.block, s)?;

        Ok(Function {
            ident: fun.ident,
            params: params,
            param_types: param_t,
            ret_types: ret_t,
            locals: locals,
            body: block,
            exported: fun.exported,
        })
    }

    fn get_locals(&mut self, fun: &NameFun, s: &State) -> Result<Vec<Local>, String> {
        let mut locals = Vec::new();
        for local_name in &fun.locals {
            let t_id = s.names.get(*local_name).t_id;
            let t = match s.types.get(t_id) {
                ASTTypes::I32 => Type::I32,
                ASTTypes::I64 => Type::I64,
                ASTTypes::F32 => Type::F32,
                ASTTypes::F64 => Type::F64,
                ASTTypes::Bool => Type::I32,
                _ => return Err(format!("Invalid parameter type for t_id {}", t_id)),
            };
            locals.push(Local {
                id: *local_name,
                t: t,
            })
        }

        Ok(locals)
    }

    fn reduce_block(&mut self, block: NameBlock, s: &mut State) -> Result<Block, String> {
        let id = s.fresh_bb_id();
        let mut stmts = Vec::new();
        self.reduce_block_rec(block, &mut stmts, s)?;
        let reduced_block = Block::Block {
            id: id,
            stmts: stmts,
        };
        Ok(reduced_block)
    }

    fn reduce_block_rec(
        &mut self,
        block: NameBlock,
        stmts: &mut Vec<Statement>,
        s: &mut State,
    ) -> Result<(), String> {
        for statement in block.stmts.into_iter() {
            match statement {
                S::AssignStmt { var, expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    stmts.push(Statement::Set { l_id: var.n_id });
                }
                S::LetStmt { var, expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    stmts.push(Statement::Set { l_id: var.n_id });
                }
                S::ExprStmt { expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    // Drop the result to conserve stack height
                    stmts.push(Statement::Parametric {
                        param: Parametric::Drop,
                    });
                }
                S::ReturnStmt { expr, .. } => {
                    if let Some(e) = expr {
                        self.reduce_expr(&e, stmts, s)?;
                    }
                    stmts.push(Statement::Control {
                        cntrl: Control::Return,
                    })
                }
                S::WhileStmt { expr, block } => {
                    let block_id = s.fresh_bb_id();
                    let loop_id = s.fresh_bb_id();
                    let mut loop_stmts = Vec::new();

                    self.reduce_expr(&expr, &mut loop_stmts, s)?;
                    // If NOT expr, then jump to end of block
                    loop_stmts.push(Statement::Const { val: Value::I32(1) });
                    loop_stmts.push(Statement::Binop {
                        binop: Binop::I32Xor,
                    });
                    loop_stmts.push(Statement::Control {
                        cntrl: Control::BrIf(block_id),
                    });

                    self.reduce_block_rec(block, &mut loop_stmts, s)?;
                    loop_stmts.push(Statement::Control {
                        cntrl: Control::Br(loop_id),
                    });
                    let loop_block = Block::Loop {
                        id: loop_id,
                        stmts: loop_stmts,
                    };
                    let block_block = Block::Block {
                        id: block_id,
                        stmts: vec![Statement::Block {
                            block: Box::new(loop_block),
                        }],
                    };
                    stmts.push(Statement::Block {
                        block: Box::new(block_block),
                    });
                }
                S::IfStmt { expr, block } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    let if_id = s.fresh_bb_id();
                    let mut then_stmts = Vec::new();
                    self.reduce_block_rec(block, &mut then_stmts, s)?;
                    let if_block = Block::If {
                        id: if_id,
                        then_stmts: then_stmts,
                        else_stmts: vec![],
                    };
                    stmts.push(Statement::Block {
                        block: Box::new(if_block),
                    });
                }
            }
        }

        Ok(())
    }

    // Push new statements that execute the given expression
    fn reduce_expr(
        &mut self,
        expression: &Expr,
        stmts: &mut Vec<Statement>,
        s: &mut State,
    ) -> Result<(), String> {
        match expression {
            Expr::Literal { value } => match value {
                V::Integer { val, t_id, .. } => {
                    let t = get_type(*t_id, s)?;
                    let val = match t {
                        Type::I32 => Value::I32((*val).try_into().unwrap()),
                        Type::I64 => Value::I64((*val).try_into().unwrap()),
                        _ => {
                            return Err(String::from("Integer constant of non integer type"));
                        }
                    };
                    stmts.push(Statement::Const { val: val })
                }
                V::Boolean { val, .. } => stmts.push(Statement::Const {
                    val: Value::I32(if *val { 1 } else { 0 }),
                }),
            },
            Expr::Variable { var } => stmts.push(Statement::Get { l_id: var.n_id }),
            Expr::Binary {
                expr_left,
                binop,
                expr_right,
                t_id,
            } => {
                let t = get_type(*t_id, s)?;
                let from_binop = get_binop(*binop, t)?;
                self.reduce_expr(expr_left, stmts, s)?;
                self.reduce_expr(expr_right, stmts, s)?;
                match from_binop {
                    FromBinop::Binop(binop) => stmts.push(Statement::Binop { binop: binop }),
                    FromBinop::Relop(relop) => stmts.push(Statement::Relop { relop: relop }),
                }
            }
            Expr::Unary { unop, expr, t_id } => {
                let t = get_type(*t_id, s)?;
                let mut unop_stmts = get_unop(*unop, t);
                self.reduce_expr(expr, stmts, s)?;
                stmts.append(&mut unop_stmts);
            }
            _ => self
                .error_handler
                .report_internal("Expression not yet handled in MIR: {"),
        }
        Ok(())
    }
}

fn get_unop(unop: ASTUnop, t: Type) -> Vec<Statement> {
    match unop {
        ASTUnop::Minus => {
            let neg = match t {
                Type::I32 => Unop::I32Neg,
                Type::I64 => Unop::I64Neg,
                Type::F32 => Unop::F32Neg,
                Type::F64 => Unop::F64Neg,
            };
            vec![Statement::Unop { unop: neg }]
        }
        ASTUnop::Not => vec![
            Statement::Const { val: Value::I32(1) },
            Statement::Binop {
                binop: Binop::I32Xor,
            },
        ],
    }
}

fn get_binop(binop: ASTBinop, t: Type) -> Result<FromBinop, String> {
    match t {
        Type::I32 => match binop {
            ASTBinop::Plus => Ok(FromBinop::Binop(Binop::I32Add)),
            ASTBinop::Minus => Ok(FromBinop::Binop(Binop::I32Sub)),
            ASTBinop::Multiply => Ok(FromBinop::Binop(Binop::I32Mul)),
            ASTBinop::Divide => Ok(FromBinop::Binop(Binop::I32Div)),
            ASTBinop::Remainder => Ok(FromBinop::Binop(Binop::I32Rem)),

            ASTBinop::Equal => Ok(FromBinop::Relop(Relop::I32Eq)),
            ASTBinop::NotEqual => Ok(FromBinop::Relop(Relop::I32Ne)),
            ASTBinop::Less => Ok(FromBinop::Relop(Relop::I32Lt)),
            ASTBinop::Greater => Ok(FromBinop::Relop(Relop::I32Gt)),
            ASTBinop::LessEqual => Ok(FromBinop::Relop(Relop::I32Le)),
            ASTBinop::GreaterEqual => Ok(FromBinop::Relop(Relop::I32Ge)),

            _ => Err(String::from("Bad binary operator for i32")),
        },
        Type::I64 => match binop {
            ASTBinop::Plus => Ok(FromBinop::Binop(Binop::I64Add)),
            ASTBinop::Minus => Ok(FromBinop::Binop(Binop::I64Sub)),
            ASTBinop::Multiply => Ok(FromBinop::Binop(Binop::I64Mul)),
            ASTBinop::Divide => Ok(FromBinop::Binop(Binop::I64Div)),
            ASTBinop::Remainder => Ok(FromBinop::Binop(Binop::I64Rem)),

            ASTBinop::Equal => Ok(FromBinop::Relop(Relop::I64Eq)),
            ASTBinop::NotEqual => Ok(FromBinop::Relop(Relop::I64Ne)),
            ASTBinop::Less => Ok(FromBinop::Relop(Relop::I64Lt)),
            ASTBinop::Greater => Ok(FromBinop::Relop(Relop::I64Gt)),
            ASTBinop::LessEqual => Ok(FromBinop::Relop(Relop::I64Le)),
            ASTBinop::GreaterEqual => Ok(FromBinop::Relop(Relop::I64Ge)),

            _ => Err(String::from("Bad binary operator for i64")),
        },
        Type::F32 => match binop {
            ASTBinop::Plus => Ok(FromBinop::Binop(Binop::F32Add)),
            ASTBinop::Minus => Ok(FromBinop::Binop(Binop::F32Sub)),
            ASTBinop::Multiply => Ok(FromBinop::Binop(Binop::F32Mul)),
            ASTBinop::Divide => Ok(FromBinop::Binop(Binop::F32Div)),

            ASTBinop::Equal => Ok(FromBinop::Relop(Relop::F32Eq)),
            ASTBinop::NotEqual => Ok(FromBinop::Relop(Relop::F32Ne)),
            ASTBinop::Less => Ok(FromBinop::Relop(Relop::F32Lt)),
            ASTBinop::Greater => Ok(FromBinop::Relop(Relop::F32Gt)),
            ASTBinop::LessEqual => Ok(FromBinop::Relop(Relop::F32Le)),
            ASTBinop::GreaterEqual => Ok(FromBinop::Relop(Relop::F32Ge)),

            _ => Err(String::from("Bad binary operator for f32")),
        },
        Type::F64 => match binop {
            ASTBinop::Plus => Ok(FromBinop::Binop(Binop::F64Add)),
            ASTBinop::Minus => Ok(FromBinop::Binop(Binop::F64Sub)),
            ASTBinop::Multiply => Ok(FromBinop::Binop(Binop::F64Mul)),
            ASTBinop::Divide => Ok(FromBinop::Binop(Binop::F64Div)),

            ASTBinop::Equal => Ok(FromBinop::Relop(Relop::F64Eq)),
            ASTBinop::NotEqual => Ok(FromBinop::Relop(Relop::F64Ne)),
            ASTBinop::Less => Ok(FromBinop::Relop(Relop::F64Lt)),
            ASTBinop::Greater => Ok(FromBinop::Relop(Relop::F64Gt)),
            ASTBinop::LessEqual => Ok(FromBinop::Relop(Relop::F64Le)),
            ASTBinop::GreaterEqual => Ok(FromBinop::Relop(Relop::F64Ge)),

            _ => Err(String::from("Bad binary operator for f64")),
        },
    }
}

fn get_type(t_id: TypeId, s: &State) -> Result<Type, String> {
    let t = s.types.get(t_id);
    match t {
        ASTTypes::Any | ASTTypes::Bug | ASTTypes::Unit => Err(format!(
            "Invalid type in MIR generation: {} for t_id: {}",
            t, t_id
        )),
        ASTTypes::I32 => Ok(Type::I32),
        ASTTypes::I64 => Ok(Type::I64),
        ASTTypes::F32 => Ok(Type::F32),
        ASTTypes::F64 => Ok(Type::F64),
        ASTTypes::Bool => Ok(Type::I32),
        ASTTypes::Fun(_, _) => Err(String::from("Function as a value are not yet implemented")),
    }
}

fn convert_type(t: &ASTTypes) -> Result<Type, String> {
    match t {
        ASTTypes::Any | ASTTypes::Bug | ASTTypes::Unit => {
            Err(format!("Invalid type in MIR generation: {}", t))
        }
        ASTTypes::I32 => Ok(Type::I32),
        ASTTypes::I64 => Ok(Type::I64),
        ASTTypes::F32 => Ok(Type::F32),
        ASTTypes::F64 => Ok(Type::F64),
        ASTTypes::Bool => Ok(Type::I32),
        ASTTypes::Fun(_, _) => Err(String::from("Function as a value are not yet implemented")),
    }
}
