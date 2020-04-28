use super::mir::*;
use super::names::{
    Block, Expression as Expr, Function as NameFun, NameStore, Statement as S, Value as V,
};
use super::types::{Type as ASTTypes, TypeStore};
use super::TypedProgram;

use crate::error::ErrorHandler;
use crate::parse;
use std::convert::TryInto;

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
            funs.push(self.reduce_fun(fun, &mut state));
        }

        Program { funs: funs }
    }

    fn reduce_fun(&mut self, fun: NameFun, s: &mut State) -> Function {
        let fun_name = s.names.get(fun.n_id);
        let (param_t, ret_t) = if let ASTTypes::Fun(param_t, ret_t) = s.types.get(fun_name.t_id) {
            (
                param_t.into_iter().map(|t| self.convert_types(t)).collect(),
                ret_t.into_iter().map(|t| self.convert_types(t)).collect(),
            )
        } else {
            self.error_handler
                .report_internal_loc(fun.loc, "Function does not have function type");
            (vec![], vec![])
        };

        let locals = fun.locals.iter().map(|l| Local { id: *l }).collect();
        let blocks = self.reduce_block(fun.block, s);

        Function {
            ident: fun.ident,
            param_types: param_t,
            ret_types: ret_t,
            locals: locals,
            blocks: blocks,
            exported: fun.exported,
        }
    }

    fn reduce_block(&mut self, block: Block, s: &mut State) -> Vec<BasicBlock> {
        self.reduce_block_rec(block, Vec::new(), s)
    }

    fn reduce_block_rec(
        &mut self,
        block: Block,
        mut basic_blocks: Vec<BasicBlock>,
        s: &mut State,
    ) -> Vec<BasicBlock> {
        let mut current_bb = BasicBlock::new(s.fresh_bb_id());

        for statement in &block.stmts {
            match statement {
                S::AssignStmt { var, expr } => {
                    self.reduce_expr(expr, &mut current_bb.stmts, s);
                    current_bb.stmts.push(Statement::Set { l_id: var.n_id });
                }
                S::LetStmt { var, expr } => {
                    self.reduce_expr(expr, &mut current_bb.stmts, s);
                    current_bb.stmts.push(Statement::Set { l_id: var.n_id });
                }
                S::ReturnStmt { expr, .. } => {
                    if let Some(e) = expr {
                        self.reduce_expr(e, &mut current_bb.stmts, s);
                    }
                    current_bb.terminator = Some(Terminator::Return)
                }
                _ => self
                    .error_handler
                    .report_internal("Statement not yet handled in MIR"),
            }
        }

        basic_blocks.push(current_bb);
        basic_blocks
    }

    // Push new statements that execute the given expression
    fn reduce_expr(&mut self, expression: &Expr, stmts: &mut Vec<Statement>, s: &mut State) {
        match expression {
            Expr::Literal { value } => match value {
                V::Integer { val, t_id, .. } => {
                    let t = self.convert_types(s.types.get(*t_id));
                    let val = match t {
                        Type::I32 => Value::I32((*val).try_into().unwrap()),
                        Type::I64 => Value::I64((*val).try_into().unwrap()),
                        _ => {
                            self.error_handler
                                .report_internal("Integer constant of non integer type");
                            return;
                        }
                    };
                    stmts.push(Statement::Const { val: val })
                }
                V::Boolean { val, .. } => stmts.push(Statement::Const {
                    val: Value::I32(if *val { 1 } else { 0 }),
                }),
            },
            Expr::Variable { var } => stmts.push(Statement::Get { l_id: var.n_id }),
            Expr::Unary { unop, expr, t_id } => match unop {
                parse::UnaryOperator::Minus => {
                    let t = self.convert_types(s.types.get(*t_id));
                    self.reduce_expr(expr, stmts, s);
                    stmts.push(Statement::Unop {
                        unop: Unop::Minus(t),
                    });
                }
                parse::UnaryOperator::Not => {
                    self.reduce_expr(expr, stmts, s);
                    stmts.push(Statement::Unop { unop: Unop::Not });
                }
            },
            _ => self
                .error_handler
                .report_internal("Expression not yet handled in MIR: {"),
        }
    }

    fn convert_types(&mut self, t: &ASTTypes) -> Type {
        match t {
            ASTTypes::Any | ASTTypes::Bug | ASTTypes::Unit => {
                self.error_handler
                    .report_internal(&format!("Invalid type in MIR generation: {}", t));
                Type::Bug
            }
            ASTTypes::I32 => Type::I32,
            ASTTypes::I64 => Type::I64,
            ASTTypes::F32 => Type::F32,
            ASTTypes::F64 => Type::F64,
            ASTTypes::Bool => Type::I32,
            ASTTypes::Fun(param, ret) => Type::Fun(
                param.into_iter().map(|t| self.convert_types(t)).collect(),
                ret.into_iter().map(|t| self.convert_types(t)).collect(),
            ),
        }
    }
}
