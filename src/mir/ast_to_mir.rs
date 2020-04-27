use super::mir::{
    BasicBlock, BasicBlockId, Function, Local, Program, Statement, Terminator, Value,
};
use super::names::{Function as NameFun, NameStore};
use super::types::{Type, TypeStore};
use super::TypedProgram;

use crate::error::ErrorHandler;
use crate::parse;
use crate::parse::{Block, Expression as Expr, Statement as S};

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
        let (param_t, ret_t) = if let Type::Fun(param_t, ret_t) = s.types.get(fun_name.t_id) {
            (param_t.clone(), ret_t.clone())
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
            blocks: blocks, //BasicBlock { id: 0 }, BasicBlock { id: 1 }
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

    fn reduce_expr(&mut self, expression: &Expr, stmts: &mut Vec<Statement>, s: &mut State) {
        match expression {
            Expr::Literal { value } => match value {
                parse::Value::Integer { val, .. } => stmts.push(Statement::Const {
                    val: Value::I64((*val).try_into().unwrap()),
                }),
                parse::Value::Boolean { val, .. } => stmts.push(Statement::Const {
                    val: Value::I32(if *val { 1 } else { 0 }),
                }),
            },
            Expr::Variable { var } => stmts.push(Statement::Get { l_id: var.n_id }),
            _ => self.error_handler.report_internal(&format!(
                "Expression not yet handled in MIR: {}",
                expression
            )),
        }
    }
}
