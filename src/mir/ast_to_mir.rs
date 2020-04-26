use super::mir::{Block, Function, Local, Program};
use super::names::{Function as NameFun, NameStore};
use super::types::{Type, TypeStore};
use super::TypedProgram;
use crate::error::ErrorHandler;

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
        let mut funs = Vec::with_capacity(prog.funs.len());

        for fun in prog.funs.into_iter() {
            funs.push(self.reduce_fun(fun, &prog.names, &prog.types));
        }

        Program { funs: funs }
    }

    fn reduce_fun(&mut self, fun: NameFun, names: &NameStore, types: &TypeStore) -> Function {
        let fun_name = names.get(fun.n_id);
        let (param_t, ret_t) = if let Type::Fun(param_t, ret_t) = types.get(fun_name.t_id) {
            (param_t.clone(), ret_t.clone())
        } else {
            self.error_handler
                .report_internal_loc(fun.loc, "Function does not have function type");
            (vec![], vec![])
        };

        let locals = fun.locals.iter().map(|l| Local { id: *l }).collect();

        Function {
            ident: fun.ident,
            param_types: param_t,
            ret_types: ret_t,
            locals: locals,
            blocks: vec![Block { id: 0 }, Block { id: 1 }],
            exported: fun.exported,
        }
    }
}
