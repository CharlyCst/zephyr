use super::mir::*;

use crate::error::ErrorHandler;
use crate::hir::{AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement};
use crate::hir::{
    Binop as HirBinop, Block as HirBlock, Body as HirBody, Expression as Expr, Function as HirFun,
    FunctionPrototype as HirFunProto, Imports as HirImports, IntegerType as HirIntergerType,
    LocalVariable as HirLocalVariable, NumericType as HirNumericType, Program as HirProgram,
    ScalarType as HirScalarType, Statement as S, Type as HirType, Unop as HirUnop, Value as V,
};

enum FromBinop {
    Binop(Binop),
    Relop(Relop),
    Logical(Logical),
}

struct State {
    bb_id: BasicBlockId,
}

impl State {
    pub fn new() -> State {
        State { bb_id: 0 }
    }

    pub fn fresh_bb_id(&mut self) -> BasicBlockId {
        let id = self.bb_id;
        self.bb_id += 1;
        id
    }
}

pub struct MIRProducer<'a> {
    err: &'a mut ErrorHandler,
}

impl<'a> MIRProducer<'a> {
    pub fn new(error_handler: &mut ErrorHandler) -> MIRProducer {
        MIRProducer { err: error_handler }
    }

    /// Lower a typed program to MIR
    pub fn reduce(&mut self, prog: HirProgram) -> Program {
        let mut state = State::new();
        let mut funs = Vec::with_capacity(prog.funs.len());
        let mut imports = Vec::with_capacity(prog.imports.len());

        for fun in prog.funs {
            match self.reduce_fun(fun, &mut state) {
                Ok(fun) => funs.push(fun),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }
        for import in prog.imports {
            match self.reduce_import(import) {
                Ok(import) => imports.push(import),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        Program {
            name: prog.package.name,
            funs,
            imports,
            pub_decls: prog.pub_decls,
        }
    }

    fn reduce_fun(&mut self, fun: HirFun, s: &mut State) -> Result<Function, String> {
        let t = fun.t;
        let mut param_t = Vec::with_capacity(t.params.len());
        let mut ret_t = Vec::with_capacity(t.ret.len());
        let mut locals = Vec::with_capacity(fun.locals.len());
        let params = fun.params;
        let block = match fun.body {
            HirBody::Zephyr(block) => self.reduce_block(block, s)?,
            HirBody::Asm(stmts) => Block::Block {
                id: s.fresh_bb_id(),
                stmts: self.reduce_asm_statements(stmts, s)?,
                t: None,
            },
        };
        for t in t.params {
            param_t.push(try_into_mir_t(&t)?);
        }
        for t in t.ret {
            ret_t.push(try_into_mir_t(&t)?);
        }
        for l in fun.locals {
            locals.push(self.reduce_local_variable(l)?);
        }

        Ok(Function {
            ident: fun.ident,
            params,
            param_t,
            ret_t,
            locals,
            body: block,
            is_pub: fun.is_pub,
            exposed: fun.exposed,
            fun_id: fun.fun_id,
        })
    }

    fn reduce_block(&mut self, block: HirBlock, s: &mut State) -> Result<Block, String> {
        let id = s.fresh_bb_id();
        let mut stmts = Vec::new();
        self.reduce_block_rec(block, &mut stmts, s)?;
        let reduced_block = Block::Block { id, stmts, t: None };
        Ok(reduced_block)
    }

    fn reduce_block_rec(
        &mut self,
        block: HirBlock,
        stmts: &mut Vec<Statement>,
        s: &mut State,
    ) -> Result<(), String> {
        for statement in block.stmts.into_iter() {
            match statement {
                S::AssignStmt { var, expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    stmts.push(Statement::Local {
                        local: Local::Set(var.n_id),
                    });
                }
                S::LetStmt { var, expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    stmts.push(Statement::Local {
                        local: Local::Set(var.n_id),
                    });
                }
                S::ExprStmt { expr } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    // We may want to control the size of the stack here,
                    // by dropping unused values for instance.
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
                        t: None,
                    };
                    let block_block = Block::Block {
                        id: block_id,
                        stmts: vec![Statement::Block {
                            block: Box::new(loop_block),
                        }],
                        t: None,
                    };
                    stmts.push(Statement::Block {
                        block: Box::new(block_block),
                    });
                }
                S::IfStmt {
                    expr,
                    block,
                    else_block,
                } => {
                    self.reduce_expr(&expr, stmts, s)?;
                    let if_id = s.fresh_bb_id();
                    let mut then_stmts = Vec::new();
                    self.reduce_block_rec(block, &mut then_stmts, s)?;
                    let mut else_stmts = Vec::new();
                    if let Some(else_block) = else_block {
                        self.reduce_block_rec(else_block, &mut else_stmts, s)?;
                    }
                    let if_block = Block::If {
                        id: if_id,
                        then_stmts,
                        else_stmts,
                        t: None,
                    };
                    stmts.push(Statement::Block {
                        block: Box::new(if_block),
                    });
                }
            }
        }

        Ok(())
    }

    /// Push new statements that execute the given expression
    fn reduce_expr(
        &mut self,
        expression: &Expr,
        stmts: &mut Vec<Statement>,
        s: &mut State,
    ) -> Result<(), String> {
        match expression {
            Expr::Literal { value } => match value {
                V::I32(val, _) => stmts.push(Statement::Const {
                    val: Value::I32(*val),
                }),
                V::I64(val, _) => stmts.push(Statement::Const {
                    val: Value::I64(*val),
                }),
                V::F32(val, _) => stmts.push(Statement::Const {
                    val: Value::F32(*val),
                }),
                V::F64(val, _) => stmts.push(Statement::Const {
                    val: Value::F64(*val),
                }),
                V::Bool(val, _) => stmts.push(Statement::Const {
                    val: Value::I32(if *val { 1 } else { 0 }),
                }),
                V::Struct { .. } => unimplemented!(),
            },
            Expr::Variable { var } => stmts.push(Statement::Local {
                local: Local::Get(var.n_id),
            }),
            Expr::Binary {
                expr_left,
                binop,
                expr_right,
                ..
            } => {
                let from_binop = get_binop(binop);
                match from_binop {
                    FromBinop::Binop(binop) => {
                        self.reduce_expr(expr_left, stmts, s)?;
                        self.reduce_expr(expr_right, stmts, s)?;
                        stmts.push(Statement::Binop { binop })
                    }
                    FromBinop::Relop(relop) => {
                        self.reduce_expr(expr_left, stmts, s)?;
                        self.reduce_expr(expr_right, stmts, s)?;
                        stmts.push(Statement::Relop { relop })
                    }
                    FromBinop::Logical(logical) => match logical {
                        Logical::And => {
                            let if_id = s.fresh_bb_id();
                            let mut then_stmts = Vec::new();
                            self.reduce_expr(expr_right, &mut then_stmts, s)?;
                            let else_stmts = vec![Statement::Const { val: Value::I32(0) }];
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.reduce_expr(expr_left, stmts, s)?;
                            stmts.push(Statement::Block {
                                block: Box::new(if_block),
                            });
                        }
                        Logical::Or => {
                            let if_id = s.fresh_bb_id();
                            let then_stmts = vec![Statement::Const { val: Value::I32(1) }];
                            let mut else_stmts = Vec::new();
                            self.reduce_expr(expr_right, &mut else_stmts, s)?;
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.reduce_expr(expr_left, stmts, s)?;
                            stmts.push(Statement::Block {
                                block: Box::new(if_block),
                            });
                        }
                    },
                }
            }
            Expr::Unary { unop, expr, .. } => match unop {
                HirUnop::Neg(t) => match t {
                    HirNumericType::I32 => {
                        stmts.push(Statement::Const { val: Value::I32(0) });
                        self.reduce_expr(expr, stmts, s)?;
                        stmts.push(Statement::Binop {
                            binop: Binop::I32Sub,
                        });
                    }
                    HirNumericType::I64 => {
                        stmts.push(Statement::Const { val: Value::I64(0) });
                        self.reduce_expr(expr, stmts, s)?;
                        stmts.push(Statement::Binop {
                            binop: Binop::I64Sub,
                        });
                    }
                    HirNumericType::F32 => {
                        self.reduce_expr(expr, stmts, s)?;
                        stmts.push(Statement::Unop { unop: Unop::F32Neg })
                    }
                    HirNumericType::F64 => {
                        self.reduce_expr(expr, stmts, s)?;
                        stmts.push(Statement::Unop { unop: Unop::F32Neg })
                    }
                },
                HirUnop::Not => {
                    stmts.push(Statement::Const { val: Value::I32(1) });
                    self.reduce_expr(expr, stmts, s)?;
                    stmts.push(Statement::Binop {
                        binop: Binop::I32Sub,
                    });
                }
            },
            Expr::CallDirect { fun_id, args, .. } => {
                for arg in args {
                    self.reduce_expr(arg, stmts, s)?;
                }
                stmts.push(Statement::Call {
                    call: Call::Direct(*fun_id),
                })
            }
            Expr::CallIndirect { loc, .. } => self
                .err
                .report(*loc, String::from("Indirect call are not yet supported")),
            Expr::Access { .. } => unimplemented!(),
            Expr::Nop { .. } => (),
        }
        Ok(())
    }

    fn reduce_local_variable(&mut self, local: HirLocalVariable) -> Result<LocalVariable, String> {
        let t = try_into_mir_t(&local.t)?;
        Ok(LocalVariable { id: local.id, t })
    }

    fn reduce_asm_statements(
        &mut self,
        stmts: Vec<AsmStatement>,
        s: &mut State,
    ) -> Result<Vec<Statement>, String> {
        let mut reduced_stmts = Vec::with_capacity(stmts.len());
        for stmt in stmts {
            match self.reduce_asm_statement(stmt, s) {
                Ok(stmt) => reduced_stmts.push(stmt),
                Err(err) => self.err.report_no_loc(err), //TODO: track location
            }
        }
        Ok(reduced_stmts)
    }

    fn reduce_asm_statement(
        &mut self,
        stmt: AsmStatement,
        _s: &mut State,
    ) -> Result<Statement, String> {
        match stmt {
            AsmStatement::Const { val, .. } => Ok(Statement::Const { val }),
            AsmStatement::Local { local, .. } => match local {
                AsmLocal::Get { var, .. } => Ok(Statement::Local {
                    local: Local::Get(var.n_id),
                }),
                AsmLocal::Set { var } => Ok(Statement::Local {
                    local: Local::Set(var.n_id),
                }),
            },
            AsmStatement::Control { cntrl, .. } => match cntrl {
                AsmControl::Return => Ok(Statement::Control {
                    cntrl: Control::Return,
                }),
                AsmControl::Unreachable => Ok(Statement::Control {
                    cntrl: Control::Unreachable,
                }),
            },
            AsmStatement::Parametric { param, .. } => match param {
                AsmParametric::Drop => Ok(Statement::Parametric {
                    param: Parametric::Drop,
                }),
            },
            AsmStatement::Memory { mem, .. } => match mem {
                AsmMemory::Size => Ok(Statement::Memory { mem: Memory::Size }),
                AsmMemory::Grow => Ok(Statement::Memory { mem: Memory::Grow }),
                AsmMemory::I32Load { align, offset } => Ok(Statement::Memory {
                    mem: Memory::I32Load { align, offset },
                }),
                AsmMemory::I64Load { align, offset } => Ok(Statement::Memory {
                    mem: Memory::I64Load { align, offset },
                }),
                AsmMemory::F32Load { align, offset } => Ok(Statement::Memory {
                    mem: Memory::F32Load { align, offset },
                }),
                AsmMemory::F64Load { align, offset } => Ok(Statement::Memory {
                    mem: Memory::F64Load { align, offset },
                }),
                AsmMemory::I32Store { align, offset } => Ok(Statement::Memory {
                    mem: Memory::I32Store { align, offset },
                }),
                AsmMemory::I64Store { align, offset } => Ok(Statement::Memory {
                    mem: Memory::I64Store { align, offset },
                }),
                AsmMemory::F32Store { align, offset } => Ok(Statement::Memory {
                    mem: Memory::F32Store { align, offset },
                }),
                AsmMemory::F64Store { align, offset } => Ok(Statement::Memory {
                    mem: Memory::F64Store { align, offset },
                }),
            },
        }
    }

    fn reduce_import(&mut self, imports: HirImports) -> Result<Imports, String> {
        let mut prototypes = Vec::with_capacity(imports.prototypes.len());
        for proto in imports.prototypes {
            prototypes.push(self.reduce_prototype(proto)?);
        }
        Ok(Imports {
            from: imports.from,
            prototypes,
        })
    }

    fn reduce_prototype(&mut self, proto: HirFunProto) -> Result<FunctionPrototype, String> {
        let mut param_t = Vec::with_capacity(proto.t.params.len());
        let mut ret_t = Vec::with_capacity(proto.t.ret.len());

        for param in proto.t.params {
            match try_into_mir_t(&param) {
                Ok(t) => param_t.push(t),
                Err(s) => return Err(s),
            }
        }
        for param in proto.t.ret {
            match try_into_mir_t(&param) {
                Ok(t) => ret_t.push(t),
                Err(s) => return Err(s),
            }
        }

        Ok(FunctionPrototype {
            ident: proto.ident,
            param_t,
            ret_t,
            alias: proto.alias,
            is_pub: proto.is_pub,
            fun_id: proto.fun_id,
        })
    }
}

fn get_binop(binop: &HirBinop) -> FromBinop {
    match binop {
        HirBinop::LogicalAnd => FromBinop::Logical(Logical::And),
        HirBinop::LogicalOr => FromBinop::Logical(Logical::Or),
        HirBinop::BinaryAnd(t) => match t {
            HirIntergerType::I32 => FromBinop::Binop(Binop::I32And),
            HirIntergerType::I64 => FromBinop::Binop(Binop::I64And),
        },
        HirBinop::BinaryOr(t) => match t {
            HirIntergerType::I32 => FromBinop::Binop(Binop::I32Or),
            HirIntergerType::I64 => FromBinop::Binop(Binop::I64Or),
        },
        HirBinop::Xor(t) => match t {
            HirIntergerType::I32 => FromBinop::Binop(Binop::I32Xor),
            HirIntergerType::I64 => FromBinop::Binop(Binop::I64Xor),
        },
        HirBinop::Eq(t) => match t {
            HirScalarType::I32 => FromBinop::Relop(Relop::I32Eq),
            HirScalarType::I64 => FromBinop::Relop(Relop::I64Eq),
            HirScalarType::F32 => FromBinop::Relop(Relop::F32Eq),
            HirScalarType::F64 => FromBinop::Relop(Relop::F64Eq),
            HirScalarType::Bool => FromBinop::Relop(Relop::I32Eq),
        },
        HirBinop::Ne(t) => match t {
            HirScalarType::I32 => FromBinop::Relop(Relop::I32Ne),
            HirScalarType::I64 => FromBinop::Relop(Relop::I64Ne),
            HirScalarType::F32 => FromBinop::Relop(Relop::F32Ne),
            HirScalarType::F64 => FromBinop::Relop(Relop::F64Ne),
            HirScalarType::Bool => FromBinop::Relop(Relop::I32Ne),
        },
        HirBinop::Gt(t) => match t {
            HirNumericType::I32 => FromBinop::Relop(Relop::I32Gt),
            HirNumericType::I64 => FromBinop::Relop(Relop::I64Gt),
            HirNumericType::F32 => FromBinop::Relop(Relop::F32Gt),
            HirNumericType::F64 => FromBinop::Relop(Relop::F64Gt),
        },

        HirBinop::Ge(t) => match t {
            HirNumericType::I32 => FromBinop::Relop(Relop::I32Ge),
            HirNumericType::I64 => FromBinop::Relop(Relop::I64Ge),
            HirNumericType::F32 => FromBinop::Relop(Relop::F32Ge),
            HirNumericType::F64 => FromBinop::Relop(Relop::F64Ge),
        },
        HirBinop::Lt(t) => match t {
            HirNumericType::I32 => FromBinop::Relop(Relop::I32Lt),
            HirNumericType::I64 => FromBinop::Relop(Relop::I64Lt),
            HirNumericType::F32 => FromBinop::Relop(Relop::F32Lt),
            HirNumericType::F64 => FromBinop::Relop(Relop::F64Lt),
        },

        HirBinop::Le(t) => match t {
            HirNumericType::I32 => FromBinop::Relop(Relop::I32Le),
            HirNumericType::I64 => FromBinop::Relop(Relop::I64Le),
            HirNumericType::F32 => FromBinop::Relop(Relop::F32Le),
            HirNumericType::F64 => FromBinop::Relop(Relop::F64Le),
        },
        HirBinop::Add(t) => match t {
            HirNumericType::I32 => FromBinop::Binop(Binop::I32Add),
            HirNumericType::I64 => FromBinop::Binop(Binop::I64Add),
            HirNumericType::F32 => FromBinop::Binop(Binop::F32Add),
            HirNumericType::F64 => FromBinop::Binop(Binop::F64Add),
        },
        HirBinop::Sub(t) => match t {
            HirNumericType::I32 => FromBinop::Binop(Binop::I32Sub),
            HirNumericType::I64 => FromBinop::Binop(Binop::I64Sub),
            HirNumericType::F32 => FromBinop::Binop(Binop::F32Sub),
            HirNumericType::F64 => FromBinop::Binop(Binop::F64Sub),
        },
        HirBinop::Mul(t) => match t {
            HirNumericType::I32 => FromBinop::Binop(Binop::I32Mul),
            HirNumericType::I64 => FromBinop::Binop(Binop::I64Mul),
            HirNumericType::F32 => FromBinop::Binop(Binop::F32Mul),
            HirNumericType::F64 => FromBinop::Binop(Binop::F64Mul),
        },
        HirBinop::Div(t) => match t {
            HirNumericType::I32 => FromBinop::Binop(Binop::I32Div),
            HirNumericType::I64 => FromBinop::Binop(Binop::I64Div),
            HirNumericType::F32 => FromBinop::Binop(Binop::F32Div),
            HirNumericType::F64 => FromBinop::Binop(Binop::F64Div),
        },
        HirBinop::Rem(t) => match t {
            HirIntergerType::I32 => FromBinop::Binop(Binop::I32Rem),
            HirIntergerType::I64 => FromBinop::Binop(Binop::I64Rem),
        },
    }
}

/// Convert a scalar value into its MIR representation.
fn get_mir_t(t: &HirScalarType) -> Type {
    match t {
        HirScalarType::I32 => Type::I32,
        HirScalarType::I64 => Type::I64,
        HirScalarType::F32 => Type::F32,
        HirScalarType::F64 => Type::F64,
        HirScalarType::Bool => Type::I32,
    }
}

/// Try to convert an arbitrary HIR type to an MIR type.
/// For now advanced types such as functions and tuples are not supported.
fn try_into_mir_t(t: &HirType) -> Result<Type, String> {
    match t {
        HirType::Scalar(t) => Ok(get_mir_t(t)),
        HirType::Fun(_) => Err(String::from("Function as value are not yet supported.")),
        HirType::Tuple(_) => Err(String::from("Tuples are not yet supported.")),
        HirType::Struct(_) => unimplemented!(), // TODO!
    }
}
