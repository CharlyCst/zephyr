use std::collections::HashMap;

use super::mir::*;

use crate::ctx::{Ctx, KnownFunctions};
use crate::error::ErrorHandler;
use crate::hir::{AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement};
use crate::hir::{
    Binop as HirBinop, Block as HirBlock, Body as HirBody, Data as HirData, Expression as Expr,
    FunKind, Function as HirFun, FunctionPrototype as HirFunProto, Import as HirImport,
    IntegerType as HirIntergerType, LocalId as HirLocalId, LocalVariable as HirLocalVariable,
    NumericType as HirNumericType, PlaceExpression as PlaceExpr, ScalarType as HirScalarType,
    Statement as S, Struct as HirStruct, TupleType as HirTupleType, Type as HirType,
    Unop as HirUnop, Value as V,
};

enum FromBinop {
    Binop(Binop),
    Relop(Relop),
    Logical(Logical),
}

struct State<'a> {
    bb_id: BasicBlockId,
    local_id: LocalId,
    /// A mapping from HIR local variable ID to MIR local variable ID
    locals: HashMap<HirLocalId, LocalId>,
    structs: &'a HashMap<StructId, Struct>,
    funs: &'a KnownFunctions,
}

impl<'a> State<'a> {
    pub fn new(structs: &'a HashMap<StructId, Struct>, funs: &'a KnownFunctions) -> Self {
        Self {
            bb_id: 0,
            local_id: 0,
            locals: HashMap::new(),
            structs,
            funs,
        }
    }

    /// Returns a globally unique basic block ID.
    pub fn fresh_bb_id(&mut self) -> BasicBlockId {
        let id = self.bb_id;
        self.bb_id += 1;
        id
    }

    /// Returns a globally unique local variable ID.
    pub fn fresh_local_id(&mut self) -> LocalId {
        let id = self.local_id;
        self.local_id += 1;
        id
    }

    /// Returns the MIR local ID corresponding to an HIR ID, creates a fresh binding if necessary.
    pub fn get_local_id(&mut self, id: HirLocalId) -> LocalId {
        match self.locals.get(&id) {
            Some(new_id) => *new_id,
            None => self.remap_local_id(id),
        }
    }

    /// Creates a fresh local variable ID and adds a mapping from HIR ID -> fresh MIR ID in the
    /// `locals` map.
    fn remap_local_id(&mut self, id: HirLocalId) -> LocalId {
        let new_id = self.fresh_local_id();
        self.locals.insert(id, new_id);
        new_id
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
    pub fn reduce(&mut self, ctx: &Ctx, known_funs: &KnownFunctions) -> Program {
        let hir_funs = ctx.hir_funs();
        let hir_structs = ctx.hir_structs();
        let hir_imports = ctx.hir_imports();
        let hir_data = ctx.hir_data();
        let mut funs = Vec::with_capacity(hir_funs.len());
        let mut imports = Vec::with_capacity(hir_imports.len());
        let structs = self.reduce_structs(hir_structs);
        let data = self.reduce_data(hir_data);
        let mut state = State::new(&structs, known_funs);

        for (_, fun) in hir_funs {
            match fun {
                FunKind::Fun(fun) => match self.reduce_fun(fun, &mut state) {
                    Ok(fun) => funs.push(fun),
                    Err(err) => self.err.report_internal_no_loc(err),
                },
                FunKind::Extern(_) => (),
            }
        }
        for import in hir_imports {
            match self.reduce_import(import, ctx) {
                Ok(import) => imports.push(import),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        Program {
            funs,
            imports,
            structs,
            data,
        }
    }

    /// Decides of the memory layout of the structs.
    ///
    /// The memory blocks returned by malloc are guaranteed to have an alignment of 8, this
    /// function should arrange all the fields so that all of them have an alignment suitable for
    /// their types while minimizing unused space.
    fn reduce_structs(
        &mut self,
        structs: &HashMap<StructId, HirStruct>,
    ) -> HashMap<StructId, Struct> {
        let mut mir_struct = HashMap::with_capacity(structs.len());
        let mut align_1 = Vec::new();
        let mut align_4 = Vec::new();
        let mut align_8 = Vec::new();
        for (s_id, s) in structs {
            let mut fields = HashMap::with_capacity(s.fields.len());
            // Collect alignments and sizes
            for (field_name, field) in &s.fields {
                // Compute memory layout of the field
                let (t, layout) = match try_into_mir_layout(&field.t) {
                    Ok(t) => t,
                    Err(e) => {
                        self.err.report_internal_no_loc(e);
                        continue;
                    }
                };
                let (alignment, size) = get_aligment(&field.t);
                match alignment {
                    Aligment::A1 => align_1.push((field_name, size, t, layout)),
                    Aligment::A4 => align_4.push((field_name, size, t, layout)),
                    Aligment::A8 => align_8.push((field_name, size, t, layout)),
                }
            }
            // Decide of the layout, this can be optimized in the future
            let mut offset = 0;
            for (field_name, size, t, layout) in align_8.drain(..) {
                offset = align_offset(offset, 8);
                fields.insert(field_name.to_owned(), StructField { offset, layout, t });
                offset += size;
            }
            for (field_name, size, t, layout) in align_4.drain(..) {
                offset = align_offset(offset, 4);
                fields.insert(field_name.to_owned(), StructField { offset, layout, t });
                offset += size;
            }
            for (field_name, size, t, layout) in align_1.drain(..) {
                fields.insert(field_name.to_owned(), StructField { offset, layout, t });
                offset += size;
            }
            // Layout is now fixed
            mir_struct.insert(
                *s_id,
                Struct {
                    fields,
                    size: offset,
                },
            );
        }
        mir_struct
    }

    fn reduce_data(&mut self, hir_data: &HashMap<DataId, HirData>) -> HashMap<DataId, Data> {
        let mut mir_data = HashMap::with_capacity(hir_data.len());
        for (data_id, data) in hir_data {
            let data = match data {
                HirData::Str(data_id_2, data) => {
                    assert!(data_id == data_id_2);
                    data.to_owned()
                }
            };
            mir_data.insert(*data_id, data);
        }
        mir_data
    }

    fn reduce_fun(&mut self, fun: &HirFun, s: &mut State) -> Result<Function, String> {
        let t = &fun.t;
        let mut param_t = Vec::with_capacity(t.params.len());
        let mut ret_t = Vec::with_capacity(t.ret.len());
        let mut locals = Vec::with_capacity(fun.locals.len());
        let mut params = Vec::with_capacity(fun.params.len());

        // Convert params and return types
        for t in &t.params {
            param_t.push(try_into_mir_t(&t)?);
        }
        for t in &t.ret {
            ret_t.push(try_into_mir_t(&t)?);
        }
        // Register params and local variables
        for param_local_id in &fun.params {
            params.push(s.remap_local_id(*param_local_id));
        }
        for l in &fun.locals {
            locals.push(self.reduce_local_variable(l, s)?);
        }
        // Reduce function body
        let (block, block_locals) = match &fun.body {
            HirBody::Zephyr(block) => self.reduce_block(block, s)?,
            HirBody::Asm(stmts) => (
                Block::Block {
                    id: s.fresh_bb_id(),
                    stmts: self.reduce_asm_statements(stmts, s)?,
                    t: None,
                },
                vec![],
            ),
        };
        locals.extend(block_locals);

        Ok(Function {
            ident: fun.ident.clone(),
            params,
            param_t,
            ret_t,
            locals,
            body: block,
            is_pub: fun.is_pub,
            exposed: fun.exposed.clone(),
            fun_id: fun.fun_id,
        })
    }

    /// Reduces a block of statements, local variables may be created for the need of computations
    /// and are returned along the reduced block.
    fn reduce_block(
        &mut self,
        block: &HirBlock,
        s: &mut State,
    ) -> Result<(Block, Vec<LocalVariable>), String> {
        let id = s.fresh_bb_id();
        let mut stmts = Vec::new();
        let mut locals = Vec::new();
        self.reduce_block_rec(block, &mut stmts, &mut locals, s)?;
        let reduced_block = Block::Block { id, stmts, t: None };
        Ok((reduced_block, locals))
    }

    fn reduce_block_rec(
        &mut self,
        block: &HirBlock,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
        s: &mut State,
    ) -> Result<(), String> {
        for statement in &block.stmts {
            match statement {
                S::AssignStmt { target, expr } => {
                    self.reduce_assign_stmt(target, expr, stmts, locals, s)?;
                }
                S::LetStmt { var, expr } => {
                    self.reduce_expr(&expr, stmts, locals, s)?;
                    stmts.push(Statement::Local(Local::Set(s.get_local_id(var.n_id))));
                }
                S::ExprStmt(expr) => {
                    let values = self.reduce_expr(&expr, stmts, locals, s)?;
                    // drop unused values
                    for _ in values {
                        stmts.push(Statement::Parametric(Parametric::Drop));
                    }
                }
                S::ReturnStmt { expr, .. } => {
                    if let Some(e) = expr {
                        self.reduce_expr(&e, stmts, locals, s)?;
                    }
                    stmts.push(Statement::Control(Control::Return))
                }
                S::WhileStmt { expr, block } => {
                    let block_id = s.fresh_bb_id();
                    let loop_id = s.fresh_bb_id();
                    let mut loop_stmts = Vec::new();

                    self.reduce_expr(&expr, &mut loop_stmts, locals, s)?;
                    // If NOT expr, then jump to end of block
                    loop_stmts.push(Statement::Const(Value::I32(1)));
                    loop_stmts.push(Statement::Binop(Binop::I32Xor));
                    loop_stmts.push(Statement::Control(Control::BrIf(block_id)));

                    self.reduce_block_rec(&block, &mut loop_stmts, locals, s)?;
                    loop_stmts.push(Statement::Control(Control::Br(loop_id)));
                    let loop_block = Block::Loop {
                        id: loop_id,
                        stmts: loop_stmts,
                        t: None,
                    };
                    let block_block = Block::Block {
                        id: block_id,
                        stmts: vec![Statement::Block(Box::new(loop_block))],
                        t: None,
                    };
                    stmts.push(Statement::Block(Box::new(block_block)));
                }
                S::IfStmt {
                    expr,
                    block,
                    else_block,
                } => {
                    self.reduce_expr(&expr, stmts, locals, s)?;
                    let if_id = s.fresh_bb_id();
                    let mut then_stmts = Vec::new();
                    self.reduce_block_rec(&block, &mut then_stmts, locals, s)?;
                    let mut else_stmts = Vec::new();
                    if let Some(else_block) = else_block {
                        self.reduce_block_rec(&else_block, &mut else_stmts, locals, s)?;
                    }
                    let if_block = Block::If {
                        id: if_id,
                        then_stmts,
                        else_stmts,
                        t: None,
                    };
                    stmts.push(Statement::Block(Box::new(if_block)));
                }
            }
        }

        Ok(())
    }

    /// Push new statements that execute the given expression and return the types of values added
    /// on top of the heap.
    fn reduce_expr(
        &mut self,
        expression: &Expr,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
        s: &mut State,
    ) -> Result<Vec<Type>, String> {
        let types = match expression {
            Expr::Literal(value) => match value {
                V::I32(val, _) => {
                    stmts.push(Statement::Const(Value::I32(*val)));
                    vec![Type::I32]
                }
                V::I64(val, _) => {
                    stmts.push(Statement::Const(Value::I64(*val)));
                    vec![Type::I64]
                }
                V::F32(val, _) => {
                    stmts.push(Statement::Const(Value::F32(*val)));
                    vec![Type::F32]
                }
                V::F64(val, _) => {
                    stmts.push(Statement::Const(Value::F64(*val)));
                    vec![Type::F64]
                }
                V::Bool(val, _) => {
                    stmts.push(Statement::Const(Value::I32(if *val { 1 } else { 0 })));
                    vec![Type::I32]
                }
                V::DataPointer(data_id, _) => {
                    stmts.push(Statement::Const(Value::DataPointer(*data_id)));
                    vec![Type::I32]
                }
                V::Struct {
                    struct_id, fields, ..
                } => {
                    let struc = s.structs.get(struct_id).unwrap();
                    // Allocate memory
                    stmts.push(Statement::Const(Value::I32(struc.size as i32)));
                    stmts.push(Statement::Call(Call::Direct(s.funs.malloc)));
                    // Save the pointer in a local variable
                    let pointer_l_id = s.fresh_local_id();
                    locals.push(LocalVariable {
                        t: Type::I32,
                        id: pointer_l_id,
                    });
                    stmts.push(Statement::Local(Local::Set(pointer_l_id)));
                    // Now fill the fields with their values
                    for field in fields {
                        let (layout, offset) = if let Some(f) = struc.fields.get(&field.ident) {
                            (f.layout, f.offset)
                        } else {
                            self.err.report_internal_no_loc(format!(
                                "Field does not exist in MIR struct: '{}'",
                                &field.ident
                            ));
                            continue;
                        };
                        // Put memory location and value on top of stack
                        stmts.push(Statement::Local(Local::Get(pointer_l_id)));
                        let values_types = self.reduce_expr(&*field.expr, stmts, locals, s)?;
                        if values_types.len() != 1 {
                            self.err.report_internal_no_loc(format!("Fields can only be initialized with expressions producing exactly one value for now, got {}", values_types.len()));
                            continue;
                        }
                        let t = values_types.first().unwrap();
                        // Store the value
                        stmts.push(Statement::Memory(get_store_instr(*t, layout, offset)?));
                    }
                    // Put the struct pointer on top of the stack
                    stmts.push(Statement::Local(Local::Get(pointer_l_id)));
                    vec![Type::I32]
                }
            },
            Expr::Variable(var) => {
                stmts.push(Statement::Local(Local::Get(s.get_local_id(var.n_id))));
                vec![try_into_mir_t(&var.t)?]
            }
            Expr::Binary {
                expr_left,
                binop,
                expr_right,
                ..
            } => {
                let from_binop = get_binop(binop);
                match from_binop {
                    FromBinop::Binop(binop) => {
                        let t = binop.get_t();
                        self.reduce_expr(expr_left, stmts, locals, s)?;
                        self.reduce_expr(expr_right, stmts, locals, s)?;
                        stmts.push(Statement::Binop(binop));
                        vec![t]
                    }
                    FromBinop::Relop(relop) => {
                        let t = relop.get_t();
                        self.reduce_expr(expr_left, stmts, locals, s)?;
                        self.reduce_expr(expr_right, stmts, locals, s)?;
                        stmts.push(Statement::Relop(relop));
                        vec![t]
                    }
                    FromBinop::Logical(logical) => match logical {
                        Logical::And => {
                            let if_id = s.fresh_bb_id();
                            let mut then_stmts = Vec::new();
                            self.reduce_expr(expr_right, &mut then_stmts, locals, s)?;
                            let else_stmts = vec![Statement::Const(Value::I32(0))];
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.reduce_expr(expr_left, stmts, locals, s)?;
                            stmts.push(Statement::Block(Box::new(if_block)));
                            vec![Type::I32]
                        }
                        Logical::Or => {
                            let if_id = s.fresh_bb_id();
                            let then_stmts = vec![Statement::Const(Value::I32(1))];
                            let mut else_stmts = Vec::new();
                            self.reduce_expr(expr_right, &mut else_stmts, locals, s)?;
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.reduce_expr(expr_left, stmts, locals, s)?;
                            stmts.push(Statement::Block(Box::new(if_block)));
                            vec![Type::I32]
                        }
                    },
                }
            }
            Expr::Unary { unop, expr, .. } => match unop {
                HirUnop::Neg(t) => match t {
                    HirNumericType::I32 => {
                        stmts.push(Statement::Const(Value::I32(0)));
                        self.reduce_expr(expr, stmts, locals, s)?;
                        stmts.push(Statement::Binop(Binop::I32Sub));
                        vec![Type::I32]
                    }
                    HirNumericType::I64 => {
                        stmts.push(Statement::Const(Value::I64(0)));
                        self.reduce_expr(expr, stmts, locals, s)?;
                        stmts.push(Statement::Binop(Binop::I64Sub));
                        vec![Type::I64]
                    }
                    HirNumericType::F32 => {
                        self.reduce_expr(expr, stmts, locals, s)?;
                        stmts.push(Statement::Unop(Unop::F32Neg));
                        vec![Type::F32]
                    }
                    HirNumericType::F64 => {
                        self.reduce_expr(expr, stmts, locals, s)?;
                        stmts.push(Statement::Unop(Unop::F32Neg));
                        vec![Type::F64]
                    }
                },
                HirUnop::Not => {
                    stmts.push(Statement::Const(Value::I32(1)));
                    self.reduce_expr(expr, stmts, locals, s)?;
                    stmts.push(Statement::Binop(Binop::I32Sub));
                    vec![Type::I32]
                }
            },
            Expr::CallDirect {
                fun_id, args, t, ..
            } => {
                for arg in args {
                    self.reduce_expr(arg, stmts, locals, s)?;
                }
                stmts.push(Statement::Call(Call::Direct(*fun_id)));
                try_into_mir_tuple_t(&t.ret)?
            }
            Expr::CallIndirect { loc, .. } => {
                self.err
                    .report(*loc, String::from("Indirect call are not yet supported"));
                todo!()
            }
            Expr::Access {
                expr,
                field,
                struct_id,
                ..
            } => {
                let struc = s.structs.get(struct_id).unwrap();
                let field = struc.fields.get(field).unwrap();
                self.reduce_expr(expr, stmts, locals, s)?;
                stmts.push(Statement::Memory(get_load_instr(
                    field.t,
                    field.layout,
                    field.offset,
                )?));
                vec![field.t]
            }
            Expr::Nop { .. } => vec![],
        };
        Ok(types)
    }

    /// Reduces an assign statement (`target = expr`).
    fn reduce_assign_stmt(
        &mut self,
        target: &PlaceExpr,
        expr: &Expr,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
        s: &mut State,
    ) -> Result<(), String> {
        match target {
            PlaceExpr::Variable(var) => {
                self.reduce_expr(&expr, stmts, locals, s)?;
                stmts.push(Statement::Local(Local::Set(s.get_local_id(var.n_id))))
            }
            PlaceExpr::Access {
                expr: target_expr,
                field,
                struct_id,
                ..
            } => {
                let struc = s.structs.get(&struct_id).unwrap();
                let field = struc.fields.get(field).unwrap();
                let store_instr = get_store_instr(field.t, field.layout, field.offset)?;
                // Push the address on the stack
                self.reduce_expr(&target_expr, stmts, locals, s)?;
                // Push the value on the stack
                self.reduce_expr(&expr, stmts, locals, s)?;
                stmts.push(Statement::Memory(store_instr));
            }
        }
        Ok(())
    }

    fn reduce_local_variable(
        &mut self,
        local: &HirLocalVariable,
        s: &mut State,
    ) -> Result<LocalVariable, String> {
        let t = try_into_mir_t(&local.t)?;
        let id = s.get_local_id(local.id);
        Ok(LocalVariable { id, t })
    }

    fn reduce_asm_statements(
        &mut self,
        stmts: &Vec<AsmStatement>,
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
        stmt: &AsmStatement,
        s: &mut State,
    ) -> Result<Statement, String> {
        match stmt {
            AsmStatement::Const { ref val, .. } => Ok(Statement::Const(val.clone())),
            AsmStatement::Local { local, .. } => match local {
                AsmLocal::Get { var, .. } => {
                    Ok(Statement::Local(Local::Get(s.get_local_id(var.n_id))))
                }
                AsmLocal::Set { var } => Ok(Statement::Local(Local::Set(s.get_local_id(var.n_id)))),
            },
            AsmStatement::Control { cntrl, .. } => match cntrl {
                AsmControl::Return => Ok(Statement::Control(Control::Return)),
                AsmControl::Unreachable => Ok(Statement::Control(Control::Unreachable)),
            },
            AsmStatement::Parametric { param, .. } => match param {
                AsmParametric::Drop => Ok(Statement::Parametric(Parametric::Drop)),
            },
            AsmStatement::Memory { mem, .. } => match mem {
                AsmMemory::Size => Ok(Statement::Memory(Memory::Size)),
                AsmMemory::Grow => Ok(Statement::Memory(Memory::Grow)),
                // Loads
                AsmMemory::I32Load { align, offset } => Ok(Statement::Memory(Memory::I32Load {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::I64Load { align, offset } => Ok(Statement::Memory(Memory::I64Load {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::F32Load { align, offset } => Ok(Statement::Memory(Memory::F32Load {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::F64Load { align, offset } => Ok(Statement::Memory(Memory::F64Load {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::I32Load8u { align, offset } => {
                    Ok(Statement::Memory(Memory::I32Load8u {
                        align: *align,
                        offset: *offset,
                    }))
                }
                // Stores
                AsmMemory::I32Store { align, offset } => Ok(Statement::Memory(Memory::I32Store {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::I64Store { align, offset } => Ok(Statement::Memory(Memory::I64Store {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::F32Store { align, offset } => Ok(Statement::Memory(Memory::F32Store {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::F64Store { align, offset } => Ok(Statement::Memory(Memory::F64Store {
                    align: *align,
                    offset: *offset,
                })),
                AsmMemory::I32Store8 { align, offset } => {
                    Ok(Statement::Memory(Memory::I32Store8 {
                        align: *align,
                        offset: *offset,
                    }))
                }
            },
        }
    }

    fn reduce_import(&mut self, imports: &HirImport, ctx: &Ctx) -> Result<Imports, String> {
        let mut prototypes = Vec::with_capacity(imports.prototypes.len());
        let hir_funs = ctx.hir_funs();
        for proto_fun_id in &imports.prototypes {
            match hir_funs.get(&proto_fun_id) {
                Some(FunKind::Extern(proto)) => prototypes.push(self.reduce_prototype(proto)?),
                Some(FunKind::Fun(_)) => {
                    return Err(String::from(
                        "Imported fun_id must correspond to an external function",
                    ))
                }
                None => return Err(String::from("Invalid fun_id for imported function")),
            }
        }
        Ok(Imports {
            from: imports.from.clone(),
            prototypes,
        })
    }

    fn reduce_prototype(&mut self, proto: &HirFunProto) -> Result<FunctionPrototype, String> {
        let mut param_t = Vec::with_capacity(proto.t.params.len());
        let mut ret_t = Vec::with_capacity(proto.t.ret.len());

        for param in &proto.t.params {
            match try_into_mir_t(&param) {
                Ok(t) => param_t.push(t),
                Err(s) => return Err(s),
            }
        }
        for param in &proto.t.ret {
            match try_into_mir_t(&param) {
                Ok(t) => ret_t.push(t),
                Err(s) => return Err(s),
            }
        }

        Ok(FunctionPrototype {
            ident: proto.ident.clone(),
            param_t,
            ret_t,
            alias: proto.alias.clone(),
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

/// Convert a scalar value into its memory representation.
fn get_mir_t_layout(t: &HirScalarType) -> MemoryLayout {
    match t {
        HirScalarType::I32 => MemoryLayout::I32,
        HirScalarType::I64 => MemoryLayout::I64,
        HirScalarType::F32 => MemoryLayout::F32,
        HirScalarType::F64 => MemoryLayout::F64,
        HirScalarType::Bool => MemoryLayout::U8,
    }
}

/// Try to convert a hir typle type into its MIR representation.
fn try_into_mir_tuple_t(t: &HirTupleType) -> Result<Vec<Type>, String> {
    let mut mir_t = Vec::with_capacity(t.len());
    for hir_t in t {
        mir_t.push(try_into_mir_t(hir_t)?);
    }
    Ok(mir_t)
}

/// Try to convert an arbitrary HIR type to an MIR type.
/// For now advanced types such as functions and tuples are not supported.
fn try_into_mir_t(t: &HirType) -> Result<Type, String> {
    match t {
        HirType::Scalar(t) => Ok(get_mir_t(t)),
        HirType::Fun(_) => Err(String::from("Function as value are not yet supported.")),
        HirType::Tuple(_) => Err(String::from("Tuples are not yet supported.")),
        // For now structs are always boxed and represented by a pointer to their location
        HirType::Struct(_) => Ok(Type::I32),
    }
}

fn try_into_mir_layout(t: &HirType) -> Result<(Type, MemoryLayout), String> {
    match t {
        HirType::Scalar(t) => Ok((get_mir_t(t), get_mir_t_layout(t))),
        HirType::Fun(_) => Err(String::from("Functions as value are not yet supported.")),
        HirType::Tuple(_) => Err(String::from("Tuples are not yet supported.")),
        // For now structs are always boxed and represented by a pointer to their location
        HirType::Struct(_) => Ok((Type::I32, MemoryLayout::I32)),
    }
}

/// Returns the alignment and size a given type occupy in memory.
fn get_aligment(t: &HirType) -> (Aligment, u32) {
    match t {
        HirType::Scalar(x) => match x {
            HirScalarType::I32 => (Aligment::A4, 4),
            HirScalarType::I64 => (Aligment::A8, 8),
            HirScalarType::F32 => (Aligment::A4, 4),
            HirScalarType::F64 => (Aligment::A8, 8),
            HirScalarType::Bool => (Aligment::A1, 1),
        },
        HirType::Struct(_) => (Aligment::A4, 4), // Represented as a i32 pointer for now
        _ => todo!("Only scalar and struct are supported inside structures at the time"), //
    }
}

/// Get the load instruction that load `t` into its expected memory layout.
fn get_load_instr(t: Type, l: MemoryLayout, offset: u32) -> Result<Memory, String> {
    match t {
        Type::I32 => match l {
            MemoryLayout::U8 => Ok(Memory::I32Load8u { offset, align: 0 }),
            MemoryLayout::I32 => Ok(Memory::I32Load { offset, align: 2 }),
            _ => Err(format!("Unexpected memory layout for i32")),
        },
        Type::I64 => match l {
            MemoryLayout::U8 => Ok(Memory::I64Load8u { offset, align: 0 }),
            MemoryLayout::I64 => Ok(Memory::I64Load { offset, align: 3 }),
            _ => Err(format!("Unexpected memory layout for i64")),
        },
        Type::F32 => match l {
            MemoryLayout::F32 => Ok(Memory::F32Load { offset, align: 2 }),
            _ => Err(format!("Unexpected memory layout for f32")),
        },
        Type::F64 => match l {
            MemoryLayout::F64 => Ok(Memory::F64Load { offset, align: 3 }),
            _ => Err(format!("Unexpected memory layout for f64")),
        },
    }
}

/// Get the store instruction that store `t` into its expected memory layout.
fn get_store_instr(t: Type, l: MemoryLayout, offset: u32) -> Result<Memory, String> {
    match t {
        Type::I32 => match l {
            MemoryLayout::U8 => Ok(Memory::I32Store8 { offset, align: 0 }),
            MemoryLayout::I32 => Ok(Memory::I32Store { offset, align: 2 }),
            _ => Err(format!("Unexpected memory layout for i32")),
        },
        Type::I64 => match l {
            MemoryLayout::U8 => Ok(Memory::I64Store8 { offset, align: 0 }),
            MemoryLayout::I64 => Ok(Memory::I64Store { offset, align: 3 }),
            _ => Err(format!("Unexpected memory layout for i64")),
        },
        Type::F32 => match l {
            MemoryLayout::F32 => Ok(Memory::F32Store { offset, align: 2 }),
            _ => Err(format!("Unexpected memory layout for f32")),
        },
        Type::F64 => match l {
            MemoryLayout::F64 => Ok(Memory::F64Store { offset, align: 3 }),
            _ => Err(format!("Unexpected memory layout for f64")),
        },
    }
}

/// If the offset does not have the target alignment, increase the offset so that is has.
fn align_offset(offset: u32, target_alignment: u32) -> u32 {
    if offset % target_alignment == 0 {
        offset
    } else {
        offset + (target_alignment - (offset % target_alignment))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn offset() {
        assert_eq!(align_offset(0, 8), 0);
        assert_eq!(align_offset(1, 8), 8);
        assert_eq!(align_offset(5, 8), 8);
        assert_eq!(align_offset(7, 8), 8);
        assert_eq!(align_offset(8, 8), 8);
        assert_eq!(align_offset(9, 8), 16);
        assert_eq!(align_offset(20, 8), 24);

        assert_eq!(align_offset(0, 4), 0);
        assert_eq!(align_offset(1, 4), 4);
        assert_eq!(align_offset(2, 4), 4);
        assert_eq!(align_offset(3, 4), 4);
        assert_eq!(align_offset(4, 4), 4);
        assert_eq!(align_offset(5, 4), 8);

        assert_eq!(align_offset(42, 1), 42);
        assert_eq!(align_offset(43, 1), 43);
    }
}
