//! # Hir to Mir
//!
//! This file handles the lowering of Hir to Mir. The process involves scanning exposed functions
//! and recursively lower all items needed by those. By items we include functions, structs, data
//! and so on...
use std::collections::{HashMap, HashSet};

use super::mir::*;

use crate::arena::Arena;
use crate::ctx::{Ctx, KnownFunctions};
use crate::error::ErrorHandler;
use crate::hir::{
    AccessKind, Binop as HirBinop, Block as HirBlock, Body as HirBody, Data as HirData,
    Expression as Expr, FunKind, Function as HirFun, FunctionPrototype as HirFunProto,
    Import as HirImport, IntegerType as HirIntergerType, LocalId as HirLocalId,
    LocalVariable as HirLocalVariable, NonNullScalarType as HirNonNullScalarType,
    NumericType as HirNumericType, PlaceExpression as PlaceExpr, ScalarType as HirScalarType,
    Statement as S, Struct as HirStruct, Tuple as HirTuple, TupleId, Type as HirType,
    Unop as HirUnop, Value as V,
};
use crate::hir::{AsmControl, AsmLocal, AsmMemory, AsmParametric, AsmStatement};

enum FromBinop {
    Binop(Binop),
    Relop(Relop),
    Logical(Logical),
}

pub enum Place<'a> {
    Local(&'a [LocalId], Vec<Type>),
    Address {
        address_l_id: LocalId,
        offset: u32,
        t: &'a Vec<(Type, MemoryLayout, Offset)>,
    },
}

struct MIR {
    funs: Vec<Function>,
    imports: Vec<Imports>,
    data: HashMap<DataId, Data>,
}

struct HIR<'a> {
    funs: &'a HashMap<FunId, FunKind>,
    tuples: &'a HashMap<TupleId, HirTuple>,
    structs: &'a HashMap<StructId, HirStruct>,
    imports: &'a Vec<HirImport>,
    data: &'a HashMap<DataId, HirData>,
}

pub struct MirProducer<'a, 'arena> {
    // Counters for local IDs
    bb_id: BasicBlockId,
    local_id: LocalId,

    // Error handler
    err: &'a mut ErrorHandler,

    // A mapping from HIR local variable ID to MIR local variable ID
    locals: HashMap<HirLocalId, Vec<LocalId>>,

    // Functions used by the runtime, such as `malloc`
    known_funs: &'a KnownFunctions,

    // MIR & HIR items
    mir: MIR,
    hir: HIR<'a>,

    // Items to lower
    todo_funs: Vec<FunId>,
    todo_data: Vec<DataId>,

    // Set of items already lowered or registered for lowering
    lowered_funs: HashSet<FunId>,
    lowered_data: HashSet<DataId>,

    // Types are store in an external arena, so we don't mutably borrow self
    struct_arena: &'arena Arena<Struct>,
    tuple_arena: &'arena Arena<Tuple>,

    // Types are lowered on the fly, always use getters instead of accessing the map
    _structs: HashMap<StructId, &'arena Struct>,
    _tuples: HashMap<TupleId, &'arena Tuple>,
}

impl MIR {
    pub fn new() -> Self {
        Self {
            funs: Vec::new(),
            imports: Vec::new(),
            data: HashMap::new(),
        }
    }
}

impl<'a> HIR<'a> {
    pub fn new(ctx: &'a Ctx) -> Self {
        let funs = ctx.hir_funs();
        let tuples = ctx.hir_tuples();
        let structs = ctx.hir_structs();
        let imports = ctx.hir_imports();
        let data = ctx.hir_data();

        Self {
            funs,
            tuples,
            structs,
            imports,
            data,
        }
    }
}

impl<'a, 'arena> MirProducer<'a, 'arena> {
    fn new(
        ctx: &'a Ctx,
        known_funs: &'a KnownFunctions,
        struct_arena: &'arena Arena<Struct>,
        tuple_arena: &'arena Arena<Tuple>,
        err: &'a mut ErrorHandler,
    ) -> Self {
        Self {
            bb_id: 0,
            local_id: 0,
            locals: HashMap::new(),
            known_funs,
            err,
            mir: MIR::new(),
            hir: HIR::new(ctx),
            todo_funs: Vec::new(),
            todo_data: Vec::new(),
            lowered_funs: HashSet::new(),
            lowered_data: HashSet::new(),
            struct_arena,
            tuple_arena,
            _structs: HashMap::new(),
            _tuples: HashMap::new(),
        }
    }

    pub fn lower(
        ctx: &'a Ctx,
        known_funs: &'a KnownFunctions,
        err: &'a mut ErrorHandler,
    ) -> Program {
        let struct_arena = Arena::new();
        let tuple_arena = Arena::new();
        let reducer = MirProducer::new(ctx, known_funs, &struct_arena, &tuple_arena, err);
        let mir = reducer.do_lower();
        mir
    }

    fn do_lower(mut self) -> Program {
        // Register exposed functions
        for (fun_id, fun_kind) in self.hir.funs {
            match fun_kind {
                FunKind::Fun(fun) => {
                    if fun.exposed.is_some() {
                        self.use_fun(*fun_id);
                    }
                }
                _ => (),
            }
        }

        while let Some(fun_id) = self.todo_funs.pop() {
            // Retrieve HIR fun
            let fun = match self.hir.funs.get(&fun_id) {
                Some(fun) => match fun {
                    FunKind::Fun(fun) => fun,
                    FunKind::Extern(_) => {
                        continue;
                    }
                },
                None => {
                    self.err.report_internal_no_loc(format!(
                        "Can't lower hir fun: no fun with id '{}'",
                        fun_id
                    ));
                    continue;
                }
            };
            // Lower fun
            match self.lower_fun(fun) {
                Ok(fun) => self.mir.funs.push(fun),
                Err(err) => self.err.report_internal_no_loc(err),
            }
        }

        while let Some(data_id) = self.todo_data.pop() {
            // Retrieve HIR data
            let data = match self.hir.data.get(&data_id) {
                Some(data) => data,
                None => {
                    self.err.report_internal_no_loc(format!(
                        "Can't lower hir data: no data with id '{}'",
                        data_id
                    ));
                    continue;
                }
            };
            // Lower data
            self.mir.data.insert(data_id, MirProducer::lower_data(data));
        }

        for import in self.hir.imports {
            let mut prototypes = Vec::new();
            for fun_id in &import.prototypes {
                if !self.lowered_funs.contains(fun_id) {
                    // Function is not used
                    continue;
                }
                let proto = match self.hir.funs.get(&fun_id) {
                    Some(fun) => match fun {
                        FunKind::Extern(proto) => proto,
                        FunKind::Fun(_) => {
                            self.err.report_internal_no_loc(String::from(
                                "Import a non external function in HIR lowering",
                            ));
                            continue;
                        }
                    },
                    None => {
                        self.err.report_internal_no_loc(format!(
                            "Can't lower hir extern fun: no fun with id '{}'",
                            fun_id
                        ));
                        continue;
                    }
                };
                let proto = match self.lower_prototype(proto) {
                    Ok(proto) => proto,
                    Err(err) => {
                        self.err.report_internal_no_loc(err);
                        continue;
                    }
                };
                prototypes.push(proto);
            }

            if prototypes.len() > 0 {
                self.mir.imports.push(Imports {
                    from: import.from.clone(),
                    prototypes,
                })
            }
        }

        Program {
            funs: self.mir.funs,
            data: self.mir.data,
            imports: self.mir.imports,
        }
    }

    // —————————————————————————————————— Misc —————————————————————————————————— //

    /// Returns a globally unique basic block ID.
    fn fresh_bb_id(&mut self) -> BasicBlockId {
        let id = self.bb_id;
        self.bb_id += 1;
        id
    }

    /// Returns a globally unique local variable ID.
    fn fresh_local_id(&mut self) -> LocalId {
        let id = self.local_id;
        self.local_id += 1;
        id
    }

    /// Returns the MIR local ID corresponding to an HIR ID.
    ///
    /// ! Locals are assumed to be registered first, this function will panic if this assumption
    /// fail to be satisfied.
    fn get_local_ids(&self, id: HirLocalId) -> &Vec<LocalId> {
        self.locals.get(&id).unwrap()
    }

    /// Creates a new mapping Hir Local -> Vec<Mir Local>
    fn register_locals(&mut self, id: HirLocalId, locals: Vec<LocalId>) {
        self.locals.insert(id, locals);
    }

    // —————————————————————————— Register items usage —————————————————————————— //
    //                                                                            //
    // HIR items are lazily lowered to MIR, that way only items required by the   //
    // program ends up in the Wasm file.                                          //
    // Whenever an item is used it _must_ be registered, so that it will be       //
    // in the executable file.                                                    //
    // For instance when using a `DataPointer(DataId)`, the `DataId` must be      //
    // registered.                                                                //
    //                                                                            //
    // —————————————————————————————————————————————————————————————————————————— //

    fn use_fun(&mut self, fun_id: FunId) {
        if !self.lowered_funs.contains(&fun_id) {
            self.lowered_funs.insert(fun_id);
            self.todo_funs.push(fun_id);
        }
    }

    fn use_data(&mut self, data_id: DataId) {
        if !self.lowered_data.contains(&data_id) {
            self.lowered_data.insert(data_id);
            self.todo_data.push(data_id);
        }
    }

    fn get_struct(&mut self, s_id: &StructId) -> Result<&'arena Struct, String> {
        if let Some(s) = self._structs.get(s_id) {
            return Ok(s);
        }
        match self.lower_struct(*s_id) {
            Ok(s) => {
                let s_ref = self.struct_arena.alloc(s);
                self._structs.insert(*s_id, s_ref);
                Ok(s_ref)
            }
            Err(err) => Err(err),
        }
    }

    fn get_tuple(&mut self, tup_id: &TupleId) -> Result<&'arena Tuple, String> {
        if let Some(tup) = self._tuples.get(tup_id) {
            return Ok(tup);
        }
        match self.lower_tuple(*tup_id) {
            Ok(tup) => {
                let tup_ref = self.tuple_arena.alloc(tup);
                self._tuples.insert(*tup_id, tup_ref);
                Ok(tup_ref)
            }
            Err(err) => Err(err),
        }
    }

    // ————————————————————————————— Items lowering ————————————————————————————— //

    /// Decides of the memory layout of the structs.
    ///
    /// The memory blocks returned by malloc are guaranteed to have an alignment of 8, this
    /// function should arrange all the fields so that all of them have an alignment suitable for
    /// their types while minimizing unused space.
    fn lower_struct(&mut self, s_id: StructId) -> Result<Struct, String> {
        let mut align_1 = Vec::new();
        let mut align_4 = Vec::new();
        let mut align_8 = Vec::new();
        let s = match self.hir.structs.get(&s_id) {
            Some(s) => s,
            None => {
                return Err(format!(
                    "Could not lower struct: no struct with id '{}'",
                    s_id
                ))
            }
        };

        let mut fields = HashMap::with_capacity(s.fields.len());
        // Collect alignments and sizes
        for (field_name, field) in &s.fields {
            // Compute memory layout of the field
            let t = match self.try_into_mir_layout(&field.t) {
                Ok(t) => t,
                Err(e) => {
                    self.err.report_internal_no_loc(e);
                    continue;
                }
            };
            let (alignment, size) = self.get_alignment(&field.t)?;
            match alignment {
                Alignment::A1 => align_1.push((field_name, size, t)),
                Alignment::A4 => align_4.push((field_name, size, t)),
                Alignment::A8 => align_8.push((field_name, size, t)),
            }
        }

        // Decide of the layout, this can be optimized in the future
        let mut offset = 0;
        for (field_name, size, t) in align_8.drain(..) {
            offset = align_offset(offset, Alignment::A8);
            fields.insert(field_name.to_owned(), StructField { offset, t });
            offset += size;
        }
        for (field_name, size, t) in align_4.drain(..) {
            offset = align_offset(offset, Alignment::A4);
            fields.insert(field_name.to_owned(), StructField { offset, t });
            offset += size;
        }
        for (field_name, size, t) in align_1.drain(..) {
            fields.insert(field_name.to_owned(), StructField { offset, t });
            offset += size;
        }
        // Layout is now fixed
        Ok(Struct {
            fields,
            size: offset,
        })
    }

    /// Decides of the memory layout of the tuples and their representation as local variables.
    ///
    /// The memory blocks returned by malloc are guaranteed to have an alignment of 8, this
    /// function should arrange all the fields so that all of them have an alignment suitable for
    /// their types while minimizing unused space.
    fn lower_tuple(&mut self, tup_id: TupleId) -> Result<Tuple, String> {
        let mut align_1 = Vec::new();
        let mut align_4 = Vec::new();
        let mut align_8 = Vec::new();
        let tup = match self.hir.tuples.get(&tup_id) {
            Some(tup) => tup,
            None => {
                return Err(format!(
                    "Could not lower tuple: no tuple with id '{}'",
                    tup_id
                ));
            }
        };

        let mut fields = Vec::with_capacity(tup.types.len());
        for t in &tup.types {
            // Compute memory layout of the field
            let ts = match self.try_into_mir_layout(t) {
                Ok(t) => t,
                Err(e) => {
                    self.err.report_internal_no_loc(e);
                    continue;
                }
            };
            let (alignment, size) = self.get_alignment(&t)?;
            match alignment {
                Alignment::A1 => align_1.push((size, ts)),
                Alignment::A4 => align_4.push((size, ts)),
                Alignment::A8 => align_8.push((size, ts)),
            }
        }

        // Decide of the layout, this can be optimized in the future
        let mut offset = 0;
        let mut local_offset = 0;
        for (size, t) in align_8.drain(..) {
            offset = align_offset(offset, Alignment::A8);
            let nb_locals = t.len();
            fields.push(TupleField {
                offset,
                t,
                local_offset,
                nb_locals,
            });
            offset += size;
            local_offset += nb_locals;
        }
        for (size, t) in align_4.drain(..) {
            offset = align_offset(offset, Alignment::A4);
            let nb_locals = t.len();
            fields.push(TupleField {
                offset,
                t,
                local_offset,
                nb_locals,
            });
            offset += size;
            local_offset += nb_locals;
        }
        for (size, t) in align_1.drain(..) {
            let nb_locals = t.len();
            fields.push(TupleField {
                offset,
                t,
                local_offset,
                nb_locals,
            });
            offset += size;
            local_offset += nb_locals;
        }
        Ok(Tuple {
            size: offset,
            nb_locals: local_offset,
            fields,
        })
    }

    fn lower_data(data: &HirData) -> Data {
        let data = match data {
            HirData::Str(_, data) => data.to_owned(),
        };
        data
    }

    fn lower_fun(&mut self, fun: &HirFun) -> Result<Function, String> {
        let t = &fun.t;
        let mut param_t = Vec::with_capacity(t.params.len());
        let mut locals = Vec::with_capacity(fun.locals.len());
        let mut params = Vec::with_capacity(fun.params.len());

        // Convert params and return types
        for t in &t.params {
            param_t.extend(self.try_into_mir_t(&t)?);
        }
        let ret_t = self.try_into_mir_t(&t.ret)?;
        // Register params and local variables
        assert!(fun.params.len() == fun.t.params.len());
        for (param_local_id, param_t) in fun.params.iter().zip(fun.t.params.iter()) {
            let mir_param_t = self.try_into_mir_t(param_t)?;
            let mut local_ids = Vec::with_capacity(mir_param_t.len());
            for _ in self.try_into_mir_t(param_t)? {
                local_ids.push(self.fresh_local_id());
            }
            params.extend(local_ids.clone());
            self.register_locals(*param_local_id, local_ids);
        }
        for l in &fun.locals {
            let mir_locals = self.lower_local_variable(l)?;
            self.register_locals(l.id, mir_locals.iter().map(|l| l.id).collect());
            locals.extend(mir_locals);
        }
        // Reduce function body
        let (block, block_locals) = match &fun.body {
            HirBody::Zephyr(block) => self.lower_block(block)?,
            HirBody::Asm(stmts) => (
                Block::Block {
                    id: self.fresh_bb_id(),
                    stmts: self.lower_asm_statements(stmts)?,
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

    fn lower_local_variable(
        &mut self,
        local: &HirLocalVariable,
    ) -> Result<Vec<LocalVariable>, String> {
        let types = self.try_into_mir_t(&local.t)?;
        let mut locals = Vec::with_capacity(types.len());
        for t in types {
            locals.push(LocalVariable {
                id: self.fresh_local_id(),
                t,
            })
        }
        Ok(locals)
    }

    /// Lowers a block of statements, local variables may be created for the need of computations
    /// and are returned along the reduced block.
    fn lower_block(&mut self, block: &HirBlock) -> Result<(Block, Vec<LocalVariable>), String> {
        let id = self.fresh_bb_id();
        let mut stmts = Vec::new();
        let mut locals = Vec::new();
        self.lower_block_rec(block, &mut stmts, &mut locals)?;
        let reduced_block = Block::Block { id, stmts, t: None };
        Ok((reduced_block, locals))
    }

    fn lower_block_rec(
        &mut self,
        block: &HirBlock,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
    ) -> Result<(), String> {
        for statement in &block.stmts {
            match statement {
                S::AssignStmt { target, expr } => {
                    self.lower_assign_stmt(target, expr, stmts, locals)?;
                }
                S::LetStmt { var, expr } => {
                    self.lower_expr(&expr, stmts, locals)?;
                    for l_id in self.get_local_ids(var.n_id).iter().rev() {
                        stmts.push(Statement::Local(Local::Set(*l_id)));
                    }
                }
                S::ExprStmt(expr) => {
                    let values = self.lower_expr(&expr, stmts, locals)?;
                    // drop unused values
                    for _ in values {
                        stmts.push(Statement::Parametric(Parametric::Drop));
                    }
                }
                S::ReturnStmt { expr, .. } => {
                    if let Some(e) = expr {
                        self.lower_expr(&e, stmts, locals)?;
                    }
                    stmts.push(Statement::Control(Control::Return))
                }
                S::WhileStmt { expr, block } => {
                    let block_id = self.fresh_bb_id();
                    let loop_id = self.fresh_bb_id();
                    let mut loop_stmts = Vec::new();

                    self.lower_expr(&expr, &mut loop_stmts, locals)?;
                    // If NOT expr, then jump to end of block
                    loop_stmts.push(Statement::Const(Value::I32(1)));
                    loop_stmts.push(Statement::Binop(Binop::I32Xor));
                    loop_stmts.push(Statement::Control(Control::BrIf(block_id)));

                    self.lower_block_rec(&block, &mut loop_stmts, locals)?;
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
                    self.lower_expr(&expr, stmts, locals)?;
                    let if_id = self.fresh_bb_id();
                    let mut then_stmts = Vec::new();
                    self.lower_block_rec(&block, &mut then_stmts, locals)?;
                    let mut else_stmts = Vec::new();
                    if let Some(else_block) = else_block {
                        self.lower_block_rec(&else_block, &mut else_stmts, locals)?;
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
    /// on top of the stack.
    fn lower_expr(
        &mut self,
        expression: &Expr,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
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
                    self.use_data(*data_id);
                    stmts.push(Statement::Const(Value::DataPointer(*data_id)));
                    vec![Type::I32]
                }
                V::Tuple { values, .. } => {
                    let mut types = Vec::with_capacity(values.len());
                    for val in values {
                        types.extend(self.lower_expr(val, stmts, locals)?);
                    }
                    types
                }
                V::Struct {
                    struct_id, fields, ..
                } => {
                    let struc = self.get_struct(struct_id)?;
                    // Allocate memory
                    stmts.push(Statement::Const(Value::I32(struc.size as i32)));
                    stmts.push(Statement::Call(Call::Direct(self.known_funs.malloc)));
                    self.use_fun(self.known_funs.malloc); // Don't forget to register malloc!
                                                          // Save the pointer in a local variable
                    let pointer_l_id = self.fresh_local_id();
                    locals.push(LocalVariable {
                        t: Type::I32,
                        id: pointer_l_id,
                    });
                    stmts.push(Statement::Local(Local::Set(pointer_l_id)));
                    // Now fill the fields with their values
                    for field in fields {
                        let (layout, offset) = if let Some(f) = struc.fields.get(&field.ident) {
                            (&f.t, f.offset)
                        } else {
                            self.err.report_internal_no_loc(format!(
                                "Field does not exist in MIR struct: '{}'",
                                &field.ident
                            ));
                            continue;
                        };
                        // Put memory location and value on top of stack
                        stmts.push(Statement::Local(Local::Get(pointer_l_id)));
                        let values_types = self.lower_expr(&*field.expr, stmts, locals)?;
                        if values_types.len() != layout.len() {
                            self.err.report_internal_no_loc(format!(
                                "Number of value miss match in field: expected {}, got {}",
                                values_types.len(),
                                layout.len()
                            ));
                            continue;
                        }
                        // Store values one by one
                        for (t, (t_2, t_layout, t_offset)) in values_types.iter().zip(layout) {
                            assert_eq!(t, t_2);
                            stmts.push(Statement::Memory(get_store_instr(
                                *t,
                                *t_layout,
                                offset + t_offset,
                            )?));
                        }
                    }
                    // Put the struct pointer on top of the stack
                    stmts.push(Statement::Local(Local::Get(pointer_l_id)));
                    vec![Type::I32]
                }
            },
            Expr::Variable(var) => {
                for l_id in self.get_local_ids(var.n_id) {
                    stmts.push(Statement::Local(Local::Get(*l_id)));
                }
                self.try_into_mir_t(&var.t)?
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
                        self.lower_expr(expr_left, stmts, locals)?;
                        self.lower_expr(expr_right, stmts, locals)?;
                        stmts.push(Statement::Binop(binop));
                        vec![t]
                    }
                    FromBinop::Relop(relop) => {
                        let t = relop.get_t();
                        self.lower_expr(expr_left, stmts, locals)?;
                        self.lower_expr(expr_right, stmts, locals)?;
                        stmts.push(Statement::Relop(relop));
                        vec![t]
                    }
                    FromBinop::Logical(logical) => match logical {
                        Logical::And => {
                            let if_id = self.fresh_bb_id();
                            let mut then_stmts = Vec::new();
                            self.lower_expr(expr_right, &mut then_stmts, locals)?;
                            let else_stmts = vec![Statement::Const(Value::I32(0))];
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.lower_expr(expr_left, stmts, locals)?;
                            stmts.push(Statement::Block(Box::new(if_block)));
                            vec![Type::I32]
                        }
                        Logical::Or => {
                            let if_id = self.fresh_bb_id();
                            let then_stmts = vec![Statement::Const(Value::I32(1))];
                            let mut else_stmts = Vec::new();
                            self.lower_expr(expr_right, &mut else_stmts, locals)?;
                            let if_block = Block::If {
                                id: if_id,
                                then_stmts,
                                else_stmts,
                                t: Some(Type::I32),
                            };
                            self.lower_expr(expr_left, stmts, locals)?;
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
                        self.lower_expr(expr, stmts, locals)?;
                        stmts.push(Statement::Binop(Binop::I32Sub));
                        vec![Type::I32]
                    }
                    HirNumericType::I64 => {
                        stmts.push(Statement::Const(Value::I64(0)));
                        self.lower_expr(expr, stmts, locals)?;
                        stmts.push(Statement::Binop(Binop::I64Sub));
                        vec![Type::I64]
                    }
                    HirNumericType::F32 => {
                        self.lower_expr(expr, stmts, locals)?;
                        stmts.push(Statement::Unop(Unop::F32Neg));
                        vec![Type::F32]
                    }
                    HirNumericType::F64 => {
                        self.lower_expr(expr, stmts, locals)?;
                        stmts.push(Statement::Unop(Unop::F32Neg));
                        vec![Type::F64]
                    }
                },
                HirUnop::Not => {
                    stmts.push(Statement::Const(Value::I32(1)));
                    self.lower_expr(expr, stmts, locals)?;
                    stmts.push(Statement::Binop(Binop::I32Sub));
                    vec![Type::I32]
                }
            },
            Expr::CallDirect {
                fun_id, args, t, ..
            } => {
                self.use_fun(*fun_id);
                for arg in args {
                    self.lower_expr(arg, stmts, locals)?;
                }
                stmts.push(Statement::Call(Call::Direct(*fun_id)));
                self.try_into_mir_t(&t.ret)?
            }
            Expr::CallIndirect { loc, .. } => {
                self.err
                    .report(*loc, String::from("Indirect call are not yet supported"));
                todo!()
            }
            Expr::Access {
                expr, kind, t: _t, ..
            } => match kind {
                AccessKind::Struct { field, s_id } => {
                    let struc = self.get_struct(s_id)?;
                    let field = struc.fields.get(field).unwrap();
                    let mut types = Vec::with_capacity(field.t.len());
                    self.lower_expr(expr, stmts, locals)?;
                    for (t, layout, offset) in &field.t {
                        stmts.push(Statement::Memory(get_load_instr(
                            *t,
                            *layout,
                            field.offset + offset,
                        )?));
                        types.push(*t);
                    }
                    types
                }
                AccessKind::Tuple { tup_id, index } => {
                    // For now let's evaluate the whole expression, this can be optimized when
                    // dealing with a place expression (i.e. fetching only the required part)
                    self.lower_expr(expr, stmts, locals)?;
                    let tup = self.get_tuple(tup_id)?;
                    let field = &tup.fields[*index as usize];
                    let mut types = Vec::with_capacity(field.t.len());
                    let mut tmp_var = Vec::with_capacity(field.t.len());
                    let nb_locals = field.nb_locals;
                    let nb_before = field.local_offset;
                    let nb_after = tup.nb_locals - nb_before - nb_locals;
                    // Drop values after the values of interests
                    for _ in 0..nb_after {
                        stmts.push(Statement::Parametric(Parametric::Drop));
                    }
                    // Store values in temporary variables (could be optimized if nb_before == 0)
                    for (t, _, _) in field.t.iter().rev() {
                        types.push(*t);
                        let tmp_var_id = self.fresh_local_id();
                        locals.push(LocalVariable {
                            id: tmp_var_id,
                            t: *t,
                        });
                        stmts.push(Statement::Local(Local::Set(tmp_var_id)));
                        tmp_var.push(tmp_var_id);
                    }
                    // Drop values before those of interest
                    for _ in 0..nb_before {
                        stmts.push(Statement::Parametric(Parametric::Drop));
                    }
                    // Restore values
                    for tmp_var_id in tmp_var.iter().rev() {
                        stmts.push(Statement::Local(Local::Get(*tmp_var_id)));
                    }
                    types
                }
            },
            Expr::Nop { .. } => vec![],
        };
        Ok(types)
    }

    /// Reduces an assign statement (`target = expr`).
    fn lower_assign_stmt(
        &mut self,
        place: &PlaceExpr,
        expr: &Expr,
        stmts: &mut Vec<Statement>,
        locals: &mut Vec<LocalVariable>,
    ) -> Result<(), String> {
        // Push values on the stack
        self.lower_expr(&expr, stmts, locals)?;
        // Compute memory location (no effect on the stack)
        let place = self.lower_place_expression(place)?;
        match place {
            Place::Local(locals_ids, _) => {
                for l_id in locals_ids.iter().rev() {
                    stmts.push(Statement::Local(Local::Set(*l_id)));
                }
            }
            Place::Address {
                address_l_id,
                offset,
                t,
            } => {
                // Release mut ref
                let t = t.clone();
                // Iterate on types in reverse order (stack => last in, first out)
                for (t, t_layout, t_offset) in t.iter().rev() {
                    // Create a local to store temporary result
                    let l_id = self.fresh_local_id();
                    locals.push(LocalVariable { t: *t, id: l_id });
                    stmts.push(Statement::Local(Local::Set(l_id)));
                    // Push the address on the stack
                    stmts.push(Statement::Local(Local::Get(address_l_id)));
                    // Push the value on the stack
                    stmts.push(Statement::Local(Local::Get(l_id)));
                    // Store the value
                    let store_instr = get_store_instr(*t, *t_layout, offset + t_offset)?;
                    stmts.push(Statement::Memory(store_instr));
                }
            }
        }
        Ok(())
    }

    fn lower_place_expression(&mut self, place: &PlaceExpr) -> Result<Place, String> {
        match place {
            PlaceExpr::Variable(var) => {
                let types = self.try_into_mir_t(&var.t)?;
                let locals = self.get_local_ids(var.n_id);
                Ok(Place::Local(locals, types))
            }
            PlaceExpr::Access { expr, kind, .. } => match kind {
                AccessKind::Struct { field, s_id } => {
                    let place = self.lower_place_expression(expr)?;
                    let (address_l_id, total_offset) = match place {
                        Place::Address {
                            address_l_id,
                            offset,
                            ..
                        } => (address_l_id, offset),
                        Place::Local(locals_ids, _) => {
                            if locals_ids.len() != 1 {
                                return Err(String::from(
                                    "Struct must be represented by their pointers",
                                ));
                            }
                            (locals_ids[0], 0)
                        }
                    };
                    let struc = self.get_struct(s_id)?;
                    let field = struc.fields.get(field).unwrap();
                    let offset = field.offset;
                    Ok(Place::Address {
                        address_l_id,
                        offset: offset + total_offset,
                        t: &field.t,
                    })
                }
                AccessKind::Tuple { index, tup_id } => {
                    let tup = self.get_tuple(tup_id)?;
                    let field = &tup.fields[*index as usize];
                    let place = self.lower_place_expression(expr)?;
                    match place {
                        Place::Address {
                            address_l_id,
                            offset,
                            ..
                        } => Ok(Place::Address {
                            address_l_id,
                            offset: offset + field.offset,
                            t: &field.t,
                        }),
                        Place::Local(locals, _) => {
                            let locals_start = field.local_offset;
                            let locals_end = locals_start + field.nb_locals;
                            let mut types = Vec::with_capacity(field.t.len());
                            for (t, _, _) in &field.t {
                                types.push(*t);
                            }
                            Ok(Place::Local(&locals[locals_start..locals_end], types))
                        }
                    }
                }
            },
        }
    }

    fn lower_asm_statements(
        &mut self,
        stmts: &Vec<AsmStatement>,
    ) -> Result<Vec<Statement>, String> {
        let mut reduced_stmts = Vec::with_capacity(stmts.len());
        for stmt in stmts {
            match self.lower_asm_statement(stmt) {
                Ok(stmt) => reduced_stmts.push(stmt),
                Err(err) => self.err.report_no_loc(err), //TODO: track location
            }
        }
        Ok(reduced_stmts)
    }

    fn lower_asm_statement(&mut self, stmt: &AsmStatement) -> Result<Statement, String> {
        match stmt {
            AsmStatement::Const { ref val, .. } => Ok(Statement::Const(val.clone())),
            AsmStatement::Local { local, .. } => match local {
                AsmLocal::Get { var, .. } => {
                    let locals = self.get_local_ids(var.n_id);
                    assert!(locals.len() == 1);
                    Ok(Statement::Local(Local::Get(locals[0])))
                }
                AsmLocal::Set { var } => {
                    let locals = self.get_local_ids(var.n_id);
                    assert!(locals.len() == 1);
                    Ok(Statement::Local(Local::Set(locals[0])))
                }
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

    fn lower_prototype(&mut self, proto: &HirFunProto) -> Result<FunctionPrototype, String> {
        let mut param_t = Vec::with_capacity(proto.t.params.len());

        for param in &proto.t.params {
            match self.try_into_mir_t(&param) {
                Ok(t) => param_t.extend(t),
                Err(s) => return Err(s),
            }
        }
        let ret_t = match self.try_into_mir_t(&proto.t.ret) {
            Ok(t) => t,
            Err(s) => return Err(s),
        };

        Ok(FunctionPrototype {
            ident: proto.ident.clone(),
            param_t,
            ret_t,
            alias: proto.alias.clone(),
            is_pub: proto.is_pub,
            fun_id: proto.fun_id,
        })
    }

    // —————————————————————————————— Type lowering ————————————————————————————— //

    /// Try to convert an arbitrary HIR type to an MIR type.
    fn try_into_mir_t(&mut self, t: &HirType) -> Result<Vec<Type>, String> {
        match t {
            HirType::Scalar(t) => Ok(match get_mir_t(t) {
                Some(t) => vec![t],
                None => vec![],
            }),
            HirType::Fun(_) => Err(String::from("Function as value are not yet supported.")),
            HirType::Tuple(tup_id) => {
                let tup = self.get_tuple(tup_id)?;
                let mut types = Vec::with_capacity(tup.fields.len());
                for field in &tup.fields {
                    for (t, _, _) in &field.t {
                        types.push(*t);
                    }
                }
                Ok(types)
            }
            // For now structs are always boxed and represented by a pointer to their location
            HirType::Struct(_) => Ok(vec![Type::I32]),
        }
    }

    /// Try to convert an arbitrary HIR type to any number of MIR types along with their layouts.
    fn try_into_mir_layout(
        &mut self,
        t: &HirType,
    ) -> Result<Vec<(Type, MemoryLayout, Offset)>, String> {
        match t {
            HirType::Scalar(t) => Ok(match get_mir_t(t) {
                Some(t) => vec![(t, t.layout(), 0)],
                None => vec![],
            }),
            HirType::Fun(_) => Err(String::from("Functions as value are not yet supported.")),
            HirType::Tuple(tup_id) => {
                let tup = self.get_tuple(tup_id)?;
                let mut types = Vec::with_capacity(tup.fields.len());
                for fields in &tup.fields {
                    let field_offset = fields.offset;
                    for (t, layout, offset) in &fields.t {
                        types.push((*t, *layout, field_offset + offset));
                    }
                }
                Ok(types)
            }
            // For now structs are always boxed and represented by a pointer to their location
            HirType::Struct(_) => Ok(vec![(Type::I32, MemoryLayout::I32, 0)]),
        }
    }

    /// Returns the alignment and size a given type occupy in memory.
    fn get_alignment(&mut self, t: &HirType) -> Result<(Alignment, u32), String> {
        match t {
            HirType::Scalar(x) => Ok(match x {
                HirScalarType::I32 => (Alignment::A4, 4),
                HirScalarType::I64 => (Alignment::A8, 8),
                HirScalarType::F32 => (Alignment::A4, 4),
                HirScalarType::F64 => (Alignment::A8, 8),
                HirScalarType::Bool => (Alignment::A1, 1),
                HirScalarType::Null => (Alignment::A1, 0),
            }),
            HirType::Struct(_) => Ok((Alignment::A4, 4)), // Represented as a i32 pointer for now
            HirType::Tuple(tup_id) => {
                let tup = self.get_tuple(tup_id)?;
                Ok((Alignment::A8, tup.size)) // We can optimize alignment in some cases
            }
            _ => todo!("Only scalar and struct are supported inside structures at the time"), //
        }
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
            HirNonNullScalarType::I32 => FromBinop::Relop(Relop::I32Eq),
            HirNonNullScalarType::I64 => FromBinop::Relop(Relop::I64Eq),
            HirNonNullScalarType::F32 => FromBinop::Relop(Relop::F32Eq),
            HirNonNullScalarType::F64 => FromBinop::Relop(Relop::F64Eq),
            HirNonNullScalarType::Bool => FromBinop::Relop(Relop::I32Eq),
        },
        HirBinop::Ne(t) => match t {
            HirNonNullScalarType::I32 => FromBinop::Relop(Relop::I32Ne),
            HirNonNullScalarType::I64 => FromBinop::Relop(Relop::I64Ne),
            HirNonNullScalarType::F32 => FromBinop::Relop(Relop::F32Ne),
            HirNonNullScalarType::F64 => FromBinop::Relop(Relop::F64Ne),
            HirNonNullScalarType::Bool => FromBinop::Relop(Relop::I32Ne),
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
fn get_mir_t(t: &HirScalarType) -> Option<Type> {
    match t {
        HirScalarType::I32 => Some(Type::I32),
        HirScalarType::I64 => Some(Type::I64),
        HirScalarType::F32 => Some(Type::F32),
        HirScalarType::F64 => Some(Type::F64),
        HirScalarType::Bool => Some(Type::I32),
        HirScalarType::Null => None,
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
fn align_offset(offset: u32, target_alignment: Alignment) -> u32 {
    let target_alignment = target_alignment.bytes();
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
        assert_eq!(align_offset(0, Alignment::A8), 0);
        assert_eq!(align_offset(1, Alignment::A8), 8);
        assert_eq!(align_offset(5, Alignment::A8), 8);
        assert_eq!(align_offset(7, Alignment::A8), 8);
        assert_eq!(align_offset(8, Alignment::A8), 8);
        assert_eq!(align_offset(9, Alignment::A8), 16);
        assert_eq!(align_offset(20, Alignment::A8), 24);

        assert_eq!(align_offset(0, Alignment::A4), 0);
        assert_eq!(align_offset(1, Alignment::A4), 4);
        assert_eq!(align_offset(2, Alignment::A4), 4);
        assert_eq!(align_offset(3, Alignment::A4), 4);
        assert_eq!(align_offset(4, Alignment::A4), 4);
        assert_eq!(align_offset(5, Alignment::A4), 8);

        assert_eq!(align_offset(42, Alignment::A1), 42);
        assert_eq!(align_offset(43, Alignment::A1), 43);
    }
}
