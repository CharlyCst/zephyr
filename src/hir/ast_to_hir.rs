use super::hir::*;
use super::names::{
    Block as NameBlock, Body as NameBody, Expression as Expr, FunId, Function as NameFun,
    FunctionPrototype as NameFunProto, Imports as NameImports, NameStore, Statement as S,
    Struct as NameStruct, TypeId, TypeStore, Value as V, Variable as NameVariable,
};
use super::types::{ScalarType as ASTScalarType, Type as ASTTypes, TypeVarTypes, TypedProgram};

use crate::ast::{BinaryOperator as ASTBinop, UnaryOperator as ASTUnop};
use crate::error::{ErrorHandler, Location};

use std::collections::HashMap;

struct State {
    pub names: NameStore,
    pub type_vars: TypeVarTypes,
    pub types: TypeStore,
    pub fun_types: HashMap<FunId, TypeId>,
}

impl State {
    pub fn new(
        names: NameStore,
        type_vars: TypeVarTypes,
        types: TypeStore,
        fun_types: HashMap<FunId, TypeId>,
    ) -> State {
        State {
            names,
            type_vars,
            types,
            fun_types,
        }
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
        let mut state = State::new(prog.names, prog.type_vars, prog.types, prog.fun_types);
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

        let structs = prog.structs.transmute(|s| match self.reduce_struct(s) {
            Ok(s) => Some(s),
            Err(err) => {
                self.err.report_internal_no_loc(err);
                None
            }
        });

        Program {
            funs,
            imports,
            structs,
            data: prog.data,
            pub_decls: prog.pub_decls,
            package: prog.package,
        }
    }

    fn reduce_fun(&mut self, fun: NameFun, s: &mut State) -> Result<Function, String> {
        let fun_t_id = *s
            .fun_types
            .get(&fun.fun_id)
            .ok_or(format!("No t_id for fun_id '{}'", fun.fun_id))?;
        let fun_t = s
            .types
            .get(fun_t_id)
            .ok_or(format!("No type ofr t_id '{}'", fun_t_id))?;
        let t = match fun_t {
            ASTTypes::Fun(t) => t.lower()?,
            _ => return Err(String::from("Function does not have function type")),
        };
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
            let t = match s.type_vars.get(&t_id) {
                Some(t) => t.lower()?,
                None => return Err(format!("Type id '{}' is invalid", t_id)),
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
            S::AssignStmt { target, expr } => {
                let expr = self.reduce_expr(expr, s)?;
                let target = self.reduce_expr(target, s)?;
                let target = self.as_place(target)?;
                Ok(Statement::AssignStmt { target, expr })
            }
            S::LetStmt { var, expr } => {
                let expr = self.reduce_expr(expr, s)?;
                let var = self.reduce_var(var, s)?;
                Ok(Statement::LetStmt { expr, var })
            }
            S::ExprStmt(expr) => {
                let expr = self.reduce_expr(expr, s)?;
                Ok(Statement::ExprStmt(expr))
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
                let expr = self.reduce_expr(expr, s)?;
                let block = self.reduce_block(block, s)?;
                Ok(Statement::WhileStmt { expr, block })
            }
            S::IfStmt {
                expr,
                block,
                else_block,
            } => {
                let expr = self.reduce_expr(expr, s)?;
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
            Expr::Literal(value) => Ok(Expression::Literal(match value {
                V::Integer { val, t_id, loc } => {
                    match s
                        .type_vars
                        .get(&t_id)
                        .ok_or(format!("Invalid t_id '{}'", t_id))?
                    {
                        ASTTypes::Scalar(ASTScalarType::I32) => Value::I32(val as i32, loc),
                        ASTTypes::Scalar(ASTScalarType::I64) => Value::I64(val as i64, loc),
                        _ => return Err(String::from("Integer constant of non integer type.")),
                    }
                }
                V::Float { val, t_id, loc } => match s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                {
                    ASTTypes::Scalar(ASTScalarType::F32) => Value::F32(val as f32, loc),
                    ASTTypes::Scalar(ASTScalarType::F64) => Value::F64(val, loc),
                    _ => return Err(String::from("Float constant of non float type.")),
                },
                V::Boolean { val, t_id, loc } => match s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                {
                    ASTTypes::Scalar(ASTScalarType::Bool) => Value::Bool(val, loc),
                    _ => return Err(String::from("Boolean constant of non boolean type.")),
                },
                V::Str {
                    data_id,
                    len,
                    loc,
                    t_id,
                } => match s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                {
                    ASTTypes::Struct(struct_id) => {
                        let len = FieldValue {
                            ident: String::from("len"),
                            expr: Box::new(Expression::Literal(Value::I32(len as i32, loc))),
                            loc,
                        };
                        let start = FieldValue {
                            ident: String::from("start"),
                            expr: Box::new(Expression::Literal(Value::DataPointer(data_id, loc))),
                            loc,
                        };
                        let fields = vec![len, start];
                        Value::Struct {
                            struct_id: *struct_id,
                            fields,
                            loc,
                        }
                    }
                    _ => return Err(String::from("Str literal of non struct type.")),
                },
                V::Struct {
                    fields, t_id, loc, ..
                } => match s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                {
                    ASTTypes::Struct(struct_id) => {
                        let mut hir_fields = Vec::with_capacity(fields.len());
                        for field in fields {
                            hir_fields.push(FieldValue {
                                ident: field.ident,
                                loc: field.loc,
                                expr: Box::new(self.reduce_expr(*field.expr, s)?),
                            });
                        }
                        Value::Struct {
                            struct_id: *struct_id,
                            fields: hir_fields,
                            loc,
                        }
                    }
                    _ => return Err(String::from("Struct literal of non struct type.")),
                },
                V::Tuple {
                    values, t_id, loc, ..
                } => match s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                {
                    ASTTypes::Tuple(tup) => {
                        assert!(tup.0.len() == values.len());
                        let mut hir_values = Vec::with_capacity(values.len());
                        for val in values {
                            hir_values.push(self.reduce_expr(val, s)?);
                        }
                        Value::Tuple(hir_values, loc)
                    },
                    _ => return Err(String::from("Tuple literal of non tuple type.")),
                },
            })),
            Expr::Variable(var) => {
                let name = s.names.get(var.n_id);
                let t = s
                    .type_vars
                    .get(&name.t_id)
                    .ok_or(format!("Invalid t_id '{}'", name.t_id))?;
                Ok(Expression::Variable(Variable {
                    ident: var.ident,
                    loc: var.loc,
                    n_id: var.n_id,
                    t: t.lower()?,
                }))
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
                let t = s
                    .type_vars
                    .get(&op_t_id)
                    .ok_or(format!("Invalid t_id '{}'", op_t_id))?;
                let t = t.lower_to_scalar()?;
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
                        ASTBinop::Equal => Binop::Eq(self.t_as_non_null_scalar(&t, loc)),
                        ASTBinop::NotEqual => Binop::Ne(self.t_as_non_null_scalar(&t, loc)),
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
                let t = s
                    .type_vars
                    .get(&op_t_id)
                    .ok_or(format!("Invalid t_id '{}'", op_t_id))?;
                let t = t.lower_to_scalar()?;
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
                let t = s
                    .type_vars
                    .get(&fun_t_id)
                    .ok_or(format!("Invalid t_id '{}'", fun_t_id))?;
                let t = t.lower_to_fun()?;
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
            Expr::Access {
                expr,
                field,
                t_id,
                struct_t_id,
                loc,
            } => {
                let expr = Box::new(self.reduce_expr(*expr, s)?);
                let t = s
                    .type_vars
                    .get(&t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                    .lower()?;
                let struct_t = s
                    .type_vars
                    .get(&struct_t_id)
                    .ok_or(format!("Invalid t_id '{}'", t_id))?
                    .lower()?;
                if let Type::Struct(struct_id) = struct_t {
                    Ok(Expression::Access {
                        expr,
                        field,
                        struct_id,
                        t,
                        loc,
                    })
                } else {
                    println!("expr: {}", expr);
                    Err(String::from("Access of a non struct type"))
                }
            }
            Expr::Namespace { loc, .. } => Ok(Expression::Nop { loc }),
        }
    }

    fn reduce_var(&self, var: NameVariable, s: &State) -> Result<Variable, String> {
        let name = s.names.get(var.n_id);
        let t = s
            .type_vars
            .get(&name.t_id)
            .ok_or(format!("Invalid t_id '{}'", name.t_id))?;
        let t = t.lower()?;
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
        let fun_t_id = *s
            .fun_types
            .get(&proto.fun_id)
            .ok_or(format!("No t_id for fun_id '{}'", proto.fun_id))?;
        let fun_t = s
            .types
            .get(fun_t_id)
            .ok_or(format!("No type ofr t_id '{}'", fun_t_id))?;
        let t = match fun_t {
            ASTTypes::Fun(t) => t.lower()?,
            _ => return Err(String::from("Function does not have function type")),
        };

        Ok(FunctionPrototype {
            ident: proto.ident,
            alias: proto.alias,
            is_pub: proto.is_pub,
            loc: proto.loc,
            fun_id: proto.fun_id,
            t,
        })
    }

    fn reduce_struct(&mut self, struc: NameStruct) -> Result<Struct, String> {
        let mut fields = HashMap::with_capacity(struc.fields.len());
        for (f_name, field) in struc.fields {
            fields.insert(
                f_name,
                StructField {
                    t: field.t.lower()?,
                    is_pub: field.is_pub,
                    loc: field.loc,
                },
            );
        }
        Ok(Struct {
            fields,
            ident: struc.ident,
            s_id: struc.s_id,
            is_pub: struc.is_pub,
            loc: struc.loc,
        })
    }

    /// Try to convert an expression into a place, that is something that can hold a value (a
    /// memory slot or a variable for instance).
    fn as_place(&mut self, expr: Expression) -> Result<PlaceExpression, String> {
        match expr {
            Expression::Variable(var) => Ok(PlaceExpression::Variable(var)),
            Expression::Access {
                expr,
                field,
                struct_id,
                t,
                loc,
            } => Ok(PlaceExpression::Access {
                expr,
                field,
                struct_id,
                t,
                loc,
            }),
            _ => Err(String::from("Expected a place expression")),
        }
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

    fn t_as_non_null_scalar(&mut self, t: &ScalarType, loc: Location) -> NonNullScalarType {
        match t {
            ScalarType::I32 => NonNullScalarType::I32,
            ScalarType::I64 => NonNullScalarType::I64,
            ScalarType::F32 => NonNullScalarType::F32,
            ScalarType::F64 => NonNullScalarType::F64,
            ScalarType::Bool => NonNullScalarType::Bool,
            _ => {
                self.err
                    .report_internal(loc, format!("Expected a non null scalar, got {}.", t));
                NonNullScalarType::I32
            }
        }
    }
}
