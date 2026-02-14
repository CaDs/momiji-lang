use std::collections::HashMap;
use std::path::Path;

use cranelift::prelude::*;
use cranelift_module::{DataDescription, FuncId, Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};

use crate::errors::CodegenError;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::sem::ir::{SemBlock, SemExpr, SemExprKind, SemProgram, SemStmt};
use crate::types::Type;

#[derive(Debug, Clone)]
struct FunctionSig {
    params: Vec<types::Type>,
    ret: Option<types::Type>,
}

#[derive(Debug, Clone, Copy)]
struct LocalVar {
    var: Variable,
    ty: types::Type,
}

/// Cranelift code generator for Momiji semantic IR
pub struct Codegen {
    function_sigs: HashMap<String, FunctionSig>,
}

/// Context for compiling a single function
struct FunctionCompiler<'a> {
    builder: FunctionBuilder<'a>,
    module: &'a mut ObjectModule,
    variable_scopes: Vec<HashMap<String, LocalVar>>,
    var_counter: usize,
    puts_id: FuncId,
    print_int_id: FuncId,
    malloc_id: FuncId,
    make_range_id: FuncId,
    string_counter: usize,
    function_ids: &'a HashMap<String, FuncId>,
    function_sigs: &'a HashMap<String, FunctionSig>,
    return_type: Option<types::Type>,
}

impl<'a> FunctionCompiler<'a> {
    fn new_variable(&mut self) -> Variable {
        let var = Variable::new(self.var_counter);
        self.var_counter += 1;
        var
    }

    fn push_scope(&mut self) {
        self.variable_scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        let _ = self.variable_scopes.pop();
    }

    fn define_local(&mut self, name: &str, ty: types::Type, value: Value) {
        if let Some(scope) = self.variable_scopes.last_mut() {
            if let Some(existing) = scope.get(name).copied() {
                self.builder.def_var(existing.var, value);
                return;
            }
        }

        let var = self.new_variable();
        self.builder.declare_var(var, ty);
        self.builder.def_var(var, value);

        if let Some(scope) = self.variable_scopes.last_mut() {
            scope.insert(name.to_string(), LocalVar { var, ty });
        }
    }

    fn lookup_local(&self, name: &str) -> Option<LocalVar> {
        for scope in self.variable_scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(*var);
            }
        }
        None
    }

    fn sem_type_to_cranelift(ty: &Type) -> Option<types::Type> {
        match ty {
            Type::Int => Some(types::I64),
            Type::Float => Some(types::F64),
            Type::Bool => Some(types::I8),
            Type::String => Some(types::I64),
            Type::Nil => Some(types::I64),
            Type::Unit => None,
            Type::Array(_) => Some(types::I64),
            Type::Nullable(_) => Some(types::I64),
            Type::Unknown | Type::Error => None,
        }
    }

    fn error<T>(&self, message: impl Into<String>) -> Result<T, CodegenError> {
        Err(CodegenError::LlvmError {
            message: message.into(),
        })
    }

    fn compile_block(&mut self, block: &SemBlock) -> Result<bool, CodegenError> {
        let mut terminated = false;
        for stmt in &block.stmts {
            if terminated {
                break;
            }
            terminated = self.compile_stmt(stmt)?;
        }
        Ok(terminated)
    }

    fn compile_block_scoped(&mut self, block: &SemBlock) -> Result<bool, CodegenError> {
        self.push_scope();
        let result = self.compile_block(block);
        self.pop_scope();
        result
    }

    fn compile_stmt(&mut self, stmt: &SemStmt) -> Result<bool, CodegenError> {
        match stmt {
            SemStmt::Let {
                name, ty, value, ..
            } => {
                let local_ty =
                    Self::sem_type_to_cranelift(ty).ok_or_else(|| CodegenError::LlvmError {
                        message: format!("unsupported local variable type `{}` for `{}`", ty, name),
                    })?;
                let value = self.compile_expr(value)?;
                let value = self.cast_value(value, local_ty)?;
                self.define_local(name, local_ty, value);
                Ok(false)
            }
            SemStmt::Assign { name, value, .. } => {
                let local = self
                    .lookup_local(name)
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: format!("assignment to unknown variable `{}`", name),
                    })?;
                let value = self.compile_expr(value)?;
                let value = self.cast_value(value, local.ty)?;
                self.builder.def_var(local.var, value);
                Ok(false)
            }
            SemStmt::Expr(expr) => {
                let _ = self.compile_expr(expr)?;
                Ok(false)
            }
            SemStmt::Return { value, .. } => {
                match self.return_type {
                    Some(ret_ty) => {
                        let expr = value.as_ref().ok_or_else(|| CodegenError::LlvmError {
                            message: "missing return value in non-Unit function".to_string(),
                        })?;
                        let value = self.compile_expr(expr)?;
                        let value = self.cast_value(value, ret_ty)?;
                        self.builder.ins().return_(&[value]);
                    }
                    None => {
                        if value.is_some() {
                            return self.error("unexpected return value in Unit function");
                        }
                        self.builder.ins().return_(&[]);
                    }
                }
                Ok(true)
            }
            SemStmt::If {
                condition,
                then_block,
                else_block,
                ..
            } => self.compile_if(condition, then_block, else_block.as_ref()),
            SemStmt::While {
                condition, body, ..
            } => self.compile_while(condition, body),
            SemStmt::For { .. } => {
                self.error("`for` loops are not yet supported by the Cranelift backend")
            }
        }
    }

    fn compile_if(
        &mut self,
        condition: &SemExpr,
        then_block: &SemBlock,
        else_block: Option<&SemBlock>,
    ) -> Result<bool, CodegenError> {
        let cond_val = self.compile_expr(condition)?;
        let cond_val = self.cast_value(cond_val, types::I8)?;

        let then_bb = self.builder.create_block();
        let else_bb = self.builder.create_block();
        let merge_bb = self.builder.create_block();

        self.builder
            .ins()
            .brif(cond_val, then_bb, &[], else_bb, &[]);

        self.builder.switch_to_block(then_bb);
        self.builder.seal_block(then_bb);
        let then_terminated = self.compile_block_scoped(then_block)?;
        if !then_terminated {
            self.builder.ins().jump(merge_bb, &[]);
        }

        self.builder.switch_to_block(else_bb);
        self.builder.seal_block(else_bb);
        let else_terminated = if let Some(else_blk) = else_block {
            self.compile_block_scoped(else_blk)?
        } else {
            false
        };
        if !else_terminated {
            self.builder.ins().jump(merge_bb, &[]);
        }

        self.builder.switch_to_block(merge_bb);
        self.builder.seal_block(merge_bb);

        Ok(then_terminated && else_terminated)
    }

    fn compile_while(
        &mut self,
        condition: &SemExpr,
        body: &SemBlock,
    ) -> Result<bool, CodegenError> {
        let header_bb = self.builder.create_block();
        let body_bb = self.builder.create_block();
        let exit_bb = self.builder.create_block();

        self.builder.ins().jump(header_bb, &[]);

        self.builder.switch_to_block(header_bb);
        let cond_val = self.compile_expr(condition)?;
        let cond_val = self.cast_value(cond_val, types::I8)?;
        self.builder
            .ins()
            .brif(cond_val, body_bb, &[], exit_bb, &[]);

        self.builder.switch_to_block(body_bb);
        self.builder.seal_block(body_bb);
        let body_terminated = self.compile_block_scoped(body)?;
        if !body_terminated {
            self.builder.ins().jump(header_bb, &[]);
        }

        self.builder.seal_block(header_bb);

        self.builder.switch_to_block(exit_bb);
        self.builder.seal_block(exit_bb);

        Ok(false)
    }

    fn compile_expr(&mut self, expr: &SemExpr) -> Result<Value, CodegenError> {
        match &expr.kind {
            SemExprKind::Int(value) => Ok(self.builder.ins().iconst(types::I64, *value)),
            SemExprKind::Float(value) => Ok(self.builder.ins().f64const(*value)),
            SemExprKind::Bool(value) => Ok(self
                .builder
                .ins()
                .iconst(types::I8, if *value { 1 } else { 0 })),
            SemExprKind::String(value) => self.compile_string_literal(value),
            SemExprKind::Nil => Ok(self.builder.ins().iconst(types::I64, 0)),
            SemExprKind::Variable(name) => {
                let local = self
                    .lookup_local(name)
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: format!("use of unknown variable `{}`", name),
                    })?;
                Ok(self.builder.use_var(local.var))
            }
            SemExprKind::Binary { left, op, right } => {
                self.compile_binary(left, *op, right, &expr.ty)
            }
            SemExprKind::Unary { op, expr: inner } => self.compile_unary(*op, inner),
            SemExprKind::Call { name, args } => self.compile_call(name, args, &expr.ty),
            SemExprKind::Index { object, index } => self.compile_index_expr(object, index, expr),
            SemExprKind::Array(elements) => self.compile_array_literal(elements, &expr.ty),
        }
    }

    fn compile_string_literal(&mut self, value: &str) -> Result<Value, CodegenError> {
        let name = format!(".str.{}", self.string_counter);
        self.string_counter += 1;

        let data_id = self
            .module
            .declare_data(&name, Linkage::Local, false, false)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let mut desc = DataDescription::new();
        let mut bytes = value.as_bytes().to_vec();
        bytes.push(0);
        desc.define(bytes.into_boxed_slice());

        self.module
            .define_data(data_id, &desc)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let gv = self.module.declare_data_in_func(data_id, self.builder.func);
        Ok(self.builder.ins().global_value(types::I64, gv))
    }

    fn compile_array_literal(
        &mut self,
        elements: &[SemExpr],
        array_ty: &Type,
    ) -> Result<Value, CodegenError> {
        let elem_ty = match array_ty {
            Type::Array(inner) => inner.as_ref(),
            other => {
                return self.error(format!(
                    "internal codegen error: array literal has non-array type `{}`",
                    other
                ));
            }
        };

        if matches!(elem_ty, Type::Float) {
            return self
                .error("array literals of Float are not yet supported by the Cranelift backend");
        }

        let len_i64 = i64::try_from(elements.len()).map_err(|_| CodegenError::LlvmError {
            message: "array literal is too large for codegen".to_string(),
        })?;
        let bytes = (len_i64 + 1) * 8;

        let malloc_ref = self
            .module
            .declare_func_in_func(self.malloc_id, self.builder.func);
        let size_val = self.builder.ins().iconst(types::I64, bytes);
        let array_ptr = self.builder.ins().call(malloc_ref, &[size_val]);
        let array_ptr = self.builder.inst_results(array_ptr)[0];

        let flags = MemFlags::new();
        let len_val = self.builder.ins().iconst(types::I64, len_i64);
        self.builder.ins().store(flags, len_val, array_ptr, 0);

        for (idx, element) in elements.iter().enumerate() {
            let element_val = self.compile_expr(element)?;
            let element_val = self.cast_value(element_val, types::I64)?;

            let slot = i64::try_from(idx + 1).map_err(|_| CodegenError::LlvmError {
                message: "array literal index overflow".to_string(),
            })?;
            let offset = slot
                .checked_mul(8)
                .and_then(|v| i32::try_from(v).ok())
                .ok_or_else(|| CodegenError::LlvmError {
                    message: "array literal offset overflow".to_string(),
                })?;

            self.builder
                .ins()
                .store(flags, element_val, array_ptr, offset);
        }

        Ok(array_ptr)
    }

    fn compile_index_expr(
        &mut self,
        object: &SemExpr,
        index: &SemExpr,
        full_expr: &SemExpr,
    ) -> Result<Value, CodegenError> {
        let object_ty = &object.ty;
        let elem_ty = match object_ty {
            Type::Array(inner) => inner.as_ref(),
            Type::String => {
                return self.error("string indexing is not yet supported by the Cranelift backend");
            }
            other => {
                return self.error(format!(
                    "indexing is not supported for type `{}` in Cranelift backend",
                    other
                ));
            }
        };

        if matches!(elem_ty, Type::Float) {
            return self
                .error("indexing Float arrays is not yet supported by the Cranelift backend");
        }

        let array_ptr = self.compile_expr(object)?;
        let array_ptr = self.cast_value(array_ptr, types::I64)?;

        let index_val = self.compile_expr(index)?;
        let index_val = self.cast_value(index_val, types::I64)?;

        let one = self.builder.ins().iconst(types::I64, 1);
        let slot = self.builder.ins().iadd(index_val, one);
        let eight = self.builder.ins().iconst(types::I64, 8);
        let byte_offset = self.builder.ins().imul(slot, eight);
        let elem_ptr = self.builder.ins().iadd(array_ptr, byte_offset);

        let flags = MemFlags::new();
        let raw_value = self.builder.ins().load(types::I64, flags, elem_ptr, 0);

        let target_ty =
            Self::sem_type_to_cranelift(&full_expr.ty).ok_or_else(|| CodegenError::LlvmError {
                message: format!(
                    "unsupported indexed result type `{}` in Cranelift backend",
                    full_expr.ty
                ),
            })?;
        self.cast_value(raw_value, target_ty)
    }

    fn compile_binary(
        &mut self,
        left: &SemExpr,
        op: BinaryOp,
        right: &SemExpr,
        result_ty: &Type,
    ) -> Result<Value, CodegenError> {
        let lhs = self.compile_expr(left)?;
        let rhs = self.compile_expr(right)?;

        let uses_float = matches!(left.ty, Type::Float)
            || matches!(right.ty, Type::Float)
            || matches!(result_ty, Type::Float);

        match op {
            BinaryOp::Add => {
                if uses_float {
                    let lhs = self.cast_value(lhs, types::F64)?;
                    let rhs = self.cast_value(rhs, types::F64)?;
                    Ok(self.builder.ins().fadd(lhs, rhs))
                } else {
                    let lhs = self.cast_value(lhs, types::I64)?;
                    let rhs = self.cast_value(rhs, types::I64)?;
                    Ok(self.builder.ins().iadd(lhs, rhs))
                }
            }
            BinaryOp::Sub => {
                if uses_float {
                    let lhs = self.cast_value(lhs, types::F64)?;
                    let rhs = self.cast_value(rhs, types::F64)?;
                    Ok(self.builder.ins().fsub(lhs, rhs))
                } else {
                    let lhs = self.cast_value(lhs, types::I64)?;
                    let rhs = self.cast_value(rhs, types::I64)?;
                    Ok(self.builder.ins().isub(lhs, rhs))
                }
            }
            BinaryOp::Mul => {
                if uses_float {
                    let lhs = self.cast_value(lhs, types::F64)?;
                    let rhs = self.cast_value(rhs, types::F64)?;
                    Ok(self.builder.ins().fmul(lhs, rhs))
                } else {
                    let lhs = self.cast_value(lhs, types::I64)?;
                    let rhs = self.cast_value(rhs, types::I64)?;
                    Ok(self.builder.ins().imul(lhs, rhs))
                }
            }
            BinaryOp::Div => {
                if uses_float {
                    let lhs = self.cast_value(lhs, types::F64)?;
                    let rhs = self.cast_value(rhs, types::F64)?;
                    Ok(self.builder.ins().fdiv(lhs, rhs))
                } else {
                    let lhs = self.cast_value(lhs, types::I64)?;
                    let rhs = self.cast_value(rhs, types::I64)?;
                    Ok(self.builder.ins().sdiv(lhs, rhs))
                }
            }
            BinaryOp::Mod => {
                if uses_float {
                    self.error("floating-point modulo is not supported by the Cranelift backend")
                } else {
                    let lhs = self.cast_value(lhs, types::I64)?;
                    let rhs = self.cast_value(rhs, types::I64)?;
                    Ok(self.builder.ins().srem(lhs, rhs))
                }
            }
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Le
            | BinaryOp::Gt
            | BinaryOp::Ge => {
                let cmp = if uses_float {
                    let lhs = self.cast_value(lhs, types::F64)?;
                    let rhs = self.cast_value(rhs, types::F64)?;
                    let cc = match op {
                        BinaryOp::Eq => FloatCC::Equal,
                        BinaryOp::Ne => FloatCC::NotEqual,
                        BinaryOp::Lt => FloatCC::LessThan,
                        BinaryOp::Le => FloatCC::LessThanOrEqual,
                        BinaryOp::Gt => FloatCC::GreaterThan,
                        BinaryOp::Ge => FloatCC::GreaterThanOrEqual,
                        _ => unreachable!(),
                    };
                    self.builder.ins().fcmp(cc, lhs, rhs)
                } else {
                    let width = if matches!(left.ty, Type::Bool) && matches!(right.ty, Type::Bool) {
                        types::I8
                    } else {
                        types::I64
                    };
                    let lhs = self.cast_value(lhs, width)?;
                    let rhs = self.cast_value(rhs, width)?;
                    let cc = match op {
                        BinaryOp::Eq => IntCC::Equal,
                        BinaryOp::Ne => IntCC::NotEqual,
                        BinaryOp::Lt => IntCC::SignedLessThan,
                        BinaryOp::Le => IntCC::SignedLessThanOrEqual,
                        BinaryOp::Gt => IntCC::SignedGreaterThan,
                        BinaryOp::Ge => IntCC::SignedGreaterThanOrEqual,
                        _ => unreachable!(),
                    };
                    self.builder.ins().icmp(cc, lhs, rhs)
                };

                let one = self.builder.ins().iconst(types::I8, 1);
                let zero = self.builder.ins().iconst(types::I8, 0);
                Ok(self.builder.ins().select(cmp, one, zero))
            }
            BinaryOp::And => {
                let lhs = self.cast_value(lhs, types::I8)?;
                let rhs = self.cast_value(rhs, types::I8)?;
                Ok(self.builder.ins().band(lhs, rhs))
            }
            BinaryOp::Or => {
                let lhs = self.cast_value(lhs, types::I8)?;
                let rhs = self.cast_value(rhs, types::I8)?;
                Ok(self.builder.ins().bor(lhs, rhs))
            }
            BinaryOp::BitAnd => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                Ok(self.builder.ins().band(lhs, rhs))
            }
            BinaryOp::BitOr => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                Ok(self.builder.ins().bor(lhs, rhs))
            }
            BinaryOp::BitXor => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                Ok(self.builder.ins().bxor(lhs, rhs))
            }
            BinaryOp::Shl => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                Ok(self.builder.ins().ishl(lhs, rhs))
            }
            BinaryOp::Shr => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                Ok(self.builder.ins().sshr(lhs, rhs))
            }
            BinaryOp::Range | BinaryOp::RangeIncl => {
                let lhs = self.cast_value(lhs, types::I64)?;
                let rhs = self.cast_value(rhs, types::I64)?;
                let inclusive = self.builder.ins().iconst(
                    types::I8,
                    if matches!(op, BinaryOp::RangeIncl) {
                        1
                    } else {
                        0
                    },
                );

                let make_range_ref = self
                    .module
                    .declare_func_in_func(self.make_range_id, self.builder.func);
                let call = self
                    .builder
                    .ins()
                    .call(make_range_ref, &[lhs, rhs, inclusive]);
                Ok(self.builder.inst_results(call)[0])
            }
        }
    }

    fn compile_unary(&mut self, op: UnaryOp, expr: &SemExpr) -> Result<Value, CodegenError> {
        let value = self.compile_expr(expr)?;
        match op {
            UnaryOp::Neg => {
                if matches!(expr.ty, Type::Float) {
                    let value = self.cast_value(value, types::F64)?;
                    Ok(self.builder.ins().fneg(value))
                } else {
                    let value = self.cast_value(value, types::I64)?;
                    Ok(self.builder.ins().ineg(value))
                }
            }
            UnaryOp::Not => {
                let value = self.cast_value(value, types::I8)?;
                let zero = self.builder.ins().iconst(types::I8, 0);
                let cmp = self.builder.ins().icmp(IntCC::Equal, value, zero);
                let one = self.builder.ins().iconst(types::I8, 1);
                Ok(self.builder.ins().select(cmp, one, zero))
            }
            UnaryOp::BitNot => {
                let value = self.cast_value(value, types::I64)?;
                Ok(self.builder.ins().bnot(value))
            }
        }
    }

    fn compile_call(
        &mut self,
        name: &str,
        args: &[SemExpr],
        expr_ty: &Type,
    ) -> Result<Value, CodegenError> {
        if name == "puts" {
            return self.compile_puts(args);
        }

        if name == "print" {
            return self.compile_print(args);
        }

        let func_id = *self
            .function_ids
            .get(name)
            .ok_or_else(|| CodegenError::LlvmError {
                message: format!("unknown function `{}` in codegen", name),
            })?;

        let sig = self
            .function_sigs
            .get(name)
            .ok_or_else(|| CodegenError::LlvmError {
                message: format!("missing signature for function `{}`", name),
            })?;

        if args.len() != sig.params.len() {
            return self.error(format!(
                "wrong arity in call to `{}`: expected {}, found {}",
                name,
                sig.params.len(),
                args.len()
            ));
        }

        let mut compiled_args = Vec::with_capacity(args.len());
        for (arg, param_ty) in args.iter().zip(sig.params.iter()) {
            let value = self.compile_expr(arg)?;
            compiled_args.push(self.cast_value(value, *param_ty)?);
        }

        let callee_ref = self.module.declare_func_in_func(func_id, self.builder.func);
        let call = self.builder.ins().call(callee_ref, &compiled_args);

        if let Some(ret_ty) = sig.ret {
            let ret = self.builder.inst_results(call)[0];
            if let Some(expected_ty) = Self::sem_type_to_cranelift(expr_ty) {
                self.cast_value(ret, expected_ty)
            } else {
                self.cast_value(ret, ret_ty)
            }
        } else {
            Ok(self.builder.ins().iconst(types::I64, 0))
        }
    }

    fn compile_puts(&mut self, args: &[SemExpr]) -> Result<Value, CodegenError> {
        if args.len() != 1 {
            return self.error(format!(
                "`puts` expects 1 argument in codegen, found {}",
                args.len()
            ));
        }

        let arg = &args[0];
        let value = self.compile_expr(arg)?;

        let ret = if matches!(arg.ty, Type::String) {
            let value = self.cast_value(value, types::I64)?;
            let puts_ref = self
                .module
                .declare_func_in_func(self.puts_id, self.builder.func);
            let call = self.builder.ins().call(puts_ref, &[value]);
            self.builder.inst_results(call)[0]
        } else {
            let value = self.cast_value(value, types::I64)?;
            let print_int_ref = self
                .module
                .declare_func_in_func(self.print_int_id, self.builder.func);
            let call = self.builder.ins().call(print_int_ref, &[value]);
            self.builder.inst_results(call)[0]
        };

        self.cast_value(ret, types::I64)
    }

    fn compile_print(&mut self, args: &[SemExpr]) -> Result<Value, CodegenError> {
        // Temporary behavior parity with previous backend implementation.
        self.compile_puts(args)
    }

    fn cast_value(&mut self, value: Value, target: types::Type) -> Result<Value, CodegenError> {
        let current = self.builder.func.dfg.value_type(value);
        if current == target {
            return Ok(value);
        }

        match (current, target) {
            (types::I8, types::I32) => Ok(self.builder.ins().uextend(types::I32, value)),
            (types::I8, types::I64) => Ok(self.builder.ins().uextend(types::I64, value)),
            (types::I32, types::I64) => Ok(self.builder.ins().uextend(types::I64, value)),

            (types::I64, types::I32) => Ok(self.builder.ins().ireduce(types::I32, value)),
            (types::I64, types::I8) => Ok(self.builder.ins().ireduce(types::I8, value)),
            (types::I32, types::I8) => Ok(self.builder.ins().ireduce(types::I8, value)),

            (types::I64, types::F64) => Ok(self.builder.ins().fcvt_from_sint(types::F64, value)),
            (types::I32, types::F64) => Ok(self.builder.ins().fcvt_from_sint(types::F64, value)),
            (types::I8, types::F64) => {
                let widened = self.builder.ins().uextend(types::I64, value);
                Ok(self.builder.ins().fcvt_from_sint(types::F64, widened))
            }

            (types::F64, types::I64) => Ok(self.builder.ins().fcvt_to_sint(types::I64, value)),
            (types::F64, types::I32) => Ok(self.builder.ins().fcvt_to_sint(types::I32, value)),
            (types::F64, types::I8) => {
                let as_i64 = self.builder.ins().fcvt_to_sint(types::I64, value);
                Ok(self.builder.ins().ireduce(types::I8, as_i64))
            }

            _ => self.error(format!(
                "unsupported cast from Cranelift type `{}` to `{}`",
                current, target
            )),
        }
    }

    fn default_return_value(&mut self, ty: types::Type) -> Value {
        if ty == types::F64 {
            self.builder.ins().f64const(0.0)
        } else {
            self.builder.ins().iconst(ty, 0)
        }
    }
}

impl Codegen {
    pub fn new(_module_name: &str) -> Result<Self, CodegenError> {
        Ok(Self {
            function_sigs: HashMap::new(),
        })
    }

    /// Collect semantic function signatures for code generation.
    pub fn compile(&mut self, program: &SemProgram) -> Result<(), CodegenError> {
        self.function_sigs.clear();

        for function in &program.functions {
            let mut params = Vec::with_capacity(function.params.len());
            for param in &function.params {
                let ty =
                    Self::type_to_cranelift(&param.ty).ok_or_else(|| CodegenError::LlvmError {
                        message: format!(
                            "unsupported parameter type `{}` for `{}`",
                            param.ty, function.name
                        ),
                    })?;
                params.push(ty);
            }

            let ret = Self::type_to_cranelift(&function.return_type);
            self.function_sigs
                .insert(function.name.clone(), FunctionSig { params, ret });
        }

        Ok(())
    }

    fn type_to_cranelift(ty: &Type) -> Option<types::Type> {
        match ty {
            Type::Int => Some(types::I64),
            Type::Float => Some(types::F64),
            Type::Bool => Some(types::I8),
            Type::String => Some(types::I64),
            Type::Nil => Some(types::I64),
            Type::Unit => None,
            Type::Array(_) => Some(types::I64),
            Type::Nullable(_) => Some(types::I64),
            Type::Unknown | Type::Error => None,
        }
    }

    /// Write object file from semantic IR.
    pub fn write_object(self, path: &Path, program: &SemProgram) -> Result<(), CodegenError> {
        let isa_builder = cranelift_native::builder().map_err(|e| CodegenError::LlvmError {
            message: e.to_string(),
        })?;

        let mut flag_builder = settings::builder();
        flag_builder.set("is_pic", "true").unwrap();
        flag_builder.set("opt_level", "speed").unwrap();

        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let builder = ObjectBuilder::new(isa, "momiji", cranelift_module::default_libcall_names())
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let mut module = ObjectModule::new(builder);
        let mut ctx = codegen::Context::new();
        let mut builder_ctx = FunctionBuilderContext::new();

        // External runtime functions.
        let mut puts_sig = module.make_signature();
        puts_sig.params.push(AbiParam::new(types::I64));
        puts_sig.returns.push(AbiParam::new(types::I32));
        let puts_id = module
            .declare_function("puts", Linkage::Import, &puts_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let mut putchar_sig = module.make_signature();
        putchar_sig.params.push(AbiParam::new(types::I32));
        putchar_sig.returns.push(AbiParam::new(types::I32));
        let putchar_id = module
            .declare_function("putchar", Linkage::Import, &putchar_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let mut malloc_sig = module.make_signature();
        malloc_sig.params.push(AbiParam::new(types::I64));
        malloc_sig.returns.push(AbiParam::new(types::I64));
        let malloc_id = module
            .declare_function("malloc", Linkage::Import, &malloc_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        // Internal helper.
        let mut print_int_sig = module.make_signature();
        print_int_sig.params.push(AbiParam::new(types::I64));
        print_int_sig.returns.push(AbiParam::new(types::I32));
        let print_int_id = module
            .declare_function("_momiji_print_int", Linkage::Local, &print_int_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let mut make_range_sig = module.make_signature();
        make_range_sig.params.push(AbiParam::new(types::I64)); // start
        make_range_sig.params.push(AbiParam::new(types::I64)); // end
        make_range_sig.params.push(AbiParam::new(types::I8)); // inclusive
        make_range_sig.returns.push(AbiParam::new(types::I64)); // array pointer
        let make_range_id = module
            .declare_function("_momiji_make_range", Linkage::Local, &make_range_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        Self::generate_print_int(
            &mut module,
            &mut ctx,
            &mut builder_ctx,
            print_int_id,
            putchar_id,
        )?;
        module.clear_context(&mut ctx);

        Self::generate_make_range(
            &mut module,
            &mut ctx,
            &mut builder_ctx,
            make_range_id,
            malloc_id,
        )?;
        module.clear_context(&mut ctx);

        let main_fn = program
            .functions
            .iter()
            .find(|f| f.name == "main")
            .ok_or_else(|| CodegenError::LlvmError {
                message: "no `main` function found".to_string(),
            })?;
        if !main_fn.params.is_empty() {
            return Err(CodegenError::LlvmError {
                message: "`main` cannot take parameters in Cranelift backend".to_string(),
            });
        }

        // Declare all user functions first so calls can reference each other.
        let mut function_ids = HashMap::new();
        for function in &program.functions {
            let sig =
                self.function_sigs
                    .get(&function.name)
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: format!("missing function signature for `{}`", function.name),
                    })?;

            let mut clif_sig = module.make_signature();
            for param in &sig.params {
                clif_sig.params.push(AbiParam::new(*param));
            }
            if let Some(ret) = sig.ret {
                clif_sig.returns.push(AbiParam::new(ret));
            }

            let symbol_name = if function.name == "main" {
                "_momiji_main".to_string()
            } else {
                function.name.clone()
            };

            let func_id = module
                .declare_function(&symbol_name, Linkage::Local, &clif_sig)
                .map_err(|e| CodegenError::LlvmError {
                    message: e.to_string(),
                })?;

            function_ids.insert(function.name.clone(), func_id);
        }

        // Define each user function.
        for function in &program.functions {
            let func_id =
                *function_ids
                    .get(&function.name)
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: format!("missing function id for `{}`", function.name),
                    })?;
            let sig =
                self.function_sigs
                    .get(&function.name)
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: format!("missing signature for `{}`", function.name),
                    })?;

            let mut clif_sig = module.make_signature();
            for param in &sig.params {
                clif_sig.params.push(AbiParam::new(*param));
            }
            if let Some(ret) = sig.ret {
                clif_sig.returns.push(AbiParam::new(ret));
            }
            ctx.func.signature = clif_sig;

            {
                let builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);

                let mut compiler = FunctionCompiler {
                    builder,
                    module: &mut module,
                    variable_scopes: vec![HashMap::new()],
                    var_counter: 0,
                    puts_id,
                    print_int_id,
                    malloc_id,
                    make_range_id,
                    string_counter: 0,
                    function_ids: &function_ids,
                    function_sigs: &self.function_sigs,
                    return_type: sig.ret,
                };

                let entry = compiler.builder.create_block();
                compiler
                    .builder
                    .append_block_params_for_function_params(entry);
                compiler.builder.switch_to_block(entry);
                compiler.builder.seal_block(entry);

                let entry_params: Vec<Value> = compiler.builder.block_params(entry).to_vec();
                for ((param, param_ty), param_val) in function
                    .params
                    .iter()
                    .zip(sig.params.iter())
                    .zip(entry_params.into_iter())
                {
                    compiler.define_local(&param.name, *param_ty, param_val);
                }

                let terminated = compiler.compile_block(&function.body)?;
                if !terminated {
                    if let Some(ret_ty) = sig.ret {
                        let zero = compiler.default_return_value(ret_ty);
                        compiler.builder.ins().return_(&[zero]);
                    } else {
                        compiler.builder.ins().return_(&[]);
                    }
                }

                compiler.builder.finalize();
            }

            module
                .define_function(func_id, &mut ctx)
                .map_err(|e| CodegenError::LlvmError {
                    message: e.to_string(),
                })?;

            module.clear_context(&mut ctx);
        }

        // Export C-compatible `main` wrapper.
        let mut c_main_sig = module.make_signature();
        c_main_sig.returns.push(AbiParam::new(types::I32));
        let c_main_id = module
            .declare_function("main", Linkage::Export, &c_main_sig)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        ctx.func.signature = c_main_sig;
        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut builder_ctx);
            let entry = builder.create_block();
            builder.switch_to_block(entry);
            builder.seal_block(entry);

            let main_id = *function_ids
                .get("main")
                .ok_or_else(|| CodegenError::LlvmError {
                    message: "missing compiled `main` function".to_string(),
                })?;
            let main_sig =
                self.function_sigs
                    .get("main")
                    .ok_or_else(|| CodegenError::LlvmError {
                        message: "missing signature for `main`".to_string(),
                    })?;

            let main_ref = module.declare_func_in_func(main_id, builder.func);
            let call = builder.ins().call(main_ref, &[]);

            let exit_code = if main_sig.ret.is_some() {
                let ret = builder.inst_results(call)[0];
                let ret_ty = builder.func.dfg.value_type(ret);
                if ret_ty == types::I32 {
                    ret
                } else if ret_ty == types::I8 {
                    builder.ins().uextend(types::I32, ret)
                } else if ret_ty == types::I64 {
                    builder.ins().ireduce(types::I32, ret)
                } else if ret_ty == types::F64 {
                    builder.ins().fcvt_to_sint(types::I32, ret)
                } else {
                    builder.ins().iconst(types::I32, 0)
                }
            } else {
                builder.ins().iconst(types::I32, 0)
            };

            builder.ins().return_(&[exit_code]);
            builder.finalize();
        }

        module
            .define_function(c_main_id, &mut ctx)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        let product = module.finish();
        let bytes = product.emit().map_err(|e| CodegenError::LlvmError {
            message: e.to_string(),
        })?;

        std::fs::write(path, bytes).map_err(|e| CodegenError::LlvmError {
            message: e.to_string(),
        })?;

        Ok(())
    }

    /// Generate the range helper function.
    /// This function allocates and materializes an Int array for `start..end` / `start...end`.
    fn generate_make_range(
        module: &mut ObjectModule,
        ctx: &mut codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
        make_range_id: FuncId,
        malloc_id: FuncId,
    ) -> Result<(), CodegenError> {
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64)); // start
        sig.params.push(AbiParam::new(types::I64)); // end
        sig.params.push(AbiParam::new(types::I8)); // inclusive
        sig.returns.push(AbiParam::new(types::I64)); // array pointer
        ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);

        let start_var = Variable::new(0);
        let end_var = Variable::new(1);
        let inclusive_var = Variable::new(2);
        let len_var = Variable::new(3);
        let ptr_var = Variable::new(4);
        builder.declare_var(start_var, types::I64);
        builder.declare_var(end_var, types::I64);
        builder.declare_var(inclusive_var, types::I8);
        builder.declare_var(len_var, types::I64);
        builder.declare_var(ptr_var, types::I64);

        let entry = builder.create_block();
        let loop_header = builder.create_block();
        let loop_body = builder.create_block();
        let done = builder.create_block();

        builder.append_block_params_for_function_params(entry);

        builder.switch_to_block(entry);
        builder.seal_block(entry);

        let start = builder.block_params(entry)[0];
        let end = builder.block_params(entry)[1];
        let inclusive = builder.block_params(entry)[2];
        builder.def_var(start_var, start);
        builder.def_var(end_var, end);
        builder.def_var(inclusive_var, inclusive);

        let zero_i64 = builder.ins().iconst(types::I64, 0);
        let one_i64 = builder.ins().iconst(types::I64, 1);
        let eight_i64 = builder.ins().iconst(types::I64, 8);

        let start_val = builder.use_var(start_var);
        let end_val = builder.use_var(end_var);
        let inclusive_val = builder.use_var(inclusive_var);

        let reversed = builder
            .ins()
            .icmp(IntCC::SignedGreaterThan, start_val, end_val);
        let diff = builder.ins().isub(end_val, start_val);
        let inclusive_i64 = builder.ins().uextend(types::I64, inclusive_val);
        let len_non_empty = builder.ins().iadd(diff, inclusive_i64);
        let len = builder.ins().select(reversed, zero_i64, len_non_empty);
        builder.def_var(len_var, len);

        let len_plus_header = builder.ins().iadd(len, one_i64);
        let bytes = builder.ins().imul(len_plus_header, eight_i64);

        let malloc_ref = module.declare_func_in_func(malloc_id, builder.func);
        let alloc_call = builder.ins().call(malloc_ref, &[bytes]);
        let array_ptr = builder.inst_results(alloc_call)[0];
        builder.def_var(ptr_var, array_ptr);

        let flags = MemFlags::new();
        builder.ins().store(flags, len, array_ptr, 0);
        builder.ins().jump(loop_header, &[zero_i64]);

        builder.switch_to_block(loop_header);
        builder.append_block_param(loop_header, types::I64); // i
        let i = builder.block_params(loop_header)[0];
        let len_now = builder.use_var(len_var);
        let keep_looping = builder.ins().icmp(IntCC::UnsignedLessThan, i, len_now);
        builder.ins().brif(keep_looping, loop_body, &[i], done, &[]);

        builder.switch_to_block(loop_body);
        builder.append_block_param(loop_body, types::I64); // i
        let loop_i = builder.block_params(loop_body)[0];

        let start_now = builder.use_var(start_var);
        let ptr_now = builder.use_var(ptr_var);
        let value = builder.ins().iadd(start_now, loop_i);
        let slot = builder.ins().iadd(loop_i, one_i64);
        let byte_offset = builder.ins().imul(slot, eight_i64);
        let elem_ptr = builder.ins().iadd(ptr_now, byte_offset);
        builder.ins().store(flags, value, elem_ptr, 0);

        let next_i = builder.ins().iadd(loop_i, one_i64);
        builder.ins().jump(loop_header, &[next_i]);
        builder.seal_block(loop_body);
        builder.seal_block(loop_header);

        builder.switch_to_block(done);
        builder.seal_block(done);
        let ptr = builder.use_var(ptr_var);
        builder.ins().return_(&[ptr]);

        builder.finalize();

        module
            .define_function(make_range_id, ctx)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        Ok(())
    }

    /// Generate the print_int helper function
    /// This function prints an i64 integer followed by a newline
    fn generate_print_int(
        module: &mut ObjectModule,
        ctx: &mut codegen::Context,
        builder_ctx: &mut FunctionBuilderContext,
        print_int_id: FuncId,
        putchar_id: FuncId,
    ) -> Result<(), CodegenError> {
        // Get the signature
        let mut sig = module.make_signature();
        sig.params.push(AbiParam::new(types::I64));
        sig.returns.push(AbiParam::new(types::I32));
        ctx.func.signature = sig;

        let mut builder = FunctionBuilder::new(&mut ctx.func, builder_ctx);

        // Blocks
        let entry = builder.create_block();
        let check_negative = builder.create_block();
        let handle_negative = builder.create_block();
        let check_zero = builder.create_block();
        let handle_zero = builder.create_block();
        let find_divisor = builder.create_block();
        let print_digits = builder.create_block();
        let print_newline = builder.create_block();

        builder.append_block_params_for_function_params(entry);

        // Entry: get the number
        builder.switch_to_block(entry);
        builder.seal_block(entry);
        let num = builder.block_params(entry)[0];
        builder.ins().jump(check_negative, &[num]);

        // Check if negative
        builder.switch_to_block(check_negative);
        builder.append_block_param(check_negative, types::I64);
        let n = builder.block_params(check_negative)[0];
        let zero_i64 = builder.ins().iconst(types::I64, 0);
        let is_neg = builder.ins().icmp(IntCC::SignedLessThan, n, zero_i64);
        builder
            .ins()
            .brif(is_neg, handle_negative, &[n], check_zero, &[n]);
        builder.seal_block(check_negative);

        // Handle negative: print '-' and negate
        builder.switch_to_block(handle_negative);
        builder.append_block_param(handle_negative, types::I64);
        let neg_n = builder.block_params(handle_negative)[0];
        let minus = builder.ins().iconst(types::I32, '-' as i64);
        let putchar_ref = module.declare_func_in_func(putchar_id, builder.func);
        builder.ins().call(putchar_ref, &[minus]);
        let pos_n = builder.ins().ineg(neg_n);
        builder.ins().jump(check_zero, &[pos_n]);
        builder.seal_block(handle_negative);

        // Check if zero
        builder.switch_to_block(check_zero);
        builder.append_block_param(check_zero, types::I64);
        let n2 = builder.block_params(check_zero)[0];
        let is_zero = builder.ins().icmp(IntCC::Equal, n2, zero_i64);
        let one = builder.ins().iconst(types::I64, 1);
        builder
            .ins()
            .brif(is_zero, handle_zero, &[], find_divisor, &[n2, one]);
        builder.seal_block(check_zero);

        // Handle zero: print '0' and exit
        builder.switch_to_block(handle_zero);
        let zero_char = builder.ins().iconst(types::I32, '0' as i64);
        let putchar_ref2 = module.declare_func_in_func(putchar_id, builder.func);
        builder.ins().call(putchar_ref2, &[zero_char]);
        builder.ins().jump(print_newline, &[]);
        builder.seal_block(handle_zero);

        // Find divisor: find the largest power of 10 <= n
        builder.switch_to_block(find_divisor);
        builder.append_block_param(find_divisor, types::I64); // n
        builder.append_block_param(find_divisor, types::I64); // divisor
        let n3 = builder.block_params(find_divisor)[0];
        let div = builder.block_params(find_divisor)[1];

        let ten = builder.ins().iconst(types::I64, 10);
        let next_div = builder.ins().imul(div, ten);
        let div_too_big = builder.ins().icmp(IntCC::UnsignedGreaterThan, next_div, n3);
        builder.ins().brif(
            div_too_big,
            print_digits,
            &[n3, div],
            find_divisor,
            &[n3, next_div],
        );
        builder.seal_block(find_divisor);

        // Print digits loop
        builder.switch_to_block(print_digits);
        builder.append_block_param(print_digits, types::I64); // remaining value
        builder.append_block_param(print_digits, types::I64); // current divisor
        let val = builder.block_params(print_digits)[0];
        let divisor = builder.block_params(print_digits)[1];

        // digit = val / divisor
        let digit = builder.ins().udiv(val, divisor);
        // char = '0' + digit
        let digit_i32 = builder.ins().ireduce(types::I32, digit);
        let zero_char2 = builder.ins().iconst(types::I32, '0' as i64);
        let char_val = builder.ins().iadd(zero_char2, digit_i32);
        // putchar(char)
        let putchar_ref3 = module.declare_func_in_func(putchar_id, builder.func);
        builder.ins().call(putchar_ref3, &[char_val]);
        // remainder = val % divisor
        let remainder = builder.ins().urem(val, divisor);
        // next_divisor = divisor / 10
        let next_divisor = builder.ins().udiv(divisor, ten);
        // if next_divisor >= 1, continue; else exit
        let one_check = builder.ins().iconst(types::I64, 1);
        let continue_loop =
            builder
                .ins()
                .icmp(IntCC::UnsignedGreaterThanOrEqual, next_divisor, one_check);
        builder.ins().brif(
            continue_loop,
            print_digits,
            &[remainder, next_divisor],
            print_newline,
            &[],
        );
        builder.seal_block(print_digits);

        // Print newline and return
        builder.switch_to_block(print_newline);
        let newline = builder.ins().iconst(types::I32, '\n' as i64);
        let putchar_ref4 = module.declare_func_in_func(putchar_id, builder.func);
        builder.ins().call(putchar_ref4, &[newline]);
        let ret_zero = builder.ins().iconst(types::I32, 0);
        builder.ins().return_(&[ret_zero]);
        builder.seal_block(print_newline);

        builder.finalize();

        module
            .define_function(print_int_id, ctx)
            .map_err(|e| CodegenError::LlvmError {
                message: e.to_string(),
            })?;

        Ok(())
    }
}
