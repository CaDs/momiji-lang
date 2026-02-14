use std::collections::HashMap;

use crate::errors::TypeError;
use crate::parser::ast::{
    BinaryOp, Block, Expr, Function, Item, Program, Stmt, TypeAnnotation, TypeKind, UnaryOp,
};
use crate::sem::ir::{
    SemBlock, SemExpr, SemExprKind, SemField, SemFunction, SemMatchArm, SemParam, SemPattern,
    SemProgram, SemStmt, SemStruct,
};
use crate::types::types::{FunctionSignature, StructSignature, Symbol, Type};

/// Lowers AST into typed semantic IR and validates semantic rules.
pub struct SemLowerer {
    functions: HashMap<String, FunctionSignature>,
    builtins: HashMap<String, FunctionSignature>,
    structs: HashMap<String, StructSignature>,
    scopes: Vec<HashMap<String, Symbol>>,
    current_return_type: Option<Type>,
    errors: Vec<TypeError>,
}

impl SemLowerer {
    pub fn new() -> Self {
        let mut lowerer = Self {
            functions: HashMap::new(),
            builtins: HashMap::new(),
            structs: HashMap::new(),
            scopes: vec![HashMap::new()],
            current_return_type: None,
            errors: Vec::new(),
        };
        lowerer.register_builtins();
        lowerer
    }

    pub fn lower(mut self, program: &Program) -> Result<SemProgram, Vec<TypeError>> {
        // Pass 1: Register struct signatures (so they're available as types)
        let mut sem_structs = Vec::new();
        for item in &program.items {
            if let Item::Struct(struct_def) = item {
                let mut fields = Vec::new();
                for field in &struct_def.fields {
                    let ty = self.resolve_type(&field.ty.kind);
                    fields.push((field.name.clone(), ty));
                }
                self.structs.insert(
                    struct_def.name.clone(),
                    StructSignature {
                        fields: fields.clone(),
                    },
                );
                sem_structs.push(SemStruct {
                    name: struct_def.name.clone(),
                    fields: fields
                        .into_iter()
                        .map(|(name, ty)| SemField { name, ty })
                        .collect(),
                    span: struct_def.span,
                });
            }
        }

        // Pass 2: Register function signatures (can now reference struct types)
        for item in &program.items {
            if let Item::Function(function) = item {
                let signature = self.function_signature(function);
                self.functions.insert(function.name.clone(), signature);
            }
        }

        // Pass 3: Lower function bodies
        let mut functions = Vec::new();
        for item in &program.items {
            if let Item::Function(function) = item {
                functions.push(self.lower_function(function));
            }
        }

        if self.errors.is_empty() {
            Ok(SemProgram {
                structs: sem_structs,
                functions,
            })
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn register_builtins(&mut self) {
        self.builtins.insert(
            "puts".to_string(),
            FunctionSignature {
                params: vec![("value".to_string(), Type::Unknown)],
                return_type: Type::Unit,
            },
        );
        self.builtins.insert(
            "print".to_string(),
            FunctionSignature {
                params: vec![("value".to_string(), Type::Unknown)],
                return_type: Type::Unit,
            },
        );
    }

    fn function_signature(&self, function: &Function) -> FunctionSignature {
        let params = function
            .params
            .iter()
            .map(|param| (param.name.clone(), self.resolve_type(&param.ty.kind)))
            .collect();
        let return_type = function
            .return_type
            .as_ref()
            .map(|annotation| self.resolve_type(&annotation.kind))
            .unwrap_or(Type::Unit);
        FunctionSignature {
            params,
            return_type,
        }
    }

    fn resolve_type(&self, kind: &TypeKind) -> Type {
        match kind {
            TypeKind::Named(name) => match name.as_str() {
                "Int" => Type::Int,
                "Float" => Type::Float,
                "Bool" => Type::Bool,
                "String" => Type::String,
                "Nil" => Type::Nil,
                "Unit" => Type::Unit,
                _ => {
                    if self.structs.contains_key(name) {
                        Type::Struct(name.clone())
                    } else {
                        Type::Unknown
                    }
                }
            },
            TypeKind::Nullable(inner) => Type::Nullable(Box::new(self.resolve_type(inner))),
            TypeKind::Array(inner) => Type::Array(Box::new(self.resolve_type(inner))),
        }
    }

    fn lower_function(&mut self, function: &Function) -> SemFunction {
        self.push_scope();

        let mut params = Vec::with_capacity(function.params.len());
        for param in &function.params {
            let ty = self.resolve_type(&param.ty.kind);
            self.define(&param.name, ty.clone());
            params.push(SemParam {
                name: param.name.clone(),
                ty,
            });
        }

        let return_type = function
            .return_type
            .as_ref()
            .map(|annotation| self.resolve_type(&annotation.kind))
            .unwrap_or(Type::Unit);
        self.current_return_type = Some(return_type.clone());

        let body = self.lower_block(&function.body);

        if return_type != Type::Unit && !self.block_definitely_returns(&body) {
            self.error(TypeError::MissingReturn {
                name: function.name.clone(),
                span: function.span.into(),
            });
        }

        self.current_return_type = None;
        self.pop_scope();

        SemFunction {
            name: function.name.clone(),
            params,
            return_type,
            body,
            span: function.span,
        }
    }

    fn lower_block(&mut self, block: &Block) -> SemBlock {
        let mut stmts = Vec::with_capacity(block.stmts.len());
        let mut terminated = false;
        for stmt in &block.stmts {
            if terminated {
                self.error(TypeError::UnreachableCode {
                    span: self.ast_stmt_span(stmt).into(),
                });
                continue;
            }

            let lowered = self.lower_stmt(stmt);
            terminated = self.stmt_definitely_returns(&lowered);
            stmts.push(lowered);
        }
        SemBlock { stmts }
    }

    fn lower_stmt(&mut self, stmt: &Stmt) -> SemStmt {
        match stmt {
            Stmt::Expr(expr) => SemStmt::Expr(self.lower_expr(expr)),
            Stmt::Let {
                name,
                ty,
                value,
                implicit,
                span,
            } => self.lower_let_stmt(name, ty.as_ref(), value, *implicit, *span),
            Stmt::Return { value, span } => {
                let lowered = value.as_ref().map(|expr| self.lower_expr(expr));
                let found = lowered
                    .as_ref()
                    .map(|expr| expr.ty.clone())
                    .unwrap_or(Type::Unit);
                let expected = self.current_return_type.clone().unwrap_or(Type::Unit);
                if !found.is_assignable_to(&expected) {
                    self.error(TypeError::TypeMismatch {
                        expected: expected.to_string(),
                        found: found.to_string(),
                        span: (*span).into(),
                    });
                }
                SemStmt::Return {
                    value: lowered,
                    span: *span,
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let condition = self.lower_expr(condition);
                if condition.ty != Type::Bool && condition.ty != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: Type::Bool.to_string(),
                        found: condition.ty.to_string(),
                        span: (*span).into(),
                    });
                }

                let then_block = self.lower_block_scoped(then_block);
                let else_block = else_block
                    .as_ref()
                    .map(|block| self.lower_block_scoped(block));

                SemStmt::If {
                    condition,
                    then_block,
                    else_block,
                    span: *span,
                }
            }
            Stmt::While {
                condition,
                body,
                span,
            } => {
                let condition = self.lower_expr(condition);
                if condition.ty != Type::Bool && condition.ty != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: Type::Bool.to_string(),
                        found: condition.ty.to_string(),
                        span: (*span).into(),
                    });
                }
                let body = self.lower_block_scoped(body);
                SemStmt::While {
                    condition,
                    body,
                    span: *span,
                }
            }
            Stmt::For {
                var,
                iterable,
                body,
                span,
            } => {
                let iterable = self.lower_expr(iterable);
                let elem_type = match &iterable.ty {
                    Type::Array(elem) => (**elem).clone(),
                    Type::Error => Type::Error,
                    other => {
                        self.error(TypeError::TypeMismatch {
                            expected: "Array".to_string(),
                            found: other.to_string(),
                            span: (*span).into(),
                        });
                        Type::Error
                    }
                };

                self.push_scope();
                self.define(var, elem_type.clone());
                let body = self.lower_block(body);
                self.pop_scope();

                SemStmt::For {
                    var: var.clone(),
                    elem_type,
                    iterable,
                    body,
                    span: *span,
                }
            }
            Stmt::Match {
                subject,
                arms,
                span,
            } => {
                let subject = self.lower_expr(subject);
                let subject_ty = subject.ty.clone();

                let mut sem_arms = Vec::with_capacity(arms.len());
                for arm in arms {
                    let (pattern, body) = self.lower_match_arm(arm, &subject_ty);
                    sem_arms.push(SemMatchArm {
                        pattern,
                        body,
                        span: arm.span,
                    });
                }

                SemStmt::Match {
                    subject,
                    arms: sem_arms,
                    span: *span,
                }
            }
        }
    }

    fn lower_block_scoped(&mut self, block: &Block) -> SemBlock {
        self.push_scope();
        let lowered = self.lower_block(block);
        self.pop_scope();
        lowered
    }

    fn ast_stmt_span(&self, stmt: &Stmt) -> crate::errors::Span {
        match stmt {
            Stmt::Expr(expr) => expr.span(),
            Stmt::Let { span, .. } => *span,
            Stmt::Return { span, .. } => *span,
            Stmt::If { span, .. } => *span,
            Stmt::While { span, .. } => *span,
            Stmt::For { span, .. } => *span,
            Stmt::Match { span, .. } => *span,
        }
    }

    fn block_definitely_returns(&self, block: &SemBlock) -> bool {
        for stmt in &block.stmts {
            if self.stmt_definitely_returns(stmt) {
                return true;
            }
        }
        false
    }

    fn stmt_definitely_returns(&self, stmt: &SemStmt) -> bool {
        match stmt {
            SemStmt::Return { .. } => true,
            SemStmt::If {
                then_block,
                else_block,
                ..
            } => {
                if let Some(else_block) = else_block {
                    self.block_definitely_returns(then_block)
                        && self.block_definitely_returns(else_block)
                } else {
                    false
                }
            }
            SemStmt::Match { arms, .. } => {
                let has_catchall = arms.iter().any(|arm| {
                    matches!(
                        arm.pattern,
                        SemPattern::Wildcard | SemPattern::Variable { .. }
                    )
                });
                has_catchall && arms.iter().all(|arm| self.block_definitely_returns(&arm.body))
            }
            SemStmt::Let { .. }
            | SemStmt::Assign { .. }
            | SemStmt::Expr(_)
            | SemStmt::While { .. }
            | SemStmt::For { .. } => false,
        }
    }

    fn lower_let_stmt(
        &mut self,
        name: &str,
        ty: Option<&TypeAnnotation>,
        value: &Expr,
        implicit: bool,
        span: crate::errors::Span,
    ) -> SemStmt {
        let value = self.lower_expr(value);

        if implicit {
            if let Some(existing) = self.lookup(name).cloned() {
                if !value.ty.is_assignable_to(&existing.ty) {
                    self.error(TypeError::TypeMismatch {
                        expected: existing.ty.to_string(),
                        found: value.ty.to_string(),
                        span: span.into(),
                    });
                }

                return SemStmt::Assign {
                    name: name.to_string(),
                    value,
                    span,
                };
            }
        }

        let declared = ty.map(|annotation| self.resolve_type(&annotation.kind));
        let final_type = if let Some(declared) = declared {
            if !value.ty.is_assignable_to(&declared) {
                self.error(TypeError::TypeMismatch {
                    expected: declared.to_string(),
                    found: value.ty.to_string(),
                    span: span.into(),
                });
            }
            declared
        } else {
            value.ty.clone()
        };

        self.define(name, final_type.clone());

        SemStmt::Let {
            name: name.to_string(),
            ty: final_type,
            value,
            span,
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> SemExpr {
        match expr {
            Expr::Int { value, span } => SemExpr {
                kind: SemExprKind::Int(*value),
                ty: Type::Int,
                span: *span,
            },
            Expr::Float { value, span } => SemExpr {
                kind: SemExprKind::Float(*value),
                ty: Type::Float,
                span: *span,
            },
            Expr::String { value, span } => SemExpr {
                kind: SemExprKind::String(value.clone()),
                ty: Type::String,
                span: *span,
            },
            Expr::Bool { value, span } => SemExpr {
                kind: SemExprKind::Bool(*value),
                ty: Type::Bool,
                span: *span,
            },
            Expr::Nil { span } => SemExpr {
                kind: SemExprKind::Nil,
                ty: Type::Nil,
                span: *span,
            },
            Expr::Identifier { name, span } => {
                let ty = self.lookup(name).map(|symbol| symbol.ty.clone());
                if let Some(ty) = ty {
                    SemExpr {
                        kind: SemExprKind::Variable(name.clone()),
                        ty,
                        span: *span,
                    }
                } else {
                    self.error(TypeError::UndefinedVariable {
                        name: name.clone(),
                        span: (*span).into(),
                    });
                    SemExpr {
                        kind: SemExprKind::Variable(name.clone()),
                        ty: Type::Error,
                        span: *span,
                    }
                }
            }
            Expr::Binary {
                left,
                op,
                right,
                span,
            } => {
                let left = self.lower_expr(left);
                let right = self.lower_expr(right);
                let ty = self.binary_type(&left.ty, *op, &right.ty, *span);
                SemExpr {
                    kind: SemExprKind::Binary {
                        left: Box::new(left),
                        op: *op,
                        right: Box::new(right),
                    },
                    ty,
                    span: *span,
                }
            }
            Expr::Unary { op, expr, span } => {
                let expr = self.lower_expr(expr);
                let ty = self.unary_type(&expr.ty, *op, *span);
                SemExpr {
                    kind: SemExprKind::Unary {
                        op: *op,
                        expr: Box::new(expr),
                    },
                    ty,
                    span: *span,
                }
            }
            Expr::Call { callee, args, span } => {
                // Detect StructName.new(...) constructor pattern
                if let Expr::Field {
                    object,
                    field,
                    ..
                } = callee.as_ref()
                {
                    if field == "new" {
                        if let Expr::Identifier { name, .. } = object.as_ref() {
                            if self.structs.contains_key(name) {
                                return self.lower_struct_construct(name, args, *span);
                            }
                        }
                    }
                }

                let name = match callee.as_ref() {
                    Expr::Identifier { name, .. } => name.clone(),
                    _ => {
                        self.error(TypeError::UndefinedFunction {
                            name: "<expression>".to_string(),
                            span: (*span).into(),
                        });
                        "<expression>".to_string()
                    }
                };

                let mut lowered_args = Vec::with_capacity(args.len());
                for arg in args {
                    lowered_args.push(self.lower_expr(arg));
                }

                let signature = self
                    .functions
                    .get(&name)
                    .or_else(|| self.builtins.get(&name))
                    .cloned();
                let return_type = if let Some(signature) = signature {
                    if lowered_args.len() != signature.params.len() {
                        self.error(TypeError::WrongArity {
                            name: name.clone(),
                            expected: signature.params.len(),
                            found: lowered_args.len(),
                            span: (*span).into(),
                        });
                        Type::Error
                    } else {
                        for (arg, (_param_name, param_type)) in
                            lowered_args.iter().zip(signature.params.iter())
                        {
                            if *param_type != Type::Unknown && !arg.ty.is_assignable_to(param_type)
                            {
                                self.error(TypeError::TypeMismatch {
                                    expected: param_type.to_string(),
                                    found: arg.ty.to_string(),
                                    span: arg.span.into(),
                                });
                            }
                        }
                        signature.return_type
                    }
                } else {
                    self.error(TypeError::UndefinedFunction {
                        name: name.clone(),
                        span: (*span).into(),
                    });
                    Type::Error
                };

                SemExpr {
                    kind: SemExprKind::Call {
                        name,
                        args: lowered_args,
                    },
                    ty: return_type,
                    span: *span,
                }
            }
            Expr::Field {
                object,
                field,
                span,
            } => {
                let object = self.lower_expr(object);
                match &object.ty {
                    Type::Struct(struct_name) => {
                        if let Some(sig) = self.structs.get(struct_name) {
                            if let Some((_fname, fty)) =
                                sig.fields.iter().find(|(name, _)| name == field)
                            {
                                let ty = fty.clone();
                                SemExpr {
                                    kind: SemExprKind::FieldAccess {
                                        object: Box::new(object),
                                        field: field.clone(),
                                    },
                                    ty,
                                    span: *span,
                                }
                            } else {
                                self.error(TypeError::UndefinedField {
                                    struct_name: struct_name.clone(),
                                    field: field.clone(),
                                    span: (*span).into(),
                                });
                                SemExpr {
                                    kind: SemExprKind::Variable("<invalid-field>".to_string()),
                                    ty: Type::Error,
                                    span: *span,
                                }
                            }
                        } else {
                            self.error(TypeError::UndefinedStruct {
                                name: struct_name.clone(),
                                span: (*span).into(),
                            });
                            SemExpr {
                                kind: SemExprKind::Variable("<invalid-field>".to_string()),
                                ty: Type::Error,
                                span: *span,
                            }
                        }
                    }
                    Type::Error => SemExpr {
                        kind: SemExprKind::Variable("<invalid-field>".to_string()),
                        ty: Type::Error,
                        span: *span,
                    },
                    _ => {
                        self.error(TypeError::InvalidOperator {
                            op: ".".to_string(),
                            lhs: object.ty.to_string(),
                            rhs: field.clone(),
                            span: (*span).into(),
                        });
                        SemExpr {
                            kind: SemExprKind::Variable("<invalid-field>".to_string()),
                            ty: Type::Error,
                            span: *span,
                        }
                    }
                }
            }
            Expr::Index {
                object,
                index,
                span,
            } => {
                let object = self.lower_expr(object);
                let index = self.lower_expr(index);

                if index.ty != Type::Int && index.ty != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: Type::Int.to_string(),
                        found: index.ty.to_string(),
                        span: index.span.into(),
                    });
                }

                let ty = match object.ty.clone() {
                    Type::Array(inner) => *inner,
                    Type::String => Type::String,
                    Type::Error => Type::Error,
                    other => {
                        self.error(TypeError::InvalidOperator {
                            op: "[]".to_string(),
                            lhs: other.to_string(),
                            rhs: index.ty.to_string(),
                            span: (*span).into(),
                        });
                        Type::Error
                    }
                };

                SemExpr {
                    kind: SemExprKind::Index {
                        object: Box::new(object),
                        index: Box::new(index),
                    },
                    ty,
                    span: *span,
                }
            }
            Expr::Array { elements, span } => {
                let mut lowered = Vec::with_capacity(elements.len());
                for element in elements {
                    lowered.push(self.lower_expr(element));
                }

                let elem_ty = if lowered.is_empty() {
                    Type::Unknown
                } else {
                    let first_ty = lowered[0].ty.clone();
                    for element in lowered.iter().skip(1) {
                        if !element.ty.is_assignable_to(&first_ty) {
                            self.error(TypeError::TypeMismatch {
                                expected: first_ty.to_string(),
                                found: element.ty.to_string(),
                                span: element.span.into(),
                            });
                        }
                    }
                    first_ty
                };

                SemExpr {
                    kind: SemExprKind::Array(lowered),
                    ty: Type::Array(Box::new(elem_ty)),
                    span: *span,
                }
            }
            Expr::Grouped { expr, .. } => self.lower_expr(expr),
        }
    }

    fn lower_struct_construct(
        &mut self,
        struct_name: &str,
        args: &[Expr],
        span: crate::errors::Span,
    ) -> SemExpr {
        let sig = self.structs.get(struct_name).cloned().unwrap();

        if args.len() != sig.fields.len() {
            self.error(TypeError::WrongArity {
                name: format!("{}.new", struct_name),
                expected: sig.fields.len(),
                found: args.len(),
                span: span.into(),
            });
            return SemExpr {
                kind: SemExprKind::StructConstruct {
                    name: struct_name.to_string(),
                    args: Vec::new(),
                },
                ty: Type::Error,
                span,
            };
        }

        let mut lowered_args = Vec::with_capacity(args.len());
        for (arg, (_field_name, field_type)) in args.iter().zip(sig.fields.iter()) {
            let lowered = self.lower_expr(arg);
            if !lowered.ty.is_assignable_to(field_type) {
                self.error(TypeError::TypeMismatch {
                    expected: field_type.to_string(),
                    found: lowered.ty.to_string(),
                    span: lowered.span.into(),
                });
            }
            lowered_args.push(lowered);
        }

        SemExpr {
            kind: SemExprKind::StructConstruct {
                name: struct_name.to_string(),
                args: lowered_args,
            },
            ty: Type::Struct(struct_name.to_string()),
            span,
        }
    }

    fn lower_match_arm(
        &mut self,
        arm: &crate::parser::ast::MatchArm,
        subject_ty: &Type,
    ) -> (SemPattern, SemBlock) {
        use crate::parser::ast::Pattern;

        self.push_scope();

        let pattern = match &arm.pattern {
            Pattern::Wildcard { .. } => SemPattern::Wildcard,
            Pattern::Literal(expr) => {
                let lowered = self.lower_expr(expr);
                if !lowered.ty.is_assignable_to(subject_ty)
                    && lowered.ty != Type::Error
                    && *subject_ty != Type::Error
                {
                    self.error(TypeError::TypeMismatch {
                        expected: subject_ty.to_string(),
                        found: lowered.ty.to_string(),
                        span: lowered.span.into(),
                    });
                }
                SemPattern::Literal(lowered)
            }
            Pattern::Variable { name, .. } => {
                self.define(name, subject_ty.clone());
                SemPattern::Variable {
                    name: name.clone(),
                    ty: subject_ty.clone(),
                }
            }
        };

        let body = self.lower_block(&arm.body);
        self.pop_scope();

        (pattern, body)
    }

    fn binary_type(
        &mut self,
        left: &Type,
        op: BinaryOp,
        right: &Type,
        span: crate::errors::Span,
    ) -> Type {
        if *left == Type::Error || *right == Type::Error {
            return Type::Error;
        }

        if matches!(op, BinaryOp::Range | BinaryOp::RangeIncl) {
            if *left == Type::Int && *right == Type::Int {
                return Type::Array(Box::new(Type::Int));
            }

            self.error(TypeError::InvalidOperator {
                op: op.as_str().to_string(),
                lhs: left.to_string(),
                rhs: right.to_string(),
                span: span.into(),
            });
            return Type::Error;
        }

        if let Some(result) = left.binary_op_result(op.as_str(), right) {
            result
        } else {
            self.error(TypeError::InvalidOperator {
                op: op.as_str().to_string(),
                lhs: left.to_string(),
                rhs: right.to_string(),
                span: span.into(),
            });
            Type::Error
        }
    }

    fn unary_type(&mut self, expr: &Type, op: UnaryOp, span: crate::errors::Span) -> Type {
        if *expr == Type::Error {
            return Type::Error;
        }

        if let Some(result) = expr.unary_op_result(op.as_str()) {
            result
        } else {
            self.error(TypeError::InvalidOperator {
                op: op.as_str().to_string(),
                lhs: expr.to_string(),
                rhs: String::new(),
                span: span.into(),
            });
            Type::Error
        }
    }

    fn push_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }

    fn pop_scope(&mut self) {
        self.scopes.pop();
    }

    fn define(&mut self, name: &str, ty: Type) {
        let symbol = Symbol { ty };
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.to_string(), symbol);
        }
    }

    fn lookup(&self, name: &str) -> Option<&Symbol> {
        for scope in self.scopes.iter().rev() {
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
        }
        None
    }

    fn error(&mut self, error: TypeError) {
        self.errors.push(error);
    }
}

impl Default for SemLowerer {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;
    use crate::parser::Parser;

    fn lower(source: &str) -> Result<SemProgram, Vec<TypeError>> {
        let tokens = Scanner::new(source).scan_all().unwrap();
        let program = Parser::new(tokens).parse().unwrap();
        SemLowerer::new().lower(&program)
    }

    #[test]
    fn test_lower_simple_program() {
        let lowered = lower("def main\n  x = 10\n  puts(x)\nend");
        assert!(lowered.is_ok());
    }

    #[test]
    fn test_lower_type_mismatch() {
        let lowered = lower("def main\n  let x: Int = \"hello\"\nend");
        assert!(lowered.is_err());
    }

    #[test]
    fn test_reassign_let_binding_is_valid() {
        let lowered = lower("def main\n  let x: Int = 1\n  x = 2\nend");
        assert!(lowered.is_ok());
    }

    #[test]
    fn test_missing_return_error() {
        let lowered = lower("def answer -> Int\n  x = 10\nend");
        let errors = lowered.expect_err("expected missing return error");
        assert!(errors
            .iter()
            .any(|err| matches!(err, TypeError::MissingReturn { .. })));
    }

    #[test]
    fn test_unreachable_code_error() {
        let lowered = lower("def main\n  return\n  x = 10\nend");
        let errors = lowered.expect_err("expected unreachable code error");
        assert!(errors
            .iter()
            .any(|err| matches!(err, TypeError::UnreachableCode { .. })));
    }

    #[test]
    fn test_no_missing_return_when_if_else_returns() {
        let lowered = lower(
            "def answer(flag: Bool) -> Int\n  if flag\n    return 1\n  else\n    return 2\n  end\nend",
        );
        assert!(lowered.is_ok());
    }

    #[test]
    fn test_missing_return_when_only_one_branch_returns() {
        let lowered = lower("def answer(flag: Bool) -> Int\n  if flag\n    return 1\n  end\nend");
        let errors = lowered.expect_err("expected missing return error");
        assert!(errors
            .iter()
            .any(|err| matches!(err, TypeError::MissingReturn { .. })));
    }

    #[test]
    fn test_unreachable_after_if_else_return() {
        let lowered =
            lower("def main\n  if true\n    return\n  else\n    return\n  end\n  x = 10\nend");
        let errors = lowered.expect_err("expected unreachable code error");
        assert!(errors
            .iter()
            .any(|err| matches!(err, TypeError::UnreachableCode { .. })));
    }
}
