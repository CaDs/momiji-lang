use std::collections::HashMap;

use crate::errors::TypeError;
use crate::parser::ast::*;
use crate::types::types::{FunctionSignature, Symbol, Type};

/// Type checker for Momiji programs
pub struct TypeChecker {
    /// Global function signatures
    functions: HashMap<String, FunctionSignature>,
    /// Builtin function signatures
    builtins: HashMap<String, FunctionSignature>,
    /// Current scope's variables
    scopes: Vec<HashMap<String, Symbol>>,
    /// Current function's return type
    current_return_type: Option<Type>,
    /// Collected errors
    errors: Vec<TypeError>,
}

impl TypeChecker {
    pub fn new() -> Self {
        let mut checker = Self {
            functions: HashMap::new(),
            builtins: HashMap::new(),
            scopes: vec![HashMap::new()],
            current_return_type: None,
            errors: Vec::new(),
        };

        // Register builtin functions
        checker.register_builtins();
        checker
    }

    fn register_builtins(&mut self) {
        // puts(value: Any) -> Unit
        self.builtins.insert(
            "puts".to_string(),
            FunctionSignature {
                params: vec![("value".to_string(), Type::Unknown)], // Accept any type
                return_type: Type::Unit,
            },
        );

        // print(value: Any) -> Unit
        self.builtins.insert(
            "print".to_string(),
            FunctionSignature {
                params: vec![("value".to_string(), Type::Unknown)],
                return_type: Type::Unit,
            },
        );
    }

    /// Type check a program
    pub fn check(&mut self, program: &Program) -> Result<(), Vec<TypeError>> {
        // First pass: collect all function signatures
        for item in &program.items {
            match item {
                Item::Function(f) => {
                    let sig = self.function_signature(f);
                    self.functions.insert(f.name.clone(), sig);
                }
                Item::Struct(_) => {}
            }
        }

        // Second pass: type check function bodies
        for item in &program.items {
            match item {
                Item::Function(f) => {
                    self.check_function(f);
                }
                Item::Struct(_) => {}
            }
        }

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn function_signature(&self, f: &Function) -> FunctionSignature {
        let params: Vec<(String, Type)> = f
            .params
            .iter()
            .map(|p| (p.name.clone(), self.resolve_type(&p.ty.kind)))
            .collect();

        let return_type = f
            .return_type
            .as_ref()
            .map(|t| self.resolve_type(&t.kind))
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
                _ => Type::Unknown, // User-defined types (not yet supported)
            },
            TypeKind::Nullable(inner) => Type::Nullable(Box::new(self.resolve_type(inner))),
            TypeKind::Array(elem) => Type::Array(Box::new(self.resolve_type(elem))),
        }
    }

    fn check_function(&mut self, f: &Function) {
        self.push_scope();

        // Add parameters to scope
        for param in &f.params {
            let ty = self.resolve_type(&param.ty.kind);
            self.define(&param.name, ty);
        }

        // Set current return type
        let return_type = f
            .return_type
            .as_ref()
            .map(|t| self.resolve_type(&t.kind))
            .unwrap_or(Type::Unit);
        self.current_return_type = Some(return_type);

        // Check body
        self.check_block(&f.body);

        self.current_return_type = None;
        self.pop_scope();
    }

    fn check_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.check_stmt(stmt);
        }
    }

    fn check_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.check_expr(expr);
            }
            Stmt::Let {
                name,
                ty,
                value,
                implicit,
                span,
            } => {
                let value_type = self.check_expr(value);

                if *implicit {
                    if let Some(existing) = self.lookup(name).cloned() {
                        if !value_type.is_assignable_to(&existing.ty) {
                            self.error(TypeError::TypeMismatch {
                                expected: existing.ty.to_string(),
                                found: value_type.to_string(),
                                span: (*span).into(),
                            });
                        }
                        return;
                    }
                }

                let declared_type = ty.as_ref().map(|t| self.resolve_type(&t.kind));

                let final_type = if let Some(declared) = &declared_type {
                    if !value_type.is_assignable_to(declared) {
                        self.error(TypeError::TypeMismatch {
                            expected: declared.to_string(),
                            found: value_type.to_string(),
                            span: (*span).into(),
                        });
                    }
                    declared.clone()
                } else {
                    value_type
                };

                self.define(name, final_type);
            }
            Stmt::Return { value, span } => {
                let return_type = self.current_return_type.clone().unwrap_or(Type::Unit);

                let value_type = value
                    .as_ref()
                    .map(|v| self.check_expr(v))
                    .unwrap_or(Type::Unit);

                if !value_type.is_assignable_to(&return_type) {
                    self.error(TypeError::TypeMismatch {
                        expected: return_type.to_string(),
                        found: value_type.to_string(),
                        span: (*span).into(),
                    });
                }
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let cond_type = self.check_expr(condition);
                if cond_type != Type::Bool && cond_type != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_type.to_string(),
                        span: (*span).into(),
                    });
                }

                self.push_scope();
                self.check_block(then_block);
                self.pop_scope();

                if let Some(else_block) = else_block {
                    self.push_scope();
                    self.check_block(else_block);
                    self.pop_scope();
                }
            }
            Stmt::While {
                condition,
                body,
                span,
            } => {
                let cond_type = self.check_expr(condition);
                if cond_type != Type::Bool && cond_type != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: "Bool".to_string(),
                        found: cond_type.to_string(),
                        span: (*span).into(),
                    });
                }

                self.push_scope();
                self.check_block(body);
                self.pop_scope();
            }
            Stmt::For {
                var,
                iterable,
                body,
                span: _,
            } => {
                let iter_type = self.check_expr(iterable);

                // Determine the element type from the iterable
                let elem_type = match &iter_type {
                    Type::Array(elem) => *elem.clone(),
                    _ => {
                        // For now, assume it's a range and use Int
                        Type::Int
                    }
                };

                self.push_scope();
                self.define(var, elem_type);
                self.check_block(body);
                self.pop_scope();
            }
            Stmt::Match {
                subject,
                arms,
                ..
            } => {
                self.check_expr(subject);
                for arm in arms {
                    self.push_scope();
                    self.check_block(&arm.body);
                    self.pop_scope();
                }
            }
        }
    }

    fn check_expr(&mut self, expr: &Expr) -> Type {
        match expr {
            Expr::Int { .. } => Type::Int,
            Expr::Float { .. } => Type::Float,
            Expr::String { .. } => Type::String,
            Expr::Bool { .. } => Type::Bool,
            Expr::Nil { .. } => Type::Nil,

            Expr::Identifier { name, span } => {
                if let Some(symbol) = self.lookup(name) {
                    symbol.ty.clone()
                } else {
                    self.error(TypeError::UndefinedVariable {
                        name: name.clone(),
                        span: (*span).into(),
                    });
                    Type::Error
                }
            }

            Expr::Binary {
                left,
                op,
                right,
                span,
            } => {
                let left_type = self.check_expr(left);
                let right_type = self.check_expr(right);

                if left_type == Type::Error || right_type == Type::Error {
                    return Type::Error;
                }

                let op_str = op.as_str();
                if let Some(result) = left_type.binary_op_result(op_str, &right_type) {
                    result
                } else {
                    self.error(TypeError::InvalidOperator {
                        op: op_str.to_string(),
                        lhs: left_type.to_string(),
                        rhs: right_type.to_string(),
                        span: (*span).into(),
                    });
                    Type::Error
                }
            }

            Expr::Unary { op, expr, span } => {
                let expr_type = self.check_expr(expr);

                if expr_type == Type::Error {
                    return Type::Error;
                }

                let op_str = op.as_str();
                if let Some(result) = expr_type.unary_op_result(op_str) {
                    result
                } else {
                    self.error(TypeError::InvalidOperator {
                        op: op_str.to_string(),
                        lhs: expr_type.to_string(),
                        rhs: "".to_string(),
                        span: (*span).into(),
                    });
                    Type::Error
                }
            }

            Expr::Call { callee, args, span } => {
                // Get function name
                let func_name = match callee.as_ref() {
                    Expr::Identifier { name, .. } => name.clone(),
                    _ => {
                        // For now, only support simple function calls
                        return Type::Unknown;
                    }
                };

                // Look up function signature
                let sig = self
                    .functions
                    .get(&func_name)
                    .or_else(|| self.builtins.get(&func_name))
                    .cloned();

                if let Some(sig) = sig {
                    // Check argument count
                    if args.len() != sig.params.len() {
                        self.error(TypeError::WrongArity {
                            name: func_name,
                            expected: sig.params.len(),
                            found: args.len(),
                            span: (*span).into(),
                        });
                        return Type::Error;
                    }

                    // Check argument types
                    for (arg, (_param_name, param_type)) in args.iter().zip(sig.params.iter()) {
                        let arg_type = self.check_expr(arg);
                        // Skip type check for Unknown (variadic/any) params
                        if *param_type != Type::Unknown && !arg_type.is_assignable_to(param_type) {
                            self.error(TypeError::TypeMismatch {
                                expected: param_type.to_string(),
                                found: arg_type.to_string(),
                                span: arg.span().into(),
                            });
                        }
                    }

                    sig.return_type
                } else {
                    self.error(TypeError::UndefinedFunction {
                        name: func_name,
                        span: (*span).into(),
                    });
                    Type::Error
                }
            }

            Expr::Field {
                object,
                field: _,
                span: _,
            } => {
                let _obj_type = self.check_expr(object);
                // For now, field access is not fully supported
                Type::Unknown
            }

            Expr::Index {
                object,
                index,
                span: _,
            } => {
                let obj_type = self.check_expr(object);
                let idx_type = self.check_expr(index);

                if idx_type != Type::Int && idx_type != Type::Error {
                    self.error(TypeError::TypeMismatch {
                        expected: "Int".to_string(),
                        found: idx_type.to_string(),
                        span: index.span().into(),
                    });
                }

                match obj_type {
                    Type::Array(elem) => *elem,
                    Type::String => Type::String, // String indexing returns a character (as string)
                    Type::Error => Type::Error,
                    _ => Type::Unknown,
                }
            }

            Expr::Array { elements, span: _ } => {
                if elements.is_empty() {
                    return Type::Array(Box::new(Type::Unknown));
                }

                let first_type = self.check_expr(&elements[0]);
                for elem in elements.iter().skip(1) {
                    let elem_type = self.check_expr(elem);
                    if !elem_type.is_assignable_to(&first_type) {
                        self.error(TypeError::TypeMismatch {
                            expected: first_type.to_string(),
                            found: elem_type.to_string(),
                            span: elem.span().into(),
                        });
                    }
                }

                Type::Array(Box::new(first_type))
            }

            Expr::Grouped { expr, .. } => self.check_expr(expr),
        }
    }

    // Scope management

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

impl Default for TypeChecker {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;
    use crate::parser::Parser;

    fn check(source: &str) -> Result<(), Vec<TypeError>> {
        let tokens = Scanner::new(source).scan_all().unwrap();
        let program = Parser::new(tokens).parse().unwrap();
        TypeChecker::new().check(&program)
    }

    #[test]
    fn test_simple_function() {
        assert!(check("def main\nend").is_ok());
    }

    #[test]
    fn test_arithmetic() {
        assert!(check("def main\n  x = 1 + 2\nend").is_ok());
    }

    #[test]
    fn test_type_mismatch() {
        let result = check("def add(a: Int, b: Int) -> Int\n  return \"hello\"\nend");
        assert!(result.is_err());
    }

    #[test]
    fn test_undefined_variable() {
        let result = check("def main\n  x = y + 1\nend");
        assert!(result.is_err());
    }

    #[test]
    fn test_puts_any_type() {
        assert!(check("def main\n  puts(42)\nend").is_ok());
        assert!(check("def main\n  puts(\"hello\")\nend").is_ok());
        assert!(check("def main\n  puts(true)\nend").is_ok());
    }
}
