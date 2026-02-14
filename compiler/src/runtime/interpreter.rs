use std::collections::HashMap;
use std::fmt;

use crate::errors::RuntimeError;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::sem::ir::{SemBlock, SemExpr, SemExprKind, SemFunction, SemProgram, SemStmt};
use crate::types::Type;

#[derive(Debug, Clone, PartialEq)]
enum Value {
    Int(i64),
    Float(f64),
    Bool(bool),
    String(String),
    Nil,
    Array(Vec<Value>),
    Unit,
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Int(v) => write!(f, "{}", v),
            Value::Float(v) => write!(f, "{}", v),
            Value::Bool(v) => write!(f, "{}", v),
            Value::String(v) => write!(f, "{}", v),
            Value::Nil => write!(f, "nil"),
            Value::Array(values) => {
                write!(f, "[")?;
                for (i, value) in values.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", value)?;
                }
                write!(f, "]")
            }
            Value::Unit => write!(f, "()"),
        }
    }
}

#[derive(Debug, Clone)]
struct Variable {
    value: Value,
    ty: Type,
}

enum ControlFlow {
    Continue,
    Return {
        value: Value,
        span: crate::errors::Span,
    },
}

type Scope = HashMap<String, Variable>;

/// Semantic-IR interpreter used as the default execution backend for MVP correctness.
pub struct Interpreter {
    emit_output: bool,
    output: String,
}

impl Interpreter {
    pub fn new(emit_output: bool) -> Self {
        Self {
            emit_output,
            output: String::new(),
        }
    }

    #[cfg(test)]
    pub fn output(&self) -> &str {
        &self.output
    }

    pub fn run_program(&mut self, program: &SemProgram) -> Result<(), RuntimeError> {
        let main = self
            .find_function(program, "main")
            .ok_or(RuntimeError::MissingMain)?;

        if !main.params.is_empty() {
            return Err(RuntimeError::WrongArity {
                name: "main".to_string(),
                expected: 0,
                found: main.params.len(),
                span: main.span.into(),
            });
        }

        let _ = self.call_function(program, "main", Vec::new(), main.span)?;
        Ok(())
    }

    fn call_function(
        &mut self,
        program: &SemProgram,
        name: &str,
        args: Vec<Value>,
        call_span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        match name {
            "puts" => return self.eval_puts(args, call_span),
            "print" => return self.eval_print(args, call_span),
            _ => {}
        }

        let function =
            self.find_function(program, name)
                .ok_or(RuntimeError::UndefinedFunction {
                    name: name.to_string(),
                    span: call_span.into(),
                })?;

        if args.len() != function.params.len() {
            return Err(RuntimeError::WrongArity {
                name: name.to_string(),
                expected: function.params.len(),
                found: args.len(),
                span: call_span.into(),
            });
        }

        let mut scopes = vec![HashMap::new()];
        for (param, arg) in function.params.iter().zip(args.into_iter()) {
            self.ensure_runtime_type(
                &arg,
                &param.ty,
                call_span,
                &format!("argument `{}` in call to `{}`", param.name, name),
            )?;
            self.define_variable(&mut scopes, &param.name, param.ty.clone(), arg);
        }

        let (value, value_span) = match self.exec_block(program, &mut scopes, &function.body)? {
            ControlFlow::Continue => (Value::Unit, function.span),
            ControlFlow::Return { value, span } => (value, span),
        };

        self.ensure_runtime_type(
            &value,
            &function.return_type,
            value_span,
            &format!("return value from `{}`", name),
        )?;

        Ok(value)
    }

    fn exec_block(
        &mut self,
        program: &SemProgram,
        scopes: &mut Vec<Scope>,
        block: &SemBlock,
    ) -> Result<ControlFlow, RuntimeError> {
        for stmt in &block.stmts {
            match self.exec_stmt(program, scopes, stmt)? {
                ControlFlow::Continue => {}
                control @ ControlFlow::Return { .. } => return Ok(control),
            }
        }
        Ok(ControlFlow::Continue)
    }

    fn exec_stmt(
        &mut self,
        program: &SemProgram,
        scopes: &mut Vec<Scope>,
        stmt: &SemStmt,
    ) -> Result<ControlFlow, RuntimeError> {
        match stmt {
            SemStmt::Expr(expr) => {
                self.eval_expr(program, scopes, expr)?;
                Ok(ControlFlow::Continue)
            }
            SemStmt::Let {
                name,
                ty,
                value,
                span,
            } => {
                let evaluated = self.eval_expr(program, scopes, value)?;
                self.ensure_runtime_type(
                    &evaluated,
                    ty,
                    *span,
                    &format!("initializer for `{}`", name),
                )?;
                self.define_variable(scopes, name, ty.clone(), evaluated);
                Ok(ControlFlow::Continue)
            }
            SemStmt::Assign { name, value, span } => {
                let evaluated = self.eval_expr(program, scopes, value)?;
                self.assign_variable(scopes, name, evaluated, *span)?;
                Ok(ControlFlow::Continue)
            }
            SemStmt::Return { value, span } => {
                let returned = if let Some(expr) = value {
                    self.eval_expr(program, scopes, expr)?
                } else {
                    Value::Unit
                };
                Ok(ControlFlow::Return {
                    value: returned,
                    span: *span,
                })
            }
            SemStmt::If {
                condition,
                then_block,
                else_block,
                span,
            } => {
                let cond = self.eval_expr(program, scopes, condition)?;
                let cond_bool = self.expect_bool(cond, *span)?;

                if cond_bool {
                    scopes.push(HashMap::new());
                    let result = self.exec_block(program, scopes, then_block);
                    scopes.pop();
                    result
                } else if let Some(else_block) = else_block {
                    scopes.push(HashMap::new());
                    let result = self.exec_block(program, scopes, else_block);
                    scopes.pop();
                    result
                } else {
                    Ok(ControlFlow::Continue)
                }
            }
            SemStmt::While {
                condition,
                body,
                span,
            } => {
                loop {
                    let cond = self.eval_expr(program, scopes, condition)?;
                    let cond_bool = self.expect_bool(cond, *span)?;
                    if !cond_bool {
                        break;
                    }

                    scopes.push(HashMap::new());
                    let flow = self.exec_block(program, scopes, body);
                    scopes.pop();
                    match flow? {
                        ControlFlow::Continue => {}
                        returned @ ControlFlow::Return { .. } => return Ok(returned),
                    }
                }
                Ok(ControlFlow::Continue)
            }
            SemStmt::For {
                var,
                elem_type,
                iterable,
                body,
                span,
            } => {
                let iterable_value = self.eval_expr(program, scopes, iterable)?;
                let values = match iterable_value {
                    Value::Array(values) => values,
                    other => {
                        return Err(RuntimeError::TypeError {
                            message: format!(
                                "expected iterable array in `for`, found {}",
                                Self::value_type_name(&other)
                            ),
                            span: (*span).into(),
                        });
                    }
                };

                for value in values {
                    scopes.push(HashMap::new());
                    self.ensure_runtime_type(
                        &value,
                        elem_type,
                        *span,
                        &format!("iteration binding `{}`", var),
                    )?;
                    self.define_variable(scopes, var, elem_type.clone(), value);
                    let flow = self.exec_block(program, scopes, body);
                    scopes.pop();
                    match flow? {
                        ControlFlow::Continue => {}
                        returned @ ControlFlow::Return { .. } => return Ok(returned),
                    }
                }

                Ok(ControlFlow::Continue)
            }
        }
    }

    fn eval_expr(
        &mut self,
        program: &SemProgram,
        scopes: &mut Vec<Scope>,
        expr: &SemExpr,
    ) -> Result<Value, RuntimeError> {
        match &expr.kind {
            SemExprKind::Int(value) => Ok(Value::Int(*value)),
            SemExprKind::Float(value) => Ok(Value::Float(*value)),
            SemExprKind::String(value) => Ok(Value::String(value.clone())),
            SemExprKind::Bool(value) => Ok(Value::Bool(*value)),
            SemExprKind::Nil => Ok(Value::Nil),
            SemExprKind::Variable(name) => self
                .lookup_variable(scopes, name)
                .map(|var| var.value.clone())
                .ok_or(RuntimeError::UndefinedVariable {
                    name: name.clone(),
                    span: expr.span.into(),
                }),
            SemExprKind::Binary { left, op, right } => match op {
                BinaryOp::And => {
                    let left_val = self.eval_expr(program, scopes, left)?;
                    let left_bool = self.expect_bool(left_val, left.span)?;
                    if !left_bool {
                        return Ok(Value::Bool(false));
                    }
                    let right_val = self.eval_expr(program, scopes, right)?;
                    let right_bool = self.expect_bool(right_val, right.span)?;
                    Ok(Value::Bool(right_bool))
                }
                BinaryOp::Or => {
                    let left_val = self.eval_expr(program, scopes, left)?;
                    let left_bool = self.expect_bool(left_val, left.span)?;
                    if left_bool {
                        return Ok(Value::Bool(true));
                    }
                    let right_val = self.eval_expr(program, scopes, right)?;
                    let right_bool = self.expect_bool(right_val, right.span)?;
                    Ok(Value::Bool(right_bool))
                }
                _ => {
                    let left_val = self.eval_expr(program, scopes, left)?;
                    let right_val = self.eval_expr(program, scopes, right)?;
                    self.eval_binary_op(*op, left_val, right_val, expr.span)
                }
            },
            SemExprKind::Unary { op, expr: inner } => {
                let value = self.eval_expr(program, scopes, inner)?;
                self.eval_unary_op(*op, value, expr.span)
            }
            SemExprKind::Call { name, args } => {
                let mut evaluated_args = Vec::with_capacity(args.len());
                for arg in args {
                    evaluated_args.push(self.eval_expr(program, scopes, arg)?);
                }
                self.call_function(program, name, evaluated_args, expr.span)
            }
            SemExprKind::Index { object, index } => {
                let object_value = self.eval_expr(program, scopes, object)?;
                let index_value = self.eval_expr(program, scopes, index)?;
                let idx = self.expect_int(index_value, index.span)?;
                if idx < 0 {
                    return Err(RuntimeError::TypeError {
                        message: "index must be non-negative".to_string(),
                        span: expr.span.into(),
                    });
                }

                match object_value {
                    Value::Array(values) => {
                        values
                            .get(idx as usize)
                            .cloned()
                            .ok_or(RuntimeError::TypeError {
                                message: "array index out of bounds".to_string(),
                                span: expr.span.into(),
                            })
                    }
                    Value::String(value) => value
                        .chars()
                        .nth(idx as usize)
                        .map(|ch| Value::String(ch.to_string()))
                        .ok_or(RuntimeError::TypeError {
                            message: "string index out of bounds".to_string(),
                            span: expr.span.into(),
                        }),
                    other => Err(RuntimeError::TypeError {
                        message: format!(
                            "cannot index into value of type {}",
                            Self::value_type_name(&other)
                        ),
                        span: expr.span.into(),
                    }),
                }
            }
            SemExprKind::Array(elements) => {
                let mut values = Vec::with_capacity(elements.len());
                for element in elements {
                    values.push(self.eval_expr(program, scopes, element)?);
                }
                Ok(Value::Array(values))
            }
        }
    }

    fn eval_binary_op(
        &self,
        op: BinaryOp,
        left: Value,
        right: Value,
        span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        match op {
            BinaryOp::Add => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 + b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + b as f64)),
                (Value::String(a), Value::String(b)) => Ok(Value::String(format!("{}{}", a, b))),
                (a, b) => self.invalid_binary("+", &a, &b, span),
            },
            BinaryOp::Sub => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 - b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - b as f64)),
                (a, b) => self.invalid_binary("-", &a, &b, span),
            },
            BinaryOp::Mul => match (left, right) {
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 * b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * b as f64)),
                (a, b) => self.invalid_binary("*", &a, &b, span),
            },
            BinaryOp::Div => match (left, right) {
                (_, Value::Int(0)) | (_, Value::Float(0.0)) => {
                    Err(RuntimeError::DivisionByZero { span: span.into() })
                }
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a / b)),
                (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a / b)),
                (Value::Int(a), Value::Float(b)) => Ok(Value::Float(a as f64 / b)),
                (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a / b as f64)),
                (a, b) => self.invalid_binary("/", &a, &b, span),
            },
            BinaryOp::Mod => match (left, right) {
                (_, Value::Int(0)) => Err(RuntimeError::DivisionByZero { span: span.into() }),
                (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a % b)),
                (a, b) => self.invalid_binary("%", &a, &b, span),
            },
            BinaryOp::Eq => Ok(Value::Bool(self.values_equal(&left, &right))),
            BinaryOp::Ne => Ok(Value::Bool(!self.values_equal(&left, &right))),
            BinaryOp::Lt => self.numeric_cmp(left, right, span, |a, b| a < b),
            BinaryOp::Le => self.numeric_cmp(left, right, span, |a, b| a <= b),
            BinaryOp::Gt => self.numeric_cmp(left, right, span, |a, b| a > b),
            BinaryOp::Ge => self.numeric_cmp(left, right, span, |a, b| a >= b),
            BinaryOp::BitAnd => self.int_binary("&", left, right, span, |a, b| a & b),
            BinaryOp::BitOr => self.int_binary("|", left, right, span, |a, b| a | b),
            BinaryOp::BitXor => self.int_binary("^", left, right, span, |a, b| a ^ b),
            BinaryOp::Shl => self.int_binary("<<", left, right, span, |a, b| a << b),
            BinaryOp::Shr => self.int_binary(">>", left, right, span, |a, b| a >> b),
            BinaryOp::Range | BinaryOp::RangeIncl => {
                let start = self.expect_int(left, span)?;
                let end = self.expect_int(right, span)?;
                let mut values = Vec::new();
                if start <= end {
                    let mut current = start;
                    while current < end || (matches!(op, BinaryOp::RangeIncl) && current <= end) {
                        values.push(Value::Int(current));
                        current += 1;
                    }
                }
                Ok(Value::Array(values))
            }
            BinaryOp::And | BinaryOp::Or => unreachable!("handled as short-circuit earlier"),
        }
    }

    fn eval_unary_op(
        &self,
        op: UnaryOp,
        value: Value,
        span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        match op {
            UnaryOp::Neg => match value {
                Value::Int(v) => Ok(Value::Int(-v)),
                Value::Float(v) => Ok(Value::Float(-v)),
                other => Err(RuntimeError::TypeError {
                    message: format!("cannot apply '-' to {}", Self::value_type_name(&other)),
                    span: span.into(),
                }),
            },
            UnaryOp::Not => match value {
                Value::Bool(v) => Ok(Value::Bool(!v)),
                other => Err(RuntimeError::TypeError {
                    message: format!("cannot apply 'not' to {}", Self::value_type_name(&other)),
                    span: span.into(),
                }),
            },
            UnaryOp::BitNot => match value {
                Value::Int(v) => Ok(Value::Int(!v)),
                other => Err(RuntimeError::TypeError {
                    message: format!("cannot apply '~' to {}", Self::value_type_name(&other)),
                    span: span.into(),
                }),
            },
        }
    }

    fn eval_puts(
        &mut self,
        args: Vec<Value>,
        span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::WrongArity {
                name: "puts".to_string(),
                expected: 1,
                found: args.len(),
                span: span.into(),
            });
        }

        let text = format!("{}\n", args[0]);
        self.output.push_str(&text);
        if self.emit_output {
            print!("{}", text);
        }
        Ok(Value::Unit)
    }

    fn eval_print(
        &mut self,
        args: Vec<Value>,
        span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        if args.len() != 1 {
            return Err(RuntimeError::WrongArity {
                name: "print".to_string(),
                expected: 1,
                found: args.len(),
                span: span.into(),
            });
        }

        let text = format!("{}", args[0]);
        self.output.push_str(&text);
        if self.emit_output {
            print!("{}", text);
        }
        Ok(Value::Unit)
    }

    fn expect_bool(&self, value: Value, span: crate::errors::Span) -> Result<bool, RuntimeError> {
        match value {
            Value::Bool(v) => Ok(v),
            other => Err(RuntimeError::TypeError {
                message: format!("expected Bool, found {}", Self::value_type_name(&other)),
                span: span.into(),
            }),
        }
    }

    fn expect_int(&self, value: Value, span: crate::errors::Span) -> Result<i64, RuntimeError> {
        match value {
            Value::Int(v) => Ok(v),
            other => Err(RuntimeError::TypeError {
                message: format!("expected Int, found {}", Self::value_type_name(&other)),
                span: span.into(),
            }),
        }
    }

    fn int_binary<F>(
        &self,
        op: &str,
        left: Value,
        right: Value,
        span: crate::errors::Span,
        f: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(i64, i64) -> i64,
    {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(f(a, b))),
            (a, b) => self.invalid_binary(op, &a, &b, span),
        }
    }

    fn numeric_cmp<F>(
        &self,
        left: Value,
        right: Value,
        span: crate::errors::Span,
        cmp: F,
    ) -> Result<Value, RuntimeError>
    where
        F: Fn(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(cmp(a as f64, b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(cmp(a, b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(cmp(a as f64, b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(cmp(a, b as f64))),
            (a, b) => self.invalid_binary("comparison", &a, &b, span),
        }
    }

    fn invalid_binary(
        &self,
        op: &str,
        left: &Value,
        right: &Value,
        span: crate::errors::Span,
    ) -> Result<Value, RuntimeError> {
        Err(RuntimeError::TypeError {
            message: format!(
                "invalid '{}' for {} and {}",
                op,
                Self::value_type_name(left),
                Self::value_type_name(right)
            ),
            span: span.into(),
        })
    }

    fn values_equal(&self, left: &Value, right: &Value) -> bool {
        match (left, right) {
            (Value::Int(a), Value::Float(b)) => (*a as f64) == *b,
            (Value::Float(a), Value::Int(b)) => *a == (*b as f64),
            _ => left == right,
        }
    }

    fn lookup_variable<'a>(&self, scopes: &'a [Scope], name: &str) -> Option<&'a Variable> {
        for scope in scopes.iter().rev() {
            if let Some(var) = scope.get(name) {
                return Some(var);
            }
        }
        None
    }

    fn lookup_variable_mut<'a>(
        &self,
        scopes: &'a mut [Scope],
        name: &str,
    ) -> Option<&'a mut Variable> {
        for scope in scopes.iter_mut().rev() {
            if let Some(var) = scope.get_mut(name) {
                return Some(var);
            }
        }
        None
    }

    fn define_variable(&self, scopes: &mut [Scope], name: &str, ty: Type, value: Value) {
        if let Some(scope) = scopes.last_mut() {
            scope.insert(name.to_string(), Variable { value, ty });
        }
    }

    fn assign_variable(
        &self,
        scopes: &mut [Scope],
        name: &str,
        value: Value,
        span: crate::errors::Span,
    ) -> Result<(), RuntimeError> {
        if let Some(var) = self.lookup_variable_mut(scopes, name) {
            self.ensure_runtime_type(&value, &var.ty, span, &format!("assignment to `{}`", name))?;
            var.value = value;
            return Ok(());
        }

        Err(RuntimeError::UndefinedVariable {
            name: name.to_string(),
            span: span.into(),
        })
    }

    fn value_type_name(value: &Value) -> &'static str {
        match value {
            Value::Int(_) => "Int",
            Value::Float(_) => "Float",
            Value::Bool(_) => "Bool",
            Value::String(_) => "String",
            Value::Nil => "Nil",
            Value::Array(_) => "Array",
            Value::Unit => "Unit",
        }
    }

    fn ensure_runtime_type(
        &self,
        value: &Value,
        expected: &Type,
        span: crate::errors::Span,
        context: &str,
    ) -> Result<(), RuntimeError> {
        if Self::value_matches_type(value, expected) {
            return Ok(());
        }

        Err(RuntimeError::TypeError {
            message: format!(
                "type mismatch in {}: expected {}, found {}",
                context,
                expected,
                Self::value_type_name(value)
            ),
            span: span.into(),
        })
    }

    fn value_matches_type(value: &Value, expected: &Type) -> bool {
        match expected {
            Type::Unknown | Type::Error => true,
            Type::Int => matches!(value, Value::Int(_)),
            Type::Float => matches!(value, Value::Float(_) | Value::Int(_)),
            Type::Bool => matches!(value, Value::Bool(_)),
            Type::String => matches!(value, Value::String(_)),
            Type::Nil => matches!(value, Value::Nil),
            Type::Unit => matches!(value, Value::Unit),
            Type::Nullable(inner) => {
                matches!(value, Value::Nil) || Self::value_matches_type(value, inner)
            }
            Type::Array(inner) => match value {
                Value::Array(values) => values
                    .iter()
                    .all(|element| Self::value_matches_type(element, inner)),
                _ => false,
            },
        }
    }

    fn find_function<'a>(&self, program: &'a SemProgram, name: &str) -> Option<&'a SemFunction> {
        program
            .functions
            .iter()
            .find(|function| function.name == name)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::errors::RuntimeError;
    use crate::lexer::Scanner;
    use crate::parser::Parser;
    use crate::sem::SemLowerer;

    fn interpret(source: &str) -> String {
        interpret_result(source).unwrap()
    }

    fn interpret_result(source: &str) -> Result<String, RuntimeError> {
        let tokens = Scanner::new(source).scan_all().unwrap();
        let program = Parser::new(tokens).parse().unwrap();
        let sem = SemLowerer::new().lower(&program).unwrap();

        let mut interpreter = Interpreter::new(false);
        interpreter.run_program(&sem)?;
        Ok(interpreter.output().to_string())
    }

    #[test]
    fn test_interprets_hello_world() {
        let output = interpret("def main\n  puts(\"Hello, Momiji!\")\nend");
        assert_eq!(output, "Hello, Momiji!\n");
    }

    #[test]
    fn test_interprets_while_loop() {
        let output =
            interpret("def main\n  i = 1\n  while i <= 3\n    puts(i)\n    i = i + 1\n  end\nend");
        assert_eq!(output, "1\n2\n3\n");
    }

    #[test]
    fn test_assignment_updates_existing_variable() {
        let output = interpret("def main\n  x = 10\n  x = x + 5\n  puts(x)\nend");
        assert_eq!(output, "15\n");
    }

    #[test]
    fn test_interprets_user_function_calls() {
        let output = interpret(
            "def add(a: Int, b: Int) -> Int\n  return a + b\nend\n\ndef main\n  puts(add(2, 5))\nend",
        );
        assert_eq!(output, "7\n");
    }

    #[test]
    fn test_interprets_range_index_access() {
        let output = interpret("def main\n  r = 3..6\n  puts(r[0])\n  puts(r[2])\nend");
        assert_eq!(output, "3\n5\n");
    }

    #[test]
    fn test_reports_array_index_out_of_bounds() {
        let err = interpret_result("def main\n  arr = [1, 2]\n  puts(arr[2])\nend")
            .expect_err("expected index error");

        match err {
            RuntimeError::TypeError { message, .. } => {
                assert!(message.contains("out of bounds"));
            }
            other => panic!("unexpected runtime error: {:?}", other),
        }
    }
}
