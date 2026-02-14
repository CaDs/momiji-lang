use std::fmt;

/// Internal representation of types used during type checking
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// Integer type (64-bit signed)
    Int,
    /// Floating point type (64-bit)
    Float,
    /// Boolean type
    Bool,
    /// String type
    String,
    /// Nil type
    Nil,
    /// Unit type (void/no return value)
    Unit,
    /// Array type
    Array(Box<Type>),
    /// Nullable type
    Nullable(Box<Type>),
    /// Struct type (identified by name)
    Struct(String),
    /// Unknown type (for type inference)
    Unknown,
    /// Error type (for error recovery)
    Error,
}

/// Struct signature for type checking
#[derive(Debug, Clone)]
pub struct StructSignature {
    pub fields: Vec<(String, Type)>,
}

impl Type {
    /// Check if this type can be assigned to another type
    pub fn is_assignable_to(&self, other: &Type) -> bool {
        if self == other {
            return true;
        }

        match (self, other) {
            // Nil can be assigned to nullable types
            (Type::Nil, Type::Nullable(_)) => true,
            // A type can be assigned to its nullable version
            (t, Type::Nullable(inner)) if t == inner.as_ref() => true,
            // Int can be assigned to Float
            (Type::Int, Type::Float) => true,
            // Same-name struct match
            (Type::Struct(a), Type::Struct(b)) if a == b => true,
            // Error type can be assigned to anything (for error recovery)
            (Type::Error, _) | (_, Type::Error) => true,
            _ => false,
        }
    }

    /// Get the result type of a binary operation
    pub fn binary_op_result(&self, op: &str, other: &Type) -> Option<Type> {
        match (self, op, other) {
            // Arithmetic operations
            (Type::Int, "+" | "-" | "*" | "/" | "%", Type::Int) => Some(Type::Int),
            (Type::Float, "+" | "-" | "*" | "/", Type::Float) => Some(Type::Float),
            (Type::Int, "+" | "-" | "*" | "/", Type::Float)
            | (Type::Float, "+" | "-" | "*" | "/", Type::Int) => Some(Type::Float),

            // String concatenation
            (Type::String, "+", Type::String) => Some(Type::String),

            // Comparison operations
            (Type::Int, "==" | "!=" | "<" | "<=" | ">" | ">=", Type::Int) => Some(Type::Bool),
            (Type::Float, "==" | "!=" | "<" | "<=" | ">" | ">=", Type::Float) => Some(Type::Bool),
            (Type::String, "==" | "!=", Type::String) => Some(Type::Bool),
            (Type::Bool, "==" | "!=", Type::Bool) => Some(Type::Bool),

            // Logical operations
            (Type::Bool, "and" | "or", Type::Bool) => Some(Type::Bool),

            // Bitwise operations
            (Type::Int, "&" | "|" | "^" | "<<" | ">>", Type::Int) => Some(Type::Int),

            _ => None,
        }
    }

    /// Get the result type of a unary operation
    pub fn unary_op_result(&self, op: &str) -> Option<Type> {
        match (self, op) {
            (Type::Int, "-") => Some(Type::Int),
            (Type::Float, "-") => Some(Type::Float),
            (Type::Bool, "not") => Some(Type::Bool),
            (Type::Int, "~") => Some(Type::Int),
            _ => None,
        }
    }
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Float => write!(f, "Float"),
            Type::Bool => write!(f, "Bool"),
            Type::String => write!(f, "String"),
            Type::Nil => write!(f, "Nil"),
            Type::Unit => write!(f, "Unit"),
            Type::Array(elem) => write!(f, "[{}]", elem),
            Type::Nullable(inner) => write!(f, "{}?", inner),
            Type::Struct(name) => write!(f, "{}", name),
            Type::Unknown => write!(f, "?"),
            Type::Error => write!(f, "<error>"),
        }
    }
}

/// Symbol table entry for variables
#[derive(Debug, Clone)]
pub struct Symbol {
    pub ty: Type,
}

/// Function signature for type checking
#[derive(Debug, Clone)]
pub struct FunctionSignature {
    pub params: Vec<(String, Type)>,
    pub return_type: Type,
}
