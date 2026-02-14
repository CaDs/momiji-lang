use crate::errors::Span;

/// A complete Momiji program
#[derive(Debug, Clone)]
pub struct Program {
    pub items: Vec<Item>,
}

/// Top-level items in a program
#[derive(Debug, Clone)]
pub enum Item {
    Function(Function),
}

/// A function definition
#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<Parameter>,
    pub return_type: Option<TypeAnnotation>,
    pub body: Block,
    pub span: Span,
}

/// A function parameter
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub ty: TypeAnnotation,
}

/// A type annotation
#[derive(Debug, Clone)]
pub struct TypeAnnotation {
    pub kind: TypeKind,
}

/// The kind of type annotation
#[derive(Debug, Clone, PartialEq)]
pub enum TypeKind {
    /// Named type like `Int`, `String`, `MyStruct`
    Named(String),
    /// Nullable type like `Int?`
    Nullable(Box<TypeKind>),
    /// Array type like `[Int]`
    Array(Box<TypeKind>),
}

/// A block of statements
#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Stmt>,
}

/// Statements
#[derive(Debug, Clone)]
pub enum Stmt {
    /// Expression statement
    Expr(Expr),
    /// Variable declaration: `x = 10` or `let x: Int = 10`
    Let {
        name: String,
        ty: Option<TypeAnnotation>,
        value: Expr,
        /// True for implicit assignment-style statements (`x = ...`),
        /// false for explicit `let` declarations.
        implicit: bool,
        span: Span,
    },
    /// Return statement
    Return { value: Option<Expr>, span: Span },
    /// If statement
    If {
        condition: Expr,
        then_block: Block,
        else_block: Option<Block>,
        span: Span,
    },
    /// While loop
    While {
        condition: Expr,
        body: Block,
        span: Span,
    },
    /// For loop
    For {
        var: String,
        iterable: Expr,
        body: Block,
        span: Span,
    },
}

/// Expressions
#[derive(Debug, Clone)]
pub enum Expr {
    /// Integer literal
    Int { value: i64, span: Span },
    /// Float literal
    Float { value: f64, span: Span },
    /// String literal
    String { value: String, span: Span },
    /// Boolean literal
    Bool { value: bool, span: Span },
    /// Nil literal
    Nil { span: Span },
    /// Identifier (variable reference)
    Identifier { name: String, span: Span },
    /// Binary operation
    Binary {
        left: Box<Expr>,
        op: BinaryOp,
        right: Box<Expr>,
        span: Span,
    },
    /// Unary operation
    Unary {
        op: UnaryOp,
        expr: Box<Expr>,
        span: Span,
    },
    /// Function call
    Call {
        callee: Box<Expr>,
        args: Vec<Expr>,
        span: Span,
    },
    /// Field access (e.g., `obj.field`)
    Field {
        object: Box<Expr>,
        field: String,
        span: Span,
    },
    /// Index access (e.g., `arr[0]`)
    Index {
        object: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    /// Array literal
    Array { elements: Vec<Expr>, span: Span },
    /// Grouped expression (parentheses)
    Grouped { expr: Box<Expr>, span: Span },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int { span, .. } => *span,
            Expr::Float { span, .. } => *span,
            Expr::String { span, .. } => *span,
            Expr::Bool { span, .. } => *span,
            Expr::Nil { span } => *span,
            Expr::Identifier { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Field { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Array { span, .. } => *span,
            Expr::Grouped { span, .. } => *span,
        }
    }
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    Add,
    Sub,
    Mul,
    Div,
    Mod,

    // Comparison
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,

    // Logical
    And,
    Or,

    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,

    // Range
    Range,     // ..
    RangeIncl, // ...
}

impl BinaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            BinaryOp::Add => "+",
            BinaryOp::Sub => "-",
            BinaryOp::Mul => "*",
            BinaryOp::Div => "/",
            BinaryOp::Mod => "%",
            BinaryOp::Eq => "==",
            BinaryOp::Ne => "!=",
            BinaryOp::Lt => "<",
            BinaryOp::Le => "<=",
            BinaryOp::Gt => ">",
            BinaryOp::Ge => ">=",
            BinaryOp::And => "and",
            BinaryOp::Or => "or",
            BinaryOp::BitAnd => "&",
            BinaryOp::BitOr => "|",
            BinaryOp::BitXor => "^",
            BinaryOp::Shl => "<<",
            BinaryOp::Shr => ">>",
            BinaryOp::Range => "..",
            BinaryOp::RangeIncl => "...",
        }
    }
}

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,    // -
    Not,    // not / !
    BitNot, // ~
}

impl UnaryOp {
    pub fn as_str(&self) -> &'static str {
        match self {
            UnaryOp::Neg => "-",
            UnaryOp::Not => "not",
            UnaryOp::BitNot => "~",
        }
    }
}
