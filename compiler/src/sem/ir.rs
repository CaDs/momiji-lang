use crate::errors::Span;
use crate::parser::ast::{BinaryOp, UnaryOp};
use crate::types::Type;

#[derive(Debug, Clone)]
pub struct SemProgram {
    pub functions: Vec<SemFunction>,
}

#[derive(Debug, Clone)]
pub struct SemFunction {
    pub name: String,
    pub params: Vec<SemParam>,
    pub return_type: Type,
    pub body: SemBlock,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct SemParam {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug, Clone)]
pub struct SemBlock {
    pub stmts: Vec<SemStmt>,
}

#[derive(Debug, Clone)]
pub enum SemStmt {
    Let {
        name: String,
        ty: Type,
        value: SemExpr,
        span: Span,
    },
    Assign {
        name: String,
        value: SemExpr,
        span: Span,
    },
    Expr(SemExpr),
    Return {
        value: Option<SemExpr>,
        span: Span,
    },
    If {
        condition: SemExpr,
        then_block: SemBlock,
        else_block: Option<SemBlock>,
        span: Span,
    },
    While {
        condition: SemExpr,
        body: SemBlock,
        span: Span,
    },
    For {
        var: String,
        elem_type: Type,
        iterable: SemExpr,
        body: SemBlock,
        span: Span,
    },
}

#[derive(Debug, Clone)]
pub struct SemExpr {
    pub kind: SemExprKind,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum SemExprKind {
    Int(i64),
    Float(f64),
    String(String),
    Bool(bool),
    Nil,
    Variable(String),
    Binary {
        left: Box<SemExpr>,
        op: BinaryOp,
        right: Box<SemExpr>,
    },
    Unary {
        op: UnaryOp,
        expr: Box<SemExpr>,
    },
    Call {
        name: String,
        args: Vec<SemExpr>,
    },
    Index {
        object: Box<SemExpr>,
        index: Box<SemExpr>,
    },
    Array(Vec<SemExpr>),
}
