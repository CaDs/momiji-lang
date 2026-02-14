use crate::errors::{ParseError, Span};
use crate::lexer::{Token, TokenKind};
use crate::parser::ast::*;

/// Parser for Momiji source code
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
    eof_token: Token,
}

/// Precedence levels for Pratt parsing
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Precedence {
    None,
    Assignment, // =
    Or,         // or ||
    And,        // and &&
    BitOr,      // |
    BitXor,     // ^
    BitAnd,     // &
    Equality,   // == !=
    Comparison, // < > <= >=
    Range,      // .. ...
    Shift,      // << >>
    Term,       // + -
    Factor,     // * / %
    Unary,      // - not ! ~
    Call,       // . () []
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        let eof_span = tokens.last().map(|t| t.span).unwrap_or(Span::new(0, 0));
        Self {
            tokens,
            current: 0,
            eof_token: Token::new(TokenKind::Eof, eof_span),
        }
    }

    /// Parse the entire program
    pub fn parse(mut self) -> Result<Program, ParseError> {
        let mut items = Vec::new();

        self.skip_newlines();

        while !self.is_at_end() {
            items.push(self.parse_item()?);
            self.skip_newlines();
        }

        Ok(Program { items })
    }

    /// Parse a top-level item
    fn parse_item(&mut self) -> Result<Item, ParseError> {
        match self.peek_kind() {
            TokenKind::Def => Ok(Item::Function(self.parse_function()?)),
            _ => Err(ParseError::UnexpectedToken {
                expected: "function definition".to_string(),
                found: self.peek().kind.description().to_string(),
                span: self.peek().span.into(),
            }),
        }
    }

    /// Parse a function definition
    fn parse_function(&mut self) -> Result<Function, ParseError> {
        let start_span = self.peek().span;
        self.expect(TokenKind::Def)?;

        let name = self.expect_identifier()?;

        // Parse parameters
        let params = if self.check(&TokenKind::LParen) {
            self.advance();
            let params = self.parse_parameters()?;
            self.expect(TokenKind::RParen)?;
            params
        } else {
            Vec::new()
        };

        // Parse return type
        let return_type = if self.check(&TokenKind::Arrow) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.skip_newlines();

        // Parse body
        let body = self.parse_block()?;

        let end_span = self.previous().span;

        // Capture error span before calling expect
        let error_span = self.peek().span;
        if let Err(_) = self.expect(TokenKind::End) {
            return Err(ParseError::ExpectedEnd {
                start_span: start_span.into(),
                span: error_span.into(),
            });
        }

        Ok(Function {
            name,
            params,
            return_type,
            body,
            span: start_span.merge(end_span),
        })
    }

    /// Parse function parameters
    fn parse_parameters(&mut self) -> Result<Vec<Parameter>, ParseError> {
        let mut params = Vec::new();

        if self.check(&TokenKind::RParen) {
            return Ok(params);
        }

        loop {
            let name = self.expect_identifier()?;
            self.expect(TokenKind::Colon)?;
            let ty = self.parse_type()?;

            params.push(Parameter { name, ty });

            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(params)
    }

    /// Parse a type annotation
    fn parse_type(&mut self) -> Result<TypeAnnotation, ParseError> {
        let kind = self.parse_type_kind()?;
        Ok(TypeAnnotation { kind })
    }

    fn parse_type_kind(&mut self) -> Result<TypeKind, ParseError> {
        // Check for array type
        if self.check(&TokenKind::LBracket) {
            self.advance();
            let elem = self.parse_type_kind()?;
            self.expect(TokenKind::RBracket)?;

            let mut kind = TypeKind::Array(Box::new(elem));

            // Check for nullable
            if self.check(&TokenKind::Question) {
                self.advance();
                kind = TypeKind::Nullable(Box::new(kind));
            }

            return Ok(kind);
        }

        // Named type
        let name = self.expect_identifier()?;
        let mut kind = TypeKind::Named(name);

        // Check for nullable
        if self.check(&TokenKind::Question) {
            self.advance();
            kind = TypeKind::Nullable(Box::new(kind));
        }

        Ok(kind)
    }

    /// Parse a block of statements
    fn parse_block(&mut self) -> Result<Block, ParseError> {
        let mut stmts = Vec::new();

        self.skip_newlines();

        while !self.check(&TokenKind::End)
            && !self.check(&TokenKind::Else)
            && !self.check(&TokenKind::Elsif)
            && !self.is_at_end()
        {
            stmts.push(self.parse_statement()?);
            self.skip_newlines();
        }

        Ok(Block { stmts })
    }

    /// Parse a statement
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        match self.peek_kind() {
            TokenKind::Return => self.parse_return(),
            TokenKind::If => self.parse_if(),
            TokenKind::While => self.parse_while(),
            TokenKind::For => self.parse_for(),
            TokenKind::Let => self.parse_let(),
            _ => self.parse_expr_or_assignment(),
        }
    }

    /// Parse a return statement
    fn parse_return(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;
        self.advance(); // consume 'return'

        let value = if self.check(&TokenKind::Newline)
            || self.check(&TokenKind::Eof)
            || self.check(&TokenKind::End)
        {
            None
        } else {
            Some(self.parse_expression()?)
        };

        let span_end = self.previous().span;
        self.consume_statement_end()?;

        Ok(Stmt::Return {
            value,
            span: span_start.merge(span_end),
        })
    }

    /// Parse an if statement
    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;
        self.advance(); // consume 'if'

        let condition = self.parse_expression()?;
        self.skip_newlines();

        let then_block = self.parse_block()?;

        let else_block = if self.check(&TokenKind::Else) {
            self.advance();
            self.skip_newlines();
            Some(self.parse_block()?)
        } else if self.check(&TokenKind::Elsif) {
            // elsif becomes a nested if in the else block
            let elsif_stmt = self.parse_if()?;
            Some(Block {
                stmts: vec![elsif_stmt.clone()],
            })
        } else {
            None
        };

        // Only expect 'end' if we didn't have an elsif
        if !matches!(self.previous().kind, TokenKind::End) {
            self.expect(TokenKind::End)?;
        }

        Ok(Stmt::If {
            condition,
            then_block,
            else_block,
            span: span_start.merge(self.previous().span),
        })
    }

    /// Parse a while loop
    fn parse_while(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;
        self.advance(); // consume 'while'

        let condition = self.parse_expression()?;
        self.skip_newlines();

        let body = self.parse_block()?;
        self.expect(TokenKind::End)?;

        Ok(Stmt::While {
            condition,
            body,
            span: span_start.merge(self.previous().span),
        })
    }

    /// Parse a for loop
    fn parse_for(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;
        self.advance(); // consume 'for'

        let var = self.expect_identifier()?;
        self.expect(TokenKind::In)?;
        let iterable = self.parse_expression()?;
        self.skip_newlines();

        let body = self.parse_block()?;
        self.expect(TokenKind::End)?;

        Ok(Stmt::For {
            var,
            iterable,
            body,
            span: span_start.merge(self.previous().span),
        })
    }

    /// Parse a let statement
    fn parse_let(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;
        self.advance(); // consume 'let'

        // `let mut` is accepted as a compatibility alias.
        if self.check(&TokenKind::Mut) {
            self.advance();
        }

        let name = self.expect_identifier()?;

        let ty = if self.check(&TokenKind::Colon) {
            self.advance();
            Some(self.parse_type()?)
        } else {
            None
        };

        self.expect(TokenKind::Eq)?;
        let value = self.parse_expression()?;
        let span_end = self.previous().span;

        self.consume_statement_end()?;

        Ok(Stmt::Let {
            name,
            ty,
            value,
            implicit: false,
            span: span_start.merge(span_end),
        })
    }

    /// Parse an expression statement or assignment
    fn parse_expr_or_assignment(&mut self) -> Result<Stmt, ParseError> {
        let span_start = self.peek().span;

        // Check for simple identifier assignment: `x = value`
        if let TokenKind::Identifier(name) = self.peek_kind() {
            let name = name.clone();

            // Look ahead to see if this is an assignment
            if self.tokens.get(self.current + 1).map(|t| &t.kind) == Some(&TokenKind::Eq) {
                self.advance(); // consume identifier
                self.advance(); // consume '='

                let value = self.parse_expression()?;
                let span_end = self.previous().span;
                self.consume_statement_end()?;

                return Ok(Stmt::Let {
                    name,
                    ty: None,
                    value,
                    implicit: true,
                    span: span_start.merge(span_end),
                });
            }
        }

        // Regular expression statement
        let expr = self.parse_expression()?;
        self.consume_statement_end()?;

        Ok(Stmt::Expr(expr))
    }

    /// Parse an expression using Pratt parsing
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_precedence(Precedence::Assignment)
    }

    /// Parse expression with given precedence
    fn parse_precedence(&mut self, precedence: Precedence) -> Result<Expr, ParseError> {
        // Parse prefix
        let mut left = self.parse_prefix()?;

        // Parse infix
        while precedence <= self.current_precedence() {
            left = self.parse_infix(left)?;
        }

        Ok(left)
    }

    /// Parse prefix expression
    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        let token_kind = self.peek_kind().clone();
        let token_span = self.peek().span;

        match token_kind {
            TokenKind::Int(n) => {
                self.advance();
                Ok(Expr::Int {
                    value: n,
                    span: token_span,
                })
            }
            TokenKind::Float(n) => {
                self.advance();
                Ok(Expr::Float {
                    value: n,
                    span: token_span,
                })
            }
            TokenKind::String(s) => {
                self.advance();
                Ok(Expr::String {
                    value: s,
                    span: token_span,
                })
            }
            TokenKind::True => {
                self.advance();
                Ok(Expr::Bool {
                    value: true,
                    span: token_span,
                })
            }
            TokenKind::False => {
                self.advance();
                Ok(Expr::Bool {
                    value: false,
                    span: token_span,
                })
            }
            TokenKind::Nil => {
                self.advance();
                Ok(Expr::Nil { span: token_span })
            }
            TokenKind::Identifier(name) => {
                self.advance();
                Ok(Expr::Identifier {
                    name,
                    span: token_span,
                })
            }
            TokenKind::Minus => {
                self.advance();
                let expr = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Neg,
                    span: token_span.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Not | TokenKind::Bang => {
                self.advance();
                let expr = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::Not,
                    span: token_span.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            TokenKind::Tilde => {
                self.advance();
                let expr = self.parse_precedence(Precedence::Unary)?;
                Ok(Expr::Unary {
                    op: UnaryOp::BitNot,
                    span: token_span.merge(expr.span()),
                    expr: Box::new(expr),
                })
            }
            TokenKind::LParen => {
                self.advance();
                let expr = self.parse_expression()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Grouped {
                    span: token_span.merge(self.previous().span),
                    expr: Box::new(expr),
                })
            }
            TokenKind::LBracket => {
                self.advance();
                let mut elements = Vec::new();

                if !self.check(&TokenKind::RBracket) {
                    loop {
                        elements.push(self.parse_expression()?);
                        if !self.match_token(&TokenKind::Comma) {
                            break;
                        }
                    }
                }

                self.expect(TokenKind::RBracket)?;
                Ok(Expr::Array {
                    elements,
                    span: token_span.merge(self.previous().span),
                })
            }
            _ => Err(ParseError::ExpectedExpression {
                span: token_span.into(),
            }),
        }
    }

    /// Parse infix expression
    fn parse_infix(&mut self, left: Expr) -> Result<Expr, ParseError> {
        let token_kind = self.peek_kind().clone();
        let token_span = self.peek().span;

        match token_kind {
            // Binary operators
            TokenKind::Plus => self.parse_binary(left, BinaryOp::Add),
            TokenKind::Minus => self.parse_binary(left, BinaryOp::Sub),
            TokenKind::Star => self.parse_binary(left, BinaryOp::Mul),
            TokenKind::Slash => self.parse_binary(left, BinaryOp::Div),
            TokenKind::Percent => self.parse_binary(left, BinaryOp::Mod),
            TokenKind::EqEq => self.parse_binary(left, BinaryOp::Eq),
            TokenKind::BangEq => self.parse_binary(left, BinaryOp::Ne),
            TokenKind::Lt => self.parse_binary(left, BinaryOp::Lt),
            TokenKind::LtEq => self.parse_binary(left, BinaryOp::Le),
            TokenKind::Gt => self.parse_binary(left, BinaryOp::Gt),
            TokenKind::GtEq => self.parse_binary(left, BinaryOp::Ge),
            TokenKind::And | TokenKind::AmpAmp => self.parse_binary(left, BinaryOp::And),
            TokenKind::Or | TokenKind::PipePipe => self.parse_binary(left, BinaryOp::Or),
            TokenKind::Amp => self.parse_binary(left, BinaryOp::BitAnd),
            TokenKind::Pipe => self.parse_binary(left, BinaryOp::BitOr),
            TokenKind::Caret => self.parse_binary(left, BinaryOp::BitXor),
            TokenKind::LtLt => self.parse_binary(left, BinaryOp::Shl),
            TokenKind::GtGt => self.parse_binary(left, BinaryOp::Shr),
            TokenKind::DotDot => self.parse_binary(left, BinaryOp::Range),
            TokenKind::DotDotDot => self.parse_binary(left, BinaryOp::RangeIncl),

            // Function call
            TokenKind::LParen => {
                self.advance();
                let args = self.parse_arguments()?;
                self.expect(TokenKind::RParen)?;
                Ok(Expr::Call {
                    callee: Box::new(left),
                    args,
                    span: token_span.merge(self.previous().span),
                })
            }

            // Field access
            TokenKind::Dot => {
                self.advance();
                let field = self.expect_identifier()?;
                Ok(Expr::Field {
                    object: Box::new(left),
                    field,
                    span: token_span.merge(self.previous().span),
                })
            }

            // Index access
            TokenKind::LBracket => {
                self.advance();
                let index = self.parse_expression()?;
                self.expect(TokenKind::RBracket)?;
                Ok(Expr::Index {
                    object: Box::new(left),
                    index: Box::new(index),
                    span: token_span.merge(self.previous().span),
                })
            }

            _ => unreachable!("parse_infix called without infix token"),
        }
    }

    fn parse_binary(&mut self, left: Expr, op: BinaryOp) -> Result<Expr, ParseError> {
        let prec = self.current_precedence();
        self.advance();
        let right = self.parse_precedence(prec.next())?;
        Ok(Expr::Binary {
            span: left.span().merge(right.span()),
            left: Box::new(left),
            op,
            right: Box::new(right),
        })
    }

    fn parse_arguments(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args = Vec::new();

        if self.check(&TokenKind::RParen) {
            return Ok(args);
        }

        loop {
            args.push(self.parse_expression()?);
            if !self.match_token(&TokenKind::Comma) {
                break;
            }
        }

        Ok(args)
    }

    fn current_precedence(&self) -> Precedence {
        match self.peek_kind() {
            TokenKind::Or | TokenKind::PipePipe => Precedence::Or,
            TokenKind::And | TokenKind::AmpAmp => Precedence::And,
            TokenKind::Pipe => Precedence::BitOr,
            TokenKind::Caret => Precedence::BitXor,
            TokenKind::Amp => Precedence::BitAnd,
            TokenKind::EqEq | TokenKind::BangEq => Precedence::Equality,
            TokenKind::Lt | TokenKind::LtEq | TokenKind::Gt | TokenKind::GtEq => {
                Precedence::Comparison
            }
            TokenKind::DotDot | TokenKind::DotDotDot => Precedence::Range,
            TokenKind::LtLt | TokenKind::GtGt => Precedence::Shift,
            TokenKind::Plus | TokenKind::Minus => Precedence::Term,
            TokenKind::Star | TokenKind::Slash | TokenKind::Percent => Precedence::Factor,
            TokenKind::LParen | TokenKind::Dot | TokenKind::LBracket => Precedence::Call,
            _ => Precedence::None,
        }
    }

    // Helper methods

    fn peek(&self) -> &Token {
        self.tokens.get(self.current).unwrap_or(&self.eof_token)
    }

    fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current.saturating_sub(1)]
    }

    fn is_at_end(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn check(&self, kind: &TokenKind) -> bool {
        std::mem::discriminant(self.peek_kind()) == std::mem::discriminant(kind)
    }

    fn match_token(&mut self, kind: &TokenKind) -> bool {
        if self.check(kind) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: TokenKind) -> Result<&Token, ParseError> {
        if self.check(&kind) {
            Ok(self.advance())
        } else {
            Err(ParseError::UnexpectedToken {
                expected: kind.description().to_string(),
                found: self.peek_kind().description().to_string(),
                span: self.peek().span.into(),
            })
        }
    }

    fn expect_identifier(&mut self) -> Result<String, ParseError> {
        if let TokenKind::Identifier(name) = self.peek_kind() {
            let name = name.clone();
            self.advance();
            Ok(name)
        } else {
            Err(ParseError::UnexpectedToken {
                expected: "identifier".to_string(),
                found: self.peek_kind().description().to_string(),
                span: self.peek().span.into(),
            })
        }
    }

    fn skip_newlines(&mut self) {
        while self.check(&TokenKind::Newline) {
            self.advance();
        }
    }

    fn consume_statement_end(&mut self) -> Result<(), ParseError> {
        // Consume newline or semicolon at end of statement.
        if self.check(&TokenKind::Newline) || self.check(&TokenKind::Semicolon) {
            self.advance();
            return Ok(());
        }

        // Also allow block or file terminators.
        if self.check(&TokenKind::End)
            || self.check(&TokenKind::Else)
            || self.check(&TokenKind::Elsif)
            || self.check(&TokenKind::Eof)
        {
            return Ok(());
        }

        Err(ParseError::UnexpectedToken {
            expected: "statement terminator (newline, ';', or block end)".to_string(),
            found: self.peek_kind().description().to_string(),
            span: self.peek().span.into(),
        })
    }
}

impl Precedence {
    fn next(self) -> Self {
        match self {
            Precedence::None => Precedence::Assignment,
            Precedence::Assignment => Precedence::Or,
            Precedence::Or => Precedence::And,
            Precedence::And => Precedence::BitOr,
            Precedence::BitOr => Precedence::BitXor,
            Precedence::BitXor => Precedence::BitAnd,
            Precedence::BitAnd => Precedence::Equality,
            Precedence::Equality => Precedence::Comparison,
            Precedence::Comparison => Precedence::Range,
            Precedence::Range => Precedence::Shift,
            Precedence::Shift => Precedence::Term,
            Precedence::Term => Precedence::Factor,
            Precedence::Factor => Precedence::Unary,
            Precedence::Unary => Precedence::Call,
            Precedence::Call => Precedence::Call,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::Scanner;

    fn parse(source: &str) -> Program {
        let tokens = Scanner::new(source).scan_all().unwrap();
        Parser::new(tokens).parse().unwrap()
    }

    #[test]
    fn test_empty_function() {
        let program = parse("def main\nend");
        assert_eq!(program.items.len(), 1);
        let Item::Function(f) = &program.items[0];
        assert_eq!(f.name, "main");
        assert!(f.params.is_empty());
        assert!(f.body.stmts.is_empty());
    }

    #[test]
    fn test_function_with_params() {
        let program = parse("def add(a: Int, b: Int) -> Int\n  return a + b\nend");
        let Item::Function(f) = &program.items[0];
        assert_eq!(f.name, "add");
        assert_eq!(f.params.len(), 2);
        assert_eq!(f.params[0].name, "a");
        assert_eq!(f.params[1].name, "b");
    }

    #[test]
    fn test_arithmetic() {
        let program = parse("def main\n  x = 1 + 2 * 3\nend");
        let Item::Function(f) = &program.items[0];
        assert_eq!(f.body.stmts.len(), 1);
    }

    #[test]
    fn test_function_call() {
        let program = parse("def main\n  puts(\"hello\")\nend");
        let Item::Function(f) = &program.items[0];
        if let Stmt::Expr(Expr::Call { args, .. }) = &f.body.stmts[0] {
            assert_eq!(args.len(), 1);
        } else {
            panic!("Expected call expression");
        }
    }

    #[test]
    fn test_hello_world() {
        let program = parse(
            r#"def main
  puts("Hello, Momiji!")
end"#,
        );
        assert_eq!(program.items.len(), 1);
        let Item::Function(f) = &program.items[0];
        assert_eq!(f.name, "main");
        assert_eq!(f.body.stmts.len(), 1);
    }

    #[test]
    fn test_requires_statement_terminator() {
        let tokens = Scanner::new(
            r#"def main
  puts "Hello, Momiji!"
end"#,
        )
        .scan_all()
        .unwrap();

        let result = Parser::new(tokens).parse();
        assert!(result.is_err());
    }
}
