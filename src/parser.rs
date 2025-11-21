use crate::ast::{BinOp, Expr, Function, Program, Stmt, UnOp};
use crate::error::CompileError;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;

// The top-level parse function now returns a Program
pub fn parse(tokens: &[Token]) -> Result<Program, CompileError> {
    let mut p = Parser { tokens, pos: 0 };
    p.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
}

impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> Result<Program, CompileError> {
        let mut functions = Vec::new();
        while !self.is_end() {
            functions.push(self.parse_function()?);
        }
        Ok(Program { functions })
    }

    fn parse_function(&mut self) -> Result<Function, CompileError> {
        self.expect(&TokenKind::IntKeyword)?;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LParen)?;

        let mut params = Vec::new();
        if self.peek_is(TokenKind::IntKeyword) {
            loop {
                self.expect(&TokenKind::IntKeyword)?;
                params.push(self.expect_identifier()?);
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
        }

        self.expect(&TokenKind::RParen)?;
        let body = self.parse_block()?;

        Ok(Function { name, params, body })
    }

    fn parse_stmt(&mut self) -> Result<Stmt, CompileError> {
        match self.peek_kind() {
            Some(TokenKind::ReturnKeyword) => {
                self.bump(); // consume "return"
                let expr = self.parse_expr()?;
                self.expect(&TokenKind::Semicolon)?;
                Ok(Stmt::Return(expr))
            }
            Some(TokenKind::IfKeyword) => self.parse_if_stmt(),
            Some(TokenKind::WhileKeyword) => self.parse_while_stmt(),
            Some(TokenKind::LBrace) => self.parse_block(),
            Some(TokenKind::IntKeyword) => self.parse_declaration(),
            _ => {
                let expr = self.parse_expr()?;
                self.expect(&TokenKind::Semicolon)?;
                Ok(Stmt::Expr(expr))
            }
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&TokenKind::IntKeyword)?;
        let name = self.expect_identifier()?;

        let initializer = if self.consume_if(&TokenKind::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };

        self.expect(&TokenKind::Semicolon)?;
        Ok(Stmt::Declare { name, initializer })
    }

    fn parse_block(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&TokenKind::LBrace)?;
        let mut stmts = Vec::new();
        while !self.peek_is(TokenKind::RBrace) && !self.is_end() {
            stmts.push(self.parse_stmt()?);
        }
        self.expect(&TokenKind::RBrace)?;
        Ok(Stmt::Block(stmts))
    }

    fn parse_if_stmt(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&TokenKind::IfKeyword)?;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        let then_branch = Box::new(self.parse_stmt()?);
        let else_branch = if self.consume_if(&TokenKind::ElseKeyword) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    fn parse_while_stmt(&mut self) -> Result<Stmt, CompileError> {
        self.expect(&TokenKind::WhileKeyword)?;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_stmt()?);
        Ok(Stmt::While { condition, body })
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, CompileError> {
        let left = self.parse_equality()?;
        if self.consume_if(&TokenKind::Assign) {
            let right = self.parse_assignment()?;
            if let Expr::Variable { name, span } = left {
                let new_span = merge_spans(span, expr_span(&right));
                return Ok(Expr::Assign {
                    name,
                    value: Box::new(right),
                    span: new_span,
                });
            }
            let span = if let Some(tok) = self.peek() {
                tok.span
            } else {
                expr_span(&left)
            };
            return Err(CompileError::new_with_span(
                "Invalid assignment target",
                span,
            ));
        }
        Ok(left)
    }

    fn parse_equality(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_relational()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::EqualTo) => BinOp::Eq,
                Some(TokenKind::NotEqual) => BinOp::NotEq,
                _ => break,
            };
            self.bump();
            let right = self.parse_relational()?;
            let span = merge_spans(expr_span(&node), expr_span(&right));
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }

    fn parse_relational(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_add()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::LessThan) => BinOp::Lt,
                Some(TokenKind::LessThanEqual) => BinOp::LtEq,
                Some(TokenKind::GreaterThan) => BinOp::Gt,
                Some(TokenKind::GreaterThanEqual) => BinOp::GtEq,
                _ => break,
            };
            self.bump();
            let right = self.parse_add()?;
            let span = merge_spans(expr_span(&node), expr_span(&right));
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }

    fn parse_add(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_mul()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::Plus) => BinOp::Add,
                Some(TokenKind::Minus) => BinOp::Sub,
                _ => break,
            };
            self.bump();
            let right = self.parse_mul()?;
            let span = merge_spans(expr_span(&node), expr_span(&right));
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }

    fn parse_mul(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_unary()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::Star) => BinOp::Mul,
                Some(TokenKind::Slash) => BinOp::Div,
                _ => break,
            };
            self.bump();
            let right = self.parse_unary()?;
            let span = merge_spans(expr_span(&node), expr_span(&right));
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }

    fn parse_unary(&mut self) -> Result<Expr, CompileError> {
        if self.peek_is(TokenKind::Minus) {
            let op_tok = self.bump().unwrap();
            let expr = self.parse_unary()?;
            let span = merge_spans(op_tok.span, expr_span(&expr));
            return Ok(Expr::Unary {
                op: UnOp::Neg,
                expr: Box::new(expr),
                span,
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        let tok = self
            .bump()
            .ok_or_else(|| CompileError::new("Unexpected end of input"))?;
        match &tok.kind {
            TokenKind::Number(v) => Ok(Expr::Number {
                value: *v,
                span: tok.span,
            }),
            TokenKind::Identifier(name) => {
                if self.peek_is(TokenKind::LParen) {
                    self.parse_call(name.clone(), tok.span)
                } else {
                    Ok(Expr::Variable {
                        name: name.clone(),
                        span: tok.span,
                    })
                }
            }
            TokenKind::LParen => {
                let expr = self.parse_expr()?;
                self.expect(&TokenKind::RParen)?;
                Ok(expr)
            }
            _ => Err(CompileError::new_with_span(
                format!("Expected primary expression, found {:?}", tok.kind),
                tok.span,
            )),
        }
    }

    fn parse_call(&mut self, name: String, start_span: Span) -> Result<Expr, CompileError> {
        self.expect(&TokenKind::LParen)?;
        let mut args = Vec::new();
        if !self.peek_is(TokenKind::RParen) {
            loop {
                args.push(self.parse_expr()?);
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
        }
        let end_tok = self.expect(&TokenKind::RParen)?;
        let span = merge_spans(start_span, end_tok.span);
        Ok(Expr::Call { name, args, span })
    }

    fn is_end(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    fn peek(&self) -> Option<&'a Token> {
        self.tokens.get(self.pos)
    }

    fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind.clone())
    }

    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek_kind().map_or(false, |k| {
            std::mem::discriminant(&k) == std::mem::discriminant(&kind)
        })
    }

    fn bump(&mut self) -> Option<&'a Token> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    fn consume_if(&mut self, kind: &TokenKind) -> bool {
        if self.peek_is(kind.clone()) {
            self.bump();
            true
        } else {
            false
        }
    }

    fn expect(&mut self, kind: &TokenKind) -> Result<&'a Token, CompileError> {
        let next_token = self.peek().ok_or_else(|| {
            let msg = format!("Expected {kind:?}, but found end of input");
            // Try to get the span of the last token for a better location
            if let Some(last_tok) = self.tokens.last() {
                CompileError::new_with_span(msg, last_tok.span)
            } else {
                CompileError::new(msg)
            }
        })?;

        if std::mem::discriminant(&next_token.kind) == std::mem::discriminant(kind) {
            Ok(self.bump().unwrap())
        } else {
            Err(CompileError::new_with_span(
                format!("Expected {kind:?}, found {:?}", next_token.kind),
                next_token.span,
            ))
        }
    }

    fn expect_identifier(&mut self) -> Result<String, CompileError> {
        let tok = self
            .bump()
            .ok_or_else(|| CompileError::new("Expected identifier, found end of input"))?;

        if let TokenKind::Identifier(name) = &tok.kind {
            Ok(name.clone())
        } else {
            Err(CompileError::new_with_span(
                format!("Expected identifier, found {:?}", tok.kind),
                tok.span,
            ))
        }
    }
}

fn expr_span(e: &Expr) -> Span {
    match e {
        Expr::Number { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Unary { span, .. }
        | Expr::Variable { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Call { span, .. } => *span,
    }
}

fn merge_spans(a: Span, b: Span) -> Span {
    Span {
        start: a.start.min(b.start),
        end: a.end.max(b.end),
    }
}
