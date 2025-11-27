use crate::ast::{
    BinOp, EnumDecl, Expr, Function, GlobalDecl, PostOp, Program, Stmt, StructDecl, Type,
    TypedefDecl, UnOp,
};
use crate::error::CompileError;
use crate::lexer::{Token, TokenKind};
use crate::span::Span;
use std::collections::HashSet;

pub fn parse(tokens: &[Token]) -> Result<Program, CompileError> {
    let mut p = Parser {
        tokens,
        pos: 0,
        type_names: HashSet::new(),
    };
    p.parse_program()
}

struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    type_names: HashSet<String>,
}

impl<'a> Parser<'a> {
    fn parse_program(&mut self) -> Result<Program, CompileError> {
        let mut functions = Vec::new();
        let mut structs = Vec::new();
        let mut globals = Vec::new();
        let mut enums = Vec::new();
        let mut typedefs = Vec::new();

        while !self.is_end() {
            if self.peek_is(TokenKind::TypedefKeyword) {
                typedefs.push(self.parse_typedef()?);
                continue;
            }
            if self.peek_is(TokenKind::EnumKeyword) {
                if self.is_enum_definition() {
                    enums.push(self.parse_enum_decl()?);
                    continue;
                }
            }
            if self.peek_is(TokenKind::StructKeyword) && self.is_struct_definition() {
                structs.push(self.parse_struct_decl()?);
                continue;
            }

            let start_pos = self.pos;
            let _ = self.consume_if(&TokenKind::ExternKeyword);
            let _ = self.parse_type();
            let _ = self.consume_if(&TokenKind::Identifier("".to_string()));

            if self.peek_is(TokenKind::LParen) {
                self.pos = start_pos;
                functions.push(self.parse_function()?);
            } else {
                self.pos = start_pos;
                globals.push(self.parse_global()?);
            }
        }
        Ok(Program {
            structs,
            globals,
            functions,
            enums,
            typedefs,
        })
    }

    fn is_struct_definition(&self) -> bool {
        if self.pos + 2 < self.tokens.len() {
            if let TokenKind::Identifier(_) = self.tokens[self.pos + 1].kind {
                if self.tokens[self.pos + 2].kind == TokenKind::LBrace {
                    return true;
                }
            }
        }
        false
    }

    fn is_enum_definition(&self) -> bool {
        if self.pos + 2 < self.tokens.len() {
            if let TokenKind::Identifier(_) = self.tokens[self.pos + 1].kind {
                if self.tokens[self.pos + 2].kind == TokenKind::LBrace {
                    return true;
                }
            }
        }
        false
    }

    fn parse_typedef(&mut self) -> Result<TypedefDecl, CompileError> {
        let start = self.expect(&TokenKind::TypedefKeyword)?.span.start;
        let ty = self.parse_type()?;
        let new_name = self.expect_identifier()?;
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        self.type_names.insert(new_name.clone());
        Ok(TypedefDecl {
            new_name,
            ty,
            span: Span::new(start, end),
        })
    }

    fn parse_enum_decl(&mut self) -> Result<EnumDecl, CompileError> {
        let start = self.expect(&TokenKind::EnumKeyword)?.span.start;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;
        let mut variants = Vec::new();
        let mut val = 0;
        while !self.peek_is(TokenKind::RBrace) {
            let var_name = self.expect_identifier()?;
            if self.consume_if(&TokenKind::Assign) {
                if let TokenKind::Number(n) = self.peek_kind().unwrap() {
                    self.bump();
                    val = n;
                } else {
                    return Err(CompileError::new("Expected number in enum"));
                }
            }
            variants.push((var_name, val));
            val += 1;
            if !self.consume_if(&TokenKind::Comma) {
                break;
            }
        }
        self.expect(&TokenKind::RBrace)?;
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        Ok(EnumDecl {
            name,
            variants,
            span: Span::new(start, end),
        })
    }

    fn parse_struct_decl(&mut self) -> Result<StructDecl, CompileError> {
        let start = self.expect(&TokenKind::StructKeyword)?.span.start;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LBrace)?;
        let mut fields = Vec::new();
        while !self.peek_is(TokenKind::RBrace) {
            let ty = self.parse_type()?;
            let field_name = self.expect_identifier()?;
            self.expect(&TokenKind::Semicolon)?;
            fields.push((field_name, ty));
        }
        self.expect(&TokenKind::RBrace)?;
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        Ok(StructDecl {
            name,
            fields,
            span: Span::new(start, end),
        })
    }

    fn parse_global(&mut self) -> Result<GlobalDecl, CompileError> {
        let _ = self.consume_if(&TokenKind::ExternKeyword);
        let start = self.peek_span().start;
        let ty = self.parse_type()?;
        let name = self.expect_identifier()?;
        let mut init = None;
        if self.consume_if(&TokenKind::Assign) {
            init = Some(self.parse_expr()?);
        }
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        Ok(GlobalDecl {
            name,
            ty,
            init,
            span: Span::new(start, end),
        })
    }

    fn parse_function(&mut self) -> Result<Function, CompileError> {
        let is_extern = self.consume_if(&TokenKind::ExternKeyword);
        let start_span = self.peek_span();
        let _ret_ty = self.parse_type()?;
        let name = self.expect_identifier()?;
        self.expect(&TokenKind::LParen)?;
        let mut params = Vec::new();
        if !self.peek_is(TokenKind::RParen) {
            loop {
                if self.consume_if(&TokenKind::Ellipsis) {
                    break;
                }
                let ty = self.parse_type()?;
                let param_name = self.expect_identifier()?;
                let final_ty = if self.consume_if(&TokenKind::LBracket) {
                    self.consume_if(&TokenKind::RBracket);
                    Type::Pointer(Box::new(ty))
                } else {
                    ty
                };
                params.push((param_name, final_ty));
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
        }
        self.expect(&TokenKind::RParen)?;
        if is_extern {
            let end = self.expect(&TokenKind::Semicolon)?.span.end;
            Ok(Function {
                name,
                params,
                body: None,
                span: Span::new(start_span.start, end),
            })
        } else {
            let body = self.parse_block()?;
            let end = body_span(&body).end;
            Ok(Function {
                name,
                params,
                span: Span::new(start_span.start, end),
                body: Some(body),
            })
        }
    }

    fn parse_type(&mut self) -> Result<Type, CompileError> {
        let mut ty = if self.consume_if(&TokenKind::IntKeyword) {
            Type::Int
        } else if self.consume_if(&TokenKind::CharKeyword) {
            Type::Char
        } else if self.consume_if(&TokenKind::VoidKeyword) {
            Type::Void
        } else if self.consume_if(&TokenKind::StructKeyword) {
            let name = self.expect_identifier()?;
            Type::Struct(name)
        } else if let Some(TokenKind::Identifier(name)) = self.peek_kind() {
            if self.type_names.contains(&name) {
                self.bump();
                Type::Alias(name)
            } else {
                return Err(CompileError::new_with_span(
                    "Expected type",
                    self.peek_span(),
                ));
            }
        } else {
            return Err(CompileError::new_with_span(
                "Expected type",
                self.peek_span(),
            ));
        };
        while self.consume_if(&TokenKind::Star) {
            ty = Type::Pointer(Box::new(ty));
        }
        Ok(ty)
    }

    fn parse_block(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::LBrace)?.span.start;
        let mut stmts = Vec::new();
        while !self.peek_is(TokenKind::RBrace) && !self.is_end() {
            stmts.push(self.parse_stmt()?);
        }
        let end = self.expect(&TokenKind::RBrace)?.span.end;
        Ok(Stmt::Block(stmts, Span::new(start, end)))
    }

    fn parse_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.peek_span();
        match self.peek_kind() {
            Some(TokenKind::ReturnKeyword) => {
                self.bump();
                let expr = if self.peek_is(TokenKind::Semicolon) {
                    None
                } else {
                    Some(self.parse_expr()?)
                };
                let end = self.expect(&TokenKind::Semicolon)?.span.end;
                Ok(Stmt::Return(expr, Span::new(start.start, end)))
            }
            Some(TokenKind::IfKeyword) => self.parse_if_stmt(),
            Some(TokenKind::WhileKeyword) => self.parse_while_stmt(),
            Some(TokenKind::DoKeyword) => self.parse_do_while_stmt(),
            Some(TokenKind::ForKeyword) => self.parse_for_stmt(),
            Some(TokenKind::SwitchKeyword) => self.parse_switch_stmt(),
            Some(TokenKind::LBrace) => self.parse_block(),
            Some(TokenKind::BreakKeyword) => {
                self.bump();
                let end = self.expect(&TokenKind::Semicolon)?.span.end;
                Ok(Stmt::Break(Span::new(start.start, end)))
            }
            Some(TokenKind::ContinueKeyword) => {
                self.bump();
                let end = self.expect(&TokenKind::Semicolon)?.span.end;
                Ok(Stmt::Continue(Span::new(start.start, end)))
            }
            _ => {
                let is_decl = self.is_declaration_start();
                if is_decl {
                    self.parse_declaration()
                } else {
                    let expr = self.parse_expr()?;
                    let _ = self.expect(&TokenKind::Semicolon)?.span.end;
                    Ok(Stmt::Expr(expr))
                }
            }
        }
    }

    fn is_declaration_start(&self) -> bool {
        match self.peek_kind() {
            Some(TokenKind::IntKeyword)
            | Some(TokenKind::CharKeyword)
            | Some(TokenKind::VoidKeyword)
            | Some(TokenKind::StructKeyword) => true,
            Some(TokenKind::Identifier(name)) => self.type_names.contains(&name),
            _ => false,
        }
    }

    fn parse_declaration(&mut self) -> Result<Stmt, CompileError> {
        let start = self.peek_span().start;
        let mut ty = self.parse_type()?;
        let name = self.expect_identifier()?;
        if self.consume_if(&TokenKind::LBracket) {
            if self.peek_is(TokenKind::RBracket) {
                self.bump();
                ty = Type::Array(Box::new(ty), 0);
            } else if let TokenKind::Number(n) = self.peek_kind().unwrap() {
                self.bump();
                self.expect(&TokenKind::RBracket)?;
                ty = Type::Array(Box::new(ty), n as usize);
            } else {
                return Err(CompileError::new("Expected array size"));
            }
        }
        let initializer = if self.consume_if(&TokenKind::Assign) {
            Some(self.parse_expr()?)
        } else {
            None
        };
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        Ok(Stmt::Declare {
            name,
            ty,
            initializer,
            span: Span::new(start, end),
        })
    }

    fn parse_for_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::ForKeyword)?.span.start;
        self.expect(&TokenKind::LParen)?;
        let init = if self.consume_if(&TokenKind::Semicolon) {
            None
        } else {
            if self.is_declaration_start() {
                Some(Box::new(self.parse_declaration()?))
            } else {
                let e = self.parse_expr()?;
                self.expect(&TokenKind::Semicolon)?;
                Some(Box::new(Stmt::Expr(e)))
            }
        };
        let condition = if self.peek_is(TokenKind::Semicolon) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(&TokenKind::Semicolon)?;
        let step = if self.peek_is(TokenKind::RParen) {
            None
        } else {
            Some(self.parse_expr()?)
        };
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_stmt()?);
        let end = body_span(&body).end;
        Ok(Stmt::For {
            init,
            condition,
            step,
            body,
            span: Span::new(start, end),
        })
    }
    fn parse_do_while_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::DoKeyword)?.span.start;
        let body = Box::new(self.parse_stmt()?);
        self.expect(&TokenKind::WhileKeyword)?;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        let end = self.expect(&TokenKind::Semicolon)?.span.end;
        Ok(Stmt::DoWhile {
            body,
            condition,
            span: Span::new(start, end),
        })
    }
    fn parse_switch_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::SwitchKeyword)?.span.start;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        self.expect(&TokenKind::LBrace)?;
        let mut cases = Vec::new();
        let mut default = None;
        while !self.peek_is(TokenKind::RBrace) && !self.is_end() {
            if self.consume_if(&TokenKind::CaseKeyword) {
                if let TokenKind::Number(val) = self.peek_kind().unwrap() {
                    self.bump();
                    self.expect(&TokenKind::Colon)?;
                    let mut stmts = Vec::new();
                    while !matches!(
                        self.peek_kind(),
                        Some(TokenKind::CaseKeyword)
                            | Some(TokenKind::DefaultKeyword)
                            | Some(TokenKind::RBrace)
                    ) {
                        stmts.push(self.parse_stmt()?);
                    }
                    cases.push((val, stmts));
                } else {
                    return Err(CompileError::new("Expected const case"));
                }
            } else if self.consume_if(&TokenKind::DefaultKeyword) {
                self.expect(&TokenKind::Colon)?;
                let mut stmts = Vec::new();
                while !matches!(
                    self.peek_kind(),
                    Some(TokenKind::CaseKeyword)
                        | Some(TokenKind::DefaultKeyword)
                        | Some(TokenKind::RBrace)
                ) {
                    stmts.push(self.parse_stmt()?);
                }
                default = Some(stmts);
            } else {
                return Err(CompileError::new("Expected case/default"));
            }
        }
        let end = self.expect(&TokenKind::RBrace)?.span.end;
        Ok(Stmt::Switch {
            condition,
            cases,
            default,
            span: Span::new(start, end),
        })
    }
    fn parse_if_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::IfKeyword)?.span.start;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        let then_branch = Box::new(self.parse_stmt()?);
        let else_branch = if self.consume_if(&TokenKind::ElseKeyword) {
            Some(Box::new(self.parse_stmt()?))
        } else {
            None
        };
        let end = else_branch
            .as_ref()
            .map(|b| body_span(b).end)
            .unwrap_or(body_span(&then_branch).end);
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
            span: Span::new(start, end),
        })
    }
    fn parse_while_stmt(&mut self) -> Result<Stmt, CompileError> {
        let start = self.expect(&TokenKind::WhileKeyword)?.span.start;
        self.expect(&TokenKind::LParen)?;
        let condition = self.parse_expr()?;
        self.expect(&TokenKind::RParen)?;
        let body = Box::new(self.parse_stmt()?);
        let end = body_span(&body).end;
        Ok(Stmt::While {
            condition,
            body,
            span: Span::new(start, end),
        })
    }

    fn parse_expr(&mut self) -> Result<Expr, CompileError> {
        self.parse_assignment()
    }
    fn parse_assignment(&mut self) -> Result<Expr, CompileError> {
        let left = self.parse_logical_or()?;
        if self.consume_if(&TokenKind::Assign) {
            let right = self.parse_assignment()?;
            let span = Span::new(expr_span(&left).start, expr_span(&right).end);
            match left {
                Expr::Variable { .. }
                | Expr::Deref { .. }
                | Expr::Index { .. }
                | Expr::Member { .. } => {
                    return Ok(Expr::Assign {
                        target: Box::new(left),
                        value: Box::new(right),
                        span,
                    });
                }
                _ => {
                    return Err(CompileError::new_with_span(
                        "Invalid assignment target",
                        expr_span(&left),
                    ));
                }
            }
        }
        Ok(left)
    }

    fn parse_logical_or(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_logical_and()?;
        while self.consume_if(&TokenKind::PipePipe) {
            let right = self.parse_logical_and()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op: BinOp::LogicOr,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }
    fn parse_logical_and(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_bitwise_or()?;
        while self.consume_if(&TokenKind::AmpAmp) {
            let right = self.parse_bitwise_or()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op: BinOp::LogicAnd,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }
    fn parse_bitwise_or(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_bitwise_xor()?;
        while self.consume_if(&TokenKind::Pipe) {
            let right = self.parse_bitwise_xor()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op: BinOp::BitOr,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }
    fn parse_bitwise_xor(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_bitwise_and()?;
        while self.consume_if(&TokenKind::Caret) {
            let right = self.parse_bitwise_and()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op: BinOp::BitXor,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }
    fn parse_bitwise_and(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_equality()?;
        while self.consume_if(&TokenKind::Ampersand) {
            let right = self.parse_equality()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op: BinOp::BitAnd,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
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
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
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
        let mut node = self.parse_shift()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::LessThan) => BinOp::Lt,
                Some(TokenKind::LessThanEqual) => BinOp::LtEq,
                Some(TokenKind::GreaterThan) => BinOp::Gt,
                Some(TokenKind::GreaterThanEqual) => BinOp::GtEq,
                _ => break,
            };
            self.bump();
            let right = self.parse_shift()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
            node = Expr::Binary {
                op,
                left: Box::new(node),
                right: Box::new(right),
                span,
            };
        }
        Ok(node)
    }
    fn parse_shift(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_add()?;
        loop {
            let op = match self.peek_kind() {
                Some(TokenKind::LessLess) => BinOp::Shl,
                Some(TokenKind::GreaterGreater) => BinOp::Shr,
                _ => break,
            };
            self.bump();
            let right = self.parse_add()?;
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
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
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
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
            let span = Span::new(expr_span(&node).start, expr_span(&right).end);
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
        let start_span = self.peek_span();

        if self.consume_if(&TokenKind::Star) {
            let expr = self.parse_unary()?;
            let end = expr_span(&expr).end;
            return Ok(Expr::Deref {
                span: Span::new(start_span.start, end),
                expr: Box::new(expr),
            });
        }

        if self.consume_if(&TokenKind::Ampersand) {
            let expr = self.parse_unary()?;
            let end = expr_span(&expr).end;
            return Ok(Expr::AddrOf {
                span: Span::new(start_span.start, end),
                expr: Box::new(expr),
            });
        }

        if self.peek_is(TokenKind::Minus) {
            let op_tok = self.bump().unwrap();
            let expr = self.parse_unary()?;
            let end = expr_span(&expr).end;
            return Ok(Expr::Unary {
                op: UnOp::Neg,
                expr: Box::new(expr),
                span: Span::new(op_tok.span.start, end),
            });
        }

        if self.peek_is(TokenKind::Bang) {
            let op_tok = self.bump().unwrap();
            let expr = self.parse_unary()?;
            let end = expr_span(&expr).end;
            return Ok(Expr::Unary {
                op: UnOp::LogicNot,
                expr: Box::new(expr),
                span: Span::new(op_tok.span.start, end),
            });
        }

        if self.peek_is(TokenKind::Tilde) {
            let op_tok = self.bump().unwrap();
            let expr = self.parse_unary()?;
            let end = expr_span(&expr).end;
            return Ok(Expr::Unary {
                op: UnOp::BitNot,
                expr: Box::new(expr),
                span: Span::new(op_tok.span.start, end),
            });
        }

        if self.peek_is(TokenKind::LParen) {
            let is_cast = if self.pos + 1 < self.tokens.len() {
                match self.tokens[self.pos + 1].kind {
                    TokenKind::IntKeyword
                    | TokenKind::CharKeyword
                    | TokenKind::VoidKeyword
                    | TokenKind::StructKeyword => true,
                    TokenKind::Identifier(ref name) => self.type_names.contains(name),
                    _ => false,
                }
            } else {
                false
            };
            if is_cast {
                self.bump();
                let ty = self.parse_type()?;
                self.expect(&TokenKind::RParen)?;
                let expr = self.parse_unary()?;
                let end = expr_span(&expr).end;
                return Ok(Expr::Cast {
                    ty,
                    expr: Box::new(expr),
                    span: Span::new(start_span.start, end),
                });
            }
        }

        if self.consume_if(&TokenKind::SizeofKeyword) {
            if self.consume_if(&TokenKind::LParen) {
                let is_type = match self.peek_kind() {
                    Some(TokenKind::IntKeyword)
                    | Some(TokenKind::CharKeyword)
                    | Some(TokenKind::VoidKeyword)
                    | Some(TokenKind::StructKeyword) => true,
                    Some(TokenKind::Identifier(ref name)) => self.type_names.contains(name),
                    _ => false,
                };
                if is_type {
                    let ty = self.parse_type()?;
                    let end_tok = self.expect(&TokenKind::RParen)?;
                    return Ok(Expr::SizeOfType {
                        ty,
                        span: Span::new(start_span.start, end_tok.span.end),
                    });
                } else {
                    let expr = self.parse_expr()?;
                    let end_tok = self.expect(&TokenKind::RParen)?;
                    return Ok(Expr::SizeOfExpr {
                        expr: Box::new(expr),
                        span: Span::new(start_span.start, end_tok.span.end),
                    });
                }
            } else {
                let expr = self.parse_unary()?;
                let end = expr_span(&expr).end;
                return Ok(Expr::SizeOfExpr {
                    expr: Box::new(expr),
                    span: Span::new(start_span.start, end),
                });
            }
        }

        self.parse_postfix()
    }

    fn parse_postfix(&mut self) -> Result<Expr, CompileError> {
        let mut node = self.parse_primary()?;
        loop {
            let start = expr_span(&node).start;
            if self.peek_is(TokenKind::LBracket) {
                self.bump();
                let index = self.parse_expr()?;
                let end = self.expect(&TokenKind::RBracket)?.span.end;
                node = Expr::Index {
                    base: Box::new(node),
                    index: Box::new(index),
                    span: Span::new(start, end),
                };
            } else if self.consume_if(&TokenKind::Dot) {
                let field = self.expect_identifier()?;
                let end = self.tokens[self.pos - 1].span.end;
                node = Expr::Member {
                    expr: Box::new(node),
                    field,
                    is_arrow: false,
                    span: Span::new(start, end),
                };
            } else if self.consume_if(&TokenKind::Arrow) {
                let field = self.expect_identifier()?;
                let end = self.tokens[self.pos - 1].span.end;
                node = Expr::Member {
                    expr: Box::new(node),
                    field,
                    is_arrow: true,
                    span: Span::new(start, end),
                };
            } else if self.consume_if(&TokenKind::Increment) {
                let end = self.tokens[self.pos - 1].span.end;
                node = Expr::PostUnary {
                    op: PostOp::Inc,
                    expr: Box::new(node),
                    span: Span::new(start, end),
                };
            } else if self.consume_if(&TokenKind::Decrement) {
                let end = self.tokens[self.pos - 1].span.end;
                node = Expr::PostUnary {
                    op: PostOp::Dec,
                    expr: Box::new(node),
                    span: Span::new(start, end),
                };
            } else {
                break;
            }
        }
        Ok(node)
    }

    fn parse_primary(&mut self) -> Result<Expr, CompileError> {
        if self.peek_is(TokenKind::LBrace) {
            let start = self.bump().unwrap().span.start;
            let mut values = Vec::new();
            while !self.peek_is(TokenKind::RBrace) {
                values.push(self.parse_expr()?);
                if !self.consume_if(&TokenKind::Comma) {
                    break;
                }
            }
            let end = self.expect(&TokenKind::RBrace)?.span.end;
            return Ok(Expr::InitList {
                values,
                span: Span::new(start, end),
            });
        }

        let tok = self
            .bump()
            .ok_or_else(|| CompileError::new("Unexpected EOF"))?;
        match &tok.kind {
            TokenKind::Number(v) => Ok(Expr::Number {
                value: *v,
                span: tok.span,
            }),
            TokenKind::StringLiteral(s) => Ok(Expr::StringLiteral {
                value: s.clone(),
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
                format!("Expected expression, found {:?}", tok.kind),
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
        Ok(Expr::Call {
            name,
            args,
            span: Span::new(start_span.start, end_tok.span.end),
        })
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
    fn peek_span(&self) -> Span {
        self.peek().map(|t| t.span).unwrap_or(Span::new(0, 0))
    }
    fn peek_is(&self, kind: TokenKind) -> bool {
        self.peek_kind().map_or(false, |k| {
            std::mem::discriminant(&k) == std::mem::discriminant(&kind)
        })
    }
    fn bump(&mut self) -> Option<&'a Token> {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
        self.tokens.get(self.pos - 1)
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
        let tok = self
            .peek()
            .ok_or_else(|| CompileError::new(format!("Expected {:?}, found EOF", kind)))?;
        if std::mem::discriminant(&tok.kind) == std::mem::discriminant(kind) {
            Ok(self.bump().unwrap())
        } else {
            Err(CompileError::new_with_span(
                format!("Expected {:?}, found {:?}", kind, tok.kind),
                tok.span,
            ))
        }
    }
    fn expect_identifier(&mut self) -> Result<String, CompileError> {
        let tok = self
            .bump()
            .ok_or_else(|| CompileError::new("Expected identifier"))?;
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
        | Expr::StringLiteral { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Unary { span, .. }
        | Expr::Variable { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Call { span, .. }
        | Expr::Deref { span, .. }
        | Expr::AddrOf { span, .. }
        | Expr::Index { span, .. }
        | Expr::Member { span, .. }
        | Expr::PostUnary { span, .. }
        | Expr::SizeOfType { span, .. }
        | Expr::SizeOfExpr { span, .. }
        | Expr::Cast { span, .. }
        | Expr::InitList { span, .. } => *span,
    }
}
fn body_span(s: &Stmt) -> Span {
    match s {
        Stmt::Return(_, s)
        | Stmt::If { span: s, .. }
        | Stmt::While { span: s, .. }
        | Stmt::For { span: s, .. }
        | Stmt::Block(_, s)
        | Stmt::Declare { span: s, .. }
        | Stmt::Break(s)
        | Stmt::Continue(s)
        | Stmt::Switch { span: s, .. }
        | Stmt::DoWhile { span: s, .. } => *s,
        Stmt::Expr(e) => expr_span(e),
    }
}
