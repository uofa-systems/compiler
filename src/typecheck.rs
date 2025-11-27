use crate::ast::{Expr, Function, Program, Stmt, Type};
use crate::error::CompileError;
use crate::span::Span;
use std::collections::HashMap;

pub fn typecheck(program: &Program) -> Result<Program, CompileError> {
    let mut checker = SemanticAnalyzer::new();
    checker.analyze_program(program)?;
    Ok(program.clone())
}

struct SemanticAnalyzer {
    scopes: Vec<HashMap<String, Type>>,
    globals: HashMap<String, Type>,
    functions: HashMap<String, usize>,
    structs: HashMap<String, Span>,
    enums: HashMap<String, i64>, // Enum variant -> value
    typedefs: HashMap<String, Type>,
    in_loop: bool,
}

impl SemanticAnalyzer {
    fn new() -> Self {
        Self {
            scopes: Vec::new(),
            globals: HashMap::new(),
            functions: HashMap::new(),
            structs: HashMap::new(),
            enums: HashMap::new(),
            typedefs: HashMap::new(),
            in_loop: false,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(HashMap::new());
    }
    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare_variable(&mut self, name: &str, ty: Type, span: Span) -> Result<(), CompileError> {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(name) {
                return Err(CompileError::new_with_span(
                    format!("Redeclaration of variable '{}'", name),
                    span,
                ));
            }
            scope.insert(name.to_string(), ty);
        }
        Ok(())
    }

    fn resolve_variable(&self, name: &str, span: Span) -> Result<(), CompileError> {
        for scope in self.scopes.iter().rev() {
            if scope.contains_key(name) {
                return Ok(());
            }
        }
        if self.globals.contains_key(name) {
            return Ok(());
        }
        if self.enums.contains_key(name) {
            return Ok(());
        }
        Err(CompileError::new_with_span(
            format!("Undefined variable '{}'", name),
            span,
        ))
    }

    fn analyze_program(&mut self, program: &Program) -> Result<(), CompileError> {
        for t in &program.typedefs {
            self.typedefs.insert(t.new_name.clone(), t.ty.clone());
        }
        for e in &program.enums {
            for (name, val) in &e.variants {
                if self.enums.contains_key(name) {
                    return Err(CompileError::new_with_span(
                        format!("Enum variant '{}' defined twice", name),
                        e.span,
                    ));
                }
                self.enums.insert(name.clone(), *val);
            }
        }
        for s in &program.structs {
            if self.structs.contains_key(&s.name) {
                return Err(CompileError::new_with_span(
                    format!("Struct '{}' defined twice", s.name),
                    s.span,
                ));
            }
            self.structs.insert(s.name.clone(), s.span);
        }
        for g in &program.globals {
            if self.globals.contains_key(&g.name) {
                return Err(CompileError::new_with_span(
                    format!("Global variable '{}' defined twice", g.name),
                    g.span,
                ));
            }
            self.globals.insert(g.name.clone(), g.ty.clone());
        }
        for func in &program.functions {
            if self.functions.contains_key(&func.name) {
                return Err(CompileError::new_with_span(
                    format!("Function '{}' defined twice", func.name),
                    func.span,
                ));
            }
            self.functions.insert(func.name.clone(), func.params.len());
        }
        for func in &program.functions {
            if let Some(body) = &func.body {
                self.analyze_function(func, body)?;
            }
        }
        Ok(())
    }

    fn analyze_function(&mut self, func: &Function, body: &Stmt) -> Result<(), CompileError> {
        self.enter_scope();
        for (param_name, param_ty) in &func.params {
            if let Some(scope) = self.scopes.last_mut() {
                scope.insert(param_name.clone(), param_ty.clone());
            }
        }
        self.analyze_stmt(body)?;
        self.exit_scope();
        Ok(())
    }

    fn analyze_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Declare {
                name,
                ty,
                initializer,
                span,
            } => {
                if let Some(init) = initializer {
                    self.analyze_expr(init)?;
                }
                self.declare_variable(name, ty.clone(), *span)?;
            }
            Stmt::Return(expr, _) => {
                if let Some(e) = expr {
                    self.analyze_expr(e)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                self.analyze_expr(condition)?;
                self.analyze_stmt(then_branch)?;
                if let Some(e) = else_branch {
                    self.analyze_stmt(e)?;
                }
            }
            Stmt::While {
                condition, body, ..
            } => {
                self.analyze_expr(condition)?;
                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.analyze_stmt(body)?;
                self.in_loop = prev_loop;
            }
            Stmt::DoWhile {
                condition, body, ..
            } => {
                self.analyze_expr(condition)?;
                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.analyze_stmt(body)?;
                self.in_loop = prev_loop;
            }
            Stmt::For {
                init,
                condition,
                step,
                body,
                ..
            } => {
                self.enter_scope();
                if let Some(i) = init {
                    self.analyze_stmt(i)?;
                }
                if let Some(c) = condition {
                    self.analyze_expr(c)?;
                }
                if let Some(s) = step {
                    self.analyze_expr(s)?;
                }
                let prev_loop = self.in_loop;
                self.in_loop = true;
                self.analyze_stmt(body)?;
                self.in_loop = prev_loop;
                self.exit_scope();
            }
            Stmt::Switch {
                condition,
                cases,
                default,
                ..
            } => {
                self.analyze_expr(condition)?;
                let prev_loop = self.in_loop;
                self.in_loop = true;
                for (_, stmts) in cases {
                    self.enter_scope();
                    for s in stmts {
                        self.analyze_stmt(s)?;
                    }
                    self.exit_scope();
                }
                if let Some(stmts) = default {
                    self.enter_scope();
                    for s in stmts {
                        self.analyze_stmt(s)?;
                    }
                    self.exit_scope();
                }
                self.in_loop = prev_loop;
            }
            Stmt::Block(stmts, _) => {
                self.enter_scope();
                for s in stmts {
                    self.analyze_stmt(s)?;
                }
                self.exit_scope();
            }
            Stmt::Expr(expr) => self.analyze_expr(expr)?,
            Stmt::Break(span) | Stmt::Continue(span) => {
                if !self.in_loop {
                    return Err(CompileError::new_with_span(
                        "Break/Continue outside loop",
                        *span,
                    ));
                }
            }
        }
        Ok(())
    }

    fn analyze_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Variable { name, span } => self.resolve_variable(name, *span)?,
            Expr::StringLiteral { .. } => {}
            Expr::Assign { target, value, .. } => {
                self.analyze_expr(target)?;
                self.analyze_expr(value)?;
            }
            Expr::Binary { left, right, .. } => {
                self.analyze_expr(left)?;
                self.analyze_expr(right)?;
            }
            Expr::Unary { expr, .. } => self.analyze_expr(expr)?,
            Expr::Call {
                name,
                args,
                span: _,
            } => {
                if !self.functions.contains_key(name) {}
                for arg in args {
                    self.analyze_expr(arg)?;
                }
            }
            Expr::Number { .. } => {}
            Expr::Deref { expr, .. } => self.analyze_expr(expr)?,
            Expr::AddrOf { expr, .. } => self.analyze_expr(expr)?,
            Expr::Index { base, index, .. } => {
                self.analyze_expr(base)?;
                self.analyze_expr(index)?;
            }
            Expr::Member { expr, .. } => self.analyze_expr(expr)?,
            Expr::PostUnary { expr, .. } => self.analyze_expr(expr)?,
            Expr::SizeOfExpr { expr, .. } => self.analyze_expr(expr)?,
            Expr::SizeOfType { .. } => {}
            Expr::Cast { expr, .. } => self.analyze_expr(expr)?,
            Expr::InitList { values, .. } => {
                for v in values {
                    self.analyze_expr(v)?;
                }
            }
        }
        Ok(())
    }
}
