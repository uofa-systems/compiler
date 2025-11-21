use crate::ast::{BinOp, Expr, Function, Program, Stmt, UnOp};
use crate::error::CompileError;
use std::collections::HashMap;

/// Context for code generation for a single function.
struct CodeGen<'a> {
    ast: &'a Function,
    var_map: HashMap<String, i32>,
    stack_size: i32,
    label_count: usize,
}

impl<'a> CodeGen<'a> {
    fn new(ast: &'a Function) -> Self {
        CodeGen {
            ast,
            var_map: HashMap::new(),
            stack_size: 0,
            label_count: 0,
        }
    }

    fn new_label(&mut self) -> String {
        let count = self.label_count;
        self.label_count += 1;
        format!(".L{}", count)
    }

    fn analyze_stack(&mut self) {
        let mut offset = 0;
        for param_name in self.ast.params.iter() {
            offset += 8;
            self.var_map.insert(param_name.clone(), offset);
        }
        self.find_declarations_in_stmt(&self.ast.body, &mut offset);
        self.stack_size = offset;
    }

    fn find_declarations_in_stmt(&mut self, stmt: &Stmt, offset: &mut i32) {
        match stmt {
            Stmt::Declare { name, .. } => {
                *offset += 8;
                self.var_map.insert(name.clone(), *offset);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.find_declarations_in_stmt(s, offset);
                }
            }
            Stmt::If {
                then_branch,
                else_branch,
                ..
            } => {
                self.find_declarations_in_stmt(then_branch, offset);
                if let Some(else_b) = else_branch {
                    self.find_declarations_in_stmt(else_b, offset);
                }
            }
            Stmt::While { body, .. } => {
                self.find_declarations_in_stmt(body, offset);
            }
            _ => {}
        }
    }

    fn generate_function(&mut self) -> Result<(), CompileError> {
        self.analyze_stack();
        println!("{}:", self.ast.name);

        // Prologue
        println!("  push rbp");
        println!("  mov rbp, rsp");
        let aligned_stack_size = (self.stack_size + 15) & !15;
        if aligned_stack_size > 0 {
            println!("  sub rsp, {}", aligned_stack_size);
        }

        // Params
        let arg_registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        for (i, param_name) in self.ast.params.iter().enumerate() {
            if i >= arg_registers.len() {
                return Err("More than 6 parameters not supported".into());
            }
            let offset = *self.var_map.get(param_name).unwrap();
            println!("  mov [rbp - {}], {}", offset, arg_registers[i]);
        }

        // Body
        self.gen_stmt(&self.ast.body)?;

        // Epilogue
        println!(".L.return.{}:", self.ast.name);
        println!("  mov rsp, rbp");
        println!("  pop rbp");
        println!("  ret");

        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Return(expr) => {
                self.gen_expr(expr)?;
                println!("  jmp .L.return.{}", self.ast.name);
            }
            Stmt::Block(stmts) => {
                for s in stmts {
                    self.gen_stmt(s)?;
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                let else_label = self.new_label();
                let end_label = self.new_label();
                self.gen_expr(condition)?;
                println!("  cmp rax, 0");
                println!("  je {}", else_label);
                self.gen_stmt(then_branch)?;
                println!("  jmp {}", end_label);
                println!("{}:", else_label);
                if let Some(else_stmt) = else_branch {
                    self.gen_stmt(else_stmt)?;
                }
                println!("{}:", end_label);
            }
            Stmt::While { condition, body } => {
                let start_label = self.new_label();
                let end_label = self.new_label();
                println!("{}:", start_label);
                self.gen_expr(condition)?;
                println!("  cmp rax, 0");
                println!("  je {}", end_label);
                self.gen_stmt(body)?;
                println!("  jmp {}", start_label);
                println!("{}:", end_label);
            }
            Stmt::Declare { name, initializer } => {
                if let Some(expr) = initializer {
                    let offset = *self.var_map.get(name).unwrap();
                    self.gen_expr(expr)?;
                    println!("  mov [rbp - {}], rax", offset);
                }
            }
            Stmt::Expr(expr) => {
                self.gen_expr(expr)?;
            }
        }
        Ok(())
    }

    /// Recursively generates code for an expression. Leaves result in RAX.
    fn gen_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Number { value, .. } => {
                println!("  mov rax, {}", value);
            }
            Expr::Variable { name, .. } => {
                let offset = *self
                    .var_map
                    .get(name)
                    .ok_or_else(|| CompileError::new(format!("Undefined variable: {}", name)))?;
                println!("  mov rax, [rbp - {}]", offset);
            }
            Expr::Assign { name, value, .. } => {
                let offset = *self.var_map.get(name).ok_or_else(|| {
                    CompileError::new(format!("Undefined variable in assignment: {}", name))
                })?;
                self.gen_expr(value)?;
                println!("  mov [rbp - {}], rax", offset);
            }
            Expr::Unary { op, expr, .. } => {
                self.gen_expr(expr)?;
                match op {
                    UnOp::Neg => println!("  neg rax"),
                }
            }
            // MODIFIED: Added code generation for new comparison operators
            Expr::Binary {
                op, left, right, ..
            } => {
                self.gen_expr(right)?;
                println!("  push rax");
                self.gen_expr(left)?;
                println!("  pop rdi");
                match op {
                    BinOp::Add => println!("  add rax, rdi"),
                    BinOp::Sub => println!("  sub rax, rdi"),
                    BinOp::Mul => println!("  imul rax, rdi"),
                    BinOp::Div => {
                        println!("  cqo");
                        println!("  idiv rdi");
                    }
                    BinOp::Eq => {
                        println!("  cmp rax, rdi");
                        println!("  sete al");
                        println!("  movzx rax, al");
                    }
                    BinOp::NotEq => {
                        println!("  cmp rax, rdi");
                        println!("  setne al");
                        println!("  movzx rax, al");
                    }
                    BinOp::Lt => {
                        println!("  cmp rax, rdi");
                        println!("  setl al");
                        println!("  movzx rax, al");
                    }
                    BinOp::LtEq => {
                        println!("  cmp rax, rdi");
                        println!("  setle al");
                        println!("  movzx rax, al");
                    }
                    BinOp::Gt => {
                        println!("  cmp rax, rdi");
                        println!("  setg al");
                        println!("  movzx rax, al");
                    }
                    BinOp::GtEq => {
                        println!("  cmp rax, rdi");
                        println!("  setge al");
                        println!("  movzx rax, al");
                    }
                }
            }
            Expr::Call { name, args, .. } => {
                if args.len() > 6 {
                    return Err("More than 6 arguments not supported".into());
                }
                for arg in args.iter() {
                    self.gen_expr(arg)?;
                    println!("  push rax");
                }
                let arg_registers = ["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
                for i in (0..args.len()).rev() {
                    println!("  pop {}", arg_registers[i]);
                }
                println!("  call {}", name);
            }
        }
        Ok(())
    }
}

pub fn generate(ast: &Program) -> Result<(), CompileError> {
    println!(".intel_syntax noprefix");

    for function in &ast.functions {
        if function.name == "main" {
            println!(".globl main");
        }
        let mut generator = CodeGen::new(function);
        generator.generate_function()?;
    }

    Ok(())
}
