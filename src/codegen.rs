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
        // In RISC-V with our frame pointer setup, parameters passed in registers
        // will be spilled to the stack. We assign them offsets relative to s0 (fp).
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

    // Helper to simulate x86 'push' on RISC-V
    fn emit_push(&self, reg: &str) {
        println!("  addi sp, sp, -8");
        println!("  sd {}, 0(sp)", reg);
    }

    // Helper to simulate x86 'pop' on RISC-V
    fn emit_pop(&self, reg: &str) {
        println!("  ld {}, 0(sp)", reg);
        println!("  addi sp, sp, 8");
    }

    fn generate_function(&mut self) -> Result<(), CompileError> {
        self.analyze_stack();

        // Function symbol
        println!("\n.text");
        println!(".globl {}", self.ast.name);
        println!(".type {}, @function", self.ast.name);
        println!("{}:", self.ast.name);

        // -- Prologue --
        // 1. Allocate space for Return Address (ra) and Old Frame Pointer (s0)
        println!("  addi sp, sp, -16");
        println!("  sd ra, 8(sp)");
        println!("  sd s0, 0(sp)");
        // 2. Set new Frame Pointer to the top of the stack frame
        println!("  addi s0, sp, 16");

        // 3. Allocate space for local variables (aligned to 16 bytes)
        let aligned_stack_size = (self.stack_size + 15) & !15;
        if aligned_stack_size > 0 {
            println!("  sub sp, sp, {}", aligned_stack_size);
        }

        // -- Parameters --
        // RISC-V passes args in a0-a7. We spill them to the stack for simplicity.
        // This compiler supports up to 6 args (a0-a5).
        let arg_registers = ["a0", "a1", "a2", "a3", "a4", "a5"];
        for (i, param_name) in self.ast.params.iter().enumerate() {
            if i >= arg_registers.len() {
                return Err("More than 6 parameters not supported".into());
            }
            let offset = *self.var_map.get(param_name).unwrap();
            // Store argument register into local stack slot: -offset(s0)
            println!("  sd {}, -{}(s0)", arg_registers[i], offset);
        }

        // -- Body --
        self.gen_stmt(&self.ast.body)?;

        // -- Epilogue Label --
        println!(".L.return.{}:", self.ast.name);

        // Restore Stack Pointer (release locals)
        // We calculate where sp should be: s0 - 16
        println!("  addi sp, s0, -16");

        // Restore RA and Old FP
        println!("  ld ra, 8(sp)");
        println!("  ld s0, 0(sp)");

        // Pop the RA/FP slot
        println!("  addi sp, sp, 16");

        // Return
        println!("  ret");

        Ok(())
    }

    fn gen_stmt(&mut self, stmt: &Stmt) -> Result<(), CompileError> {
        match stmt {
            Stmt::Return(expr) => {
                // Result goes in a0
                self.gen_expr(expr)?;
                println!("  j .L.return.{}", self.ast.name);
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

                self.gen_expr(condition)?; // Result in a0
                // If a0 == 0, jump to else
                println!("  beq a0, zero, {}", else_label);

                self.gen_stmt(then_branch)?;
                println!("  j {}", end_label);

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
                self.gen_expr(condition)?; // Result in a0

                // If a0 == 0, jump to end
                println!("  beq a0, zero, {}", end_label);

                self.gen_stmt(body)?;
                println!("  j {}", start_label);
                println!("{}:", end_label);
            }
            Stmt::Declare { name, initializer } => {
                if let Some(expr) = initializer {
                    let offset = *self.var_map.get(name).unwrap();
                    self.gen_expr(expr)?;
                    // Store a0 into stack variable
                    println!("  sd a0, -{}(s0)", offset);
                }
            }
            Stmt::Expr(expr) => {
                self.gen_expr(expr)?;
            }
        }
        Ok(())
    }

    /// Recursively generates code for an expression. Leaves result in a0.
    fn gen_expr(&mut self, expr: &Expr) -> Result<(), CompileError> {
        match expr {
            Expr::Number { value, .. } => {
                println!("  li a0, {}", value);
            }
            Expr::Variable { name, .. } => {
                let offset = *self
                    .var_map
                    .get(name)
                    .ok_or_else(|| CompileError::new(format!("Undefined variable: {}", name)))?;
                println!("  ld a0, -{}(s0)", offset);
            }
            Expr::Assign { name, value, .. } => {
                let offset = *self.var_map.get(name).ok_or_else(|| {
                    CompileError::new(format!("Undefined variable in assignment: {}", name))
                })?;
                self.gen_expr(value)?;
                println!("  sd a0, -{}(s0)", offset);
            }
            Expr::Unary { op, expr, .. } => {
                self.gen_expr(expr)?;
                match op {
                    UnOp::Neg => println!("  neg a0, a0"),
                }
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                // 1. Evaluate Right, result in a0
                self.gen_expr(right)?;
                // 2. Push Right to stack
                self.emit_push("a0");
                // 3. Evaluate Left, result in a0
                self.gen_expr(left)?;
                // 4. Pop Right into t1
                self.emit_pop("t1");

                // Left is in a0, Right is in t1
                match op {
                    BinOp::Add => println!("  add a0, a0, t1"),
                    BinOp::Sub => println!("  sub a0, a0, t1"),
                    BinOp::Mul => println!("  mul a0, a0, t1"),
                    BinOp::Div => println!("  div a0, a0, t1"), // Integer division

                    // Comparisons return 1 or 0 in a0
                    BinOp::Eq => {
                        println!("  sub a0, a0, t1"); // a0 = left - right
                        println!("  seqz a0, a0"); // a0 = (a0 == 0) ? 1 : 0
                    }
                    BinOp::NotEq => {
                        println!("  sub a0, a0, t1"); // a0 = left - right
                        println!("  snez a0, a0"); // a0 = (a0 != 0) ? 1 : 0
                    }
                    BinOp::Lt => {
                        // slt rd, rs1, rs2 -> set rd=1 if rs1 < rs2
                        println!("  slt a0, a0, t1");
                    }
                    BinOp::LtEq => {
                        // left <= right is !(right < left) -> slt t0, right, left; xori a0, t0, 1
                        println!("  slt a0, t1, a0"); // Check right < left
                        println!("  xori a0, a0, 1"); // Negate result
                    }
                    BinOp::Gt => {
                        // left > right is right < left
                        println!("  slt a0, t1, a0");
                    }
                    BinOp::GtEq => {
                        // left >= right is !(left < right)
                        println!("  slt a0, a0, t1"); // Check left < right
                        println!("  xori a0, a0, 1"); // Negate
                    }
                }
            }
            Expr::Call { name, args, .. } => {
                if args.len() > 6 {
                    return Err("More than 6 arguments not supported".into());
                }

                // Evaluate arguments and push them to stack
                for arg in args.iter() {
                    self.gen_expr(arg)?;
                    self.emit_push("a0");
                }

                // Pop arguments into argument registers in reverse order
                let arg_registers = ["a0", "a1", "a2", "a3", "a4", "a5"];
                for i in (0..args.len()).rev() {
                    self.emit_pop(arg_registers[i]);
                }

                println!("  call {}", name);
            }
        }
        Ok(())
    }
}

pub fn generate(ast: &Program) -> Result<(), CompileError> {
    // Basic RISC-V Header

    // Process functions
    for function in &ast.functions {
        let mut generator = CodeGen::new(function);
        generator.generate_function()?;
    }

    Ok(())
}
