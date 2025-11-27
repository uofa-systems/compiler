use crate::ast::{BinOp, Expr, PostOp, Program, Stmt, Type, UnOp};
use crate::ir::{Instr, IrFunction, IrProgram, Op, Val};
use std::collections::HashMap;

pub fn lower(program: &Program) -> IrProgram {
    let mut ir_funcs = Vec::new();
    let mut rodata = Vec::new();
    let mut str_counter = 0;
    let mut label_counter = 0;

    let mut global_types = HashMap::new();
    let mut globals_list = Vec::new();
    let mut global_inits = HashMap::new();

    let mut struct_defs = HashMap::new();
    for s in &program.structs {
        struct_defs.insert(s.name.clone(), s.fields.clone());
    }

    let mut enum_vals = HashMap::new();
    for e in &program.enums {
        for (name, val) in &e.variants {
            enum_vals.insert(name.clone(), *val);
        }
    }

    for g in &program.globals {
        global_types.insert(g.name.clone(), g.ty.clone());
        globals_list.push(g.name.clone());
        if let Some(init) = &g.init {
            if let Expr::Number { value, .. } = init {
                global_inits.insert(g.name.clone(), *value);
            }
        }
    }

    for func in &program.functions {
        if let Some(body) = &func.body {
            let mut builder = IrBuilder::new(
                &mut rodata,
                &mut str_counter,
                &mut label_counter,
                &struct_defs,
                &global_types,
                &enum_vals,
            );
            for (p_name, p_ty) in &func.params {
                builder.emit(Instr::StackAlloc {
                    name: p_name.clone(),
                    size: 8,
                });
                builder.scope_types.insert(p_name.clone(), p_ty.clone());
            }
            builder.lower_stmt(body);
            builder.emit(Instr::Return(Some(Val::Const(0))));
            ir_funcs.push(IrFunction {
                name: func.name.clone(),
                params: func.params.iter().map(|(n, _)| n.clone()).collect(),
                instrs: builder.instrs,
                temp_count: builder.temp_counter,
            });
        }
    }
    IrProgram {
        functions: ir_funcs,
        rodata,
        globals: globals_list,
        global_inits,
    }
}

struct IrBuilder<'a> {
    instrs: Vec<Instr>,
    temp_counter: usize,
    label_counter: &'a mut usize,
    loop_stack: Vec<(String, String)>,
    rodata: &'a mut Vec<(String, String)>,
    str_counter: &'a mut usize,
    struct_defs: &'a HashMap<String, Vec<(String, Type)>>,
    scope_types: HashMap<String, Type>,
    global_types: &'a HashMap<String, Type>,
    enum_vals: &'a HashMap<String, i64>,
}

impl<'a> IrBuilder<'a> {
    fn new(
        rodata: &'a mut Vec<(String, String)>,
        str_counter: &'a mut usize,
        label_counter: &'a mut usize,
        struct_defs: &'a HashMap<String, Vec<(String, Type)>>,
        global_types: &'a HashMap<String, Type>,
        enum_vals: &'a HashMap<String, i64>,
    ) -> Self {
        Self {
            instrs: Vec::new(),
            temp_counter: 0,
            label_counter,
            loop_stack: Vec::new(),
            rodata,
            str_counter,
            struct_defs,
            scope_types: HashMap::new(),
            global_types,
            enum_vals,
        }
    }
    fn new_temp(&mut self) -> Val {
        let t = self.temp_counter;
        self.temp_counter += 1;
        Val::Temp(t)
    }
    fn new_label(&mut self) -> String {
        let l = format!(".L{}", self.label_counter);
        *self.label_counter += 1;
        l
    }
    fn add_string(&mut self, content: String) -> Val {
        let label = format!(".L.str.{}", self.str_counter);
        *self.str_counter += 1;
        self.rodata.push((label.clone(), content));
        Val::StringLabel(label)
    }
    fn emit(&mut self, instr: Instr) {
        self.instrs.push(instr);
    }

    fn get_size(&self, ty: &Type) -> usize {
        match ty {
            Type::Char => 1,
            Type::Int | Type::Pointer(_) | Type::Array(..) => 8,
            Type::Void => 0,
            Type::Struct(name) => {
                if let Some(fields) = self.struct_defs.get(name) {
                    fields.iter().map(|(_, t)| self.get_size(t)).sum()
                } else {
                    8
                }
            }
            Type::Alias(_) => 8,
        }
    }

    fn lower_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.lower_expr(expr);
            }
            Stmt::Declare {
                name,
                ty,
                initializer,
                ..
            } => {
                let size = self.get_size(ty);
                self.emit(Instr::StackAlloc {
                    name: name.clone(),
                    size,
                });
                self.scope_types.insert(name.clone(), ty.clone());
                if let Some(init) = initializer {
                    if let Expr::InitList { values, .. } = init {
                        let base_addr = self.new_temp();
                        self.emit(Instr::AddrOf {
                            dest: base_addr.clone(),
                            src_var: name.clone(),
                        });
                        for (i, val_expr) in values.iter().enumerate() {
                            let val = self.lower_expr(val_expr);
                            let elem_size = match ty {
                                Type::Array(inner, _) => self.get_size(inner),
                                _ => 8,
                            };
                            let offset = self.new_temp();
                            self.emit(Instr::Binary {
                                dest: offset.clone(),
                                op: Op::Mul,
                                left: Val::Const(i as i64),
                                right: Val::Const(elem_size as i64),
                            });
                            let addr = self.new_temp();
                            self.emit(Instr::Binary {
                                dest: addr.clone(),
                                op: Op::Add,
                                left: base_addr.clone(),
                                right: offset,
                            });
                            if elem_size == 1 {
                                self.emit(Instr::StoreByte { addr, src: val });
                            } else {
                                self.emit(Instr::Store { addr, src: val });
                            }
                        }
                    } else {
                        let val = self.lower_expr(init);
                        self.emit(Instr::Copy {
                            dest: Val::Var(name.clone()),
                            src: val,
                        });
                    }
                }
            }
            Stmt::Return(expr, _) => {
                let val = expr.as_ref().map(|e| self.lower_expr(e));
                self.emit(Instr::Return(val));
            }
            Stmt::Block(stmts, _) => {
                for s in stmts {
                    self.lower_stmt(s);
                }
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
                ..
            } => {
                let true_lbl = self.new_label();
                let else_lbl = self.new_label();
                let end_lbl = self.new_label();
                let cond = self.lower_expr(condition);
                self.emit(Instr::Branch {
                    cond,
                    true_label: true_lbl.clone(),
                    false_label: else_lbl.clone(),
                });
                self.emit(Instr::Label(true_lbl));
                self.lower_stmt(then_branch);
                self.emit(Instr::Jump {
                    label: end_lbl.clone(),
                });
                self.emit(Instr::Label(else_lbl));
                if let Some(else_b) = else_branch {
                    self.lower_stmt(else_b);
                }
                self.emit(Instr::Label(end_lbl));
            }
            Stmt::While {
                condition, body, ..
            } => {
                let start_lbl = self.new_label();
                let body_lbl = self.new_label();
                let end_lbl = self.new_label();
                self.loop_stack.push((start_lbl.clone(), end_lbl.clone()));
                self.emit(Instr::Label(start_lbl.clone()));
                let cond = self.lower_expr(condition);
                self.emit(Instr::Branch {
                    cond,
                    true_label: body_lbl.clone(),
                    false_label: end_lbl.clone(),
                });
                self.emit(Instr::Label(body_lbl));
                self.lower_stmt(body);
                self.emit(Instr::Jump { label: start_lbl });
                self.emit(Instr::Label(end_lbl));
                self.loop_stack.pop();
            }
            Stmt::DoWhile {
                condition, body, ..
            } => {
                let start_lbl = self.new_label();
                let cond_lbl = self.new_label();
                let end_lbl = self.new_label();
                self.loop_stack.push((cond_lbl.clone(), end_lbl.clone()));
                self.emit(Instr::Label(start_lbl.clone()));
                self.lower_stmt(body);
                self.emit(Instr::Label(cond_lbl));
                let cond = self.lower_expr(condition);
                self.emit(Instr::Branch {
                    cond,
                    true_label: start_lbl.clone(),
                    false_label: end_lbl.clone(),
                });
                self.emit(Instr::Label(end_lbl));
                self.loop_stack.pop();
            }
            Stmt::For {
                init,
                condition,
                step,
                body,
                ..
            } => {
                if let Some(i) = init {
                    self.lower_stmt(i);
                }
                let start_lbl = self.new_label();
                let body_lbl = self.new_label();
                let end_lbl = self.new_label();
                let step_lbl = self.new_label();
                self.loop_stack.push((step_lbl.clone(), end_lbl.clone()));
                self.emit(Instr::Label(start_lbl.clone()));
                if let Some(cond_expr) = condition {
                    let cond = self.lower_expr(cond_expr);
                    self.emit(Instr::Branch {
                        cond,
                        true_label: body_lbl.clone(),
                        false_label: end_lbl.clone(),
                    });
                } else {
                    self.emit(Instr::Jump {
                        label: body_lbl.clone(),
                    });
                }
                self.emit(Instr::Label(body_lbl));
                self.lower_stmt(body);
                self.emit(Instr::Label(step_lbl));
                if let Some(step_expr) = step {
                    self.lower_expr(step_expr);
                }
                self.emit(Instr::Jump { label: start_lbl });
                self.emit(Instr::Label(end_lbl));
                self.loop_stack.pop();
            }
            Stmt::Switch {
                condition,
                cases,
                default,
                ..
            } => {
                let cond_val = self.lower_expr(condition);
                let end_lbl = self.new_label();
                self.loop_stack.push((String::new(), end_lbl.clone()));
                let mut next_case_lbl = self.new_label();
                for (val, stmts) in cases {
                    let body_lbl = self.new_label();
                    let check_lbl = next_case_lbl;
                    next_case_lbl = self.new_label();
                    self.emit(Instr::Label(check_lbl));
                    let eq_tmp = self.new_temp();
                    self.emit(Instr::Binary {
                        dest: eq_tmp.clone(),
                        op: Op::Eq,
                        left: cond_val.clone(),
                        right: Val::Const(*val),
                    });
                    self.emit(Instr::Branch {
                        cond: eq_tmp,
                        true_label: body_lbl.clone(),
                        false_label: next_case_lbl.clone(),
                    });
                    self.emit(Instr::Label(body_lbl));
                    for s in stmts {
                        self.lower_stmt(s);
                    }
                    self.emit(Instr::Jump {
                        label: end_lbl.clone(),
                    });
                }
                self.emit(Instr::Label(next_case_lbl));
                if let Some(def_stmts) = default {
                    for s in def_stmts {
                        self.lower_stmt(s);
                    }
                }
                self.emit(Instr::Label(end_lbl));
                self.loop_stack.pop();
            }
            Stmt::Break(_) => {
                let target = self.loop_stack.last().unwrap().1.clone();
                self.emit(Instr::Jump { label: target });
            }
            Stmt::Continue(_) => {
                let target = self.loop_stack.last().unwrap().0.clone();
                if target.is_empty() {
                    panic!("Continue not allowed in switch");
                }
                self.emit(Instr::Jump { label: target });
            }
        }
    }

    fn get_expr_type(&self, expr: &Expr) -> Option<Type> {
        match expr {
            Expr::Variable { name, .. } => self.scope_types.get(name).cloned(),
            Expr::Deref { expr, .. } => {
                if let Some(Type::Pointer(inner)) = self.get_expr_type(expr) {
                    Some(*inner)
                } else {
                    None
                }
            }
            Expr::Index { base, .. } => {
                if let Some(Type::Pointer(inner)) = self.get_expr_type(base) {
                    Some(*inner)
                } else if let Some(Type::Array(inner, _)) = self.get_expr_type(base) {
                    Some(*inner)
                } else {
                    None
                }
            }
            Expr::Member {
                expr,
                field,
                is_arrow,
                ..
            } => {
                let base_ty = self.get_expr_type(expr)?;
                let struct_name = match base_ty {
                    Type::Struct(n) if !is_arrow => n,
                    Type::Pointer(inner) if *is_arrow => {
                        if let Type::Struct(n) = *inner {
                            n
                        } else {
                            return None;
                        }
                    }
                    _ => return None,
                };
                if let Some(fields) = self.struct_defs.get(&struct_name) {
                    for (fname, fty) in fields {
                        if fname == field {
                            return Some(fty.clone());
                        }
                    }
                }
                None
            }
            Expr::Cast { ty, .. } => Some(ty.clone()),
            _ => None,
        }
    }
    fn lower_lvalue_addr(&mut self, expr: &Expr) -> Val {
        match expr {
            Expr::Variable { name, .. } => {
                if self.global_types.contains_key(name) && !self.scope_types.contains_key(name) {
                    Val::Global(name.clone())
                } else {
                    let dest = self.new_temp();
                    self.emit(Instr::AddrOf {
                        dest: dest.clone(),
                        src_var: name.clone(),
                    });
                    dest
                }
            }
            Expr::Deref { expr, .. } => self.lower_expr(expr),
            Expr::Index { base, index, .. } => {
                let base_addr = self.lower_expr(base);
                let idx_val = self.lower_expr(index);
                let elem_ty = self.get_expr_type(expr).unwrap_or(Type::Int);
                let elem_size = self.get_size(&elem_ty);
                let offset = self.new_temp();
                self.emit(Instr::Binary {
                    dest: offset.clone(),
                    op: Op::Mul,
                    left: idx_val,
                    right: Val::Const(elem_size as i64),
                });
                let addr = self.new_temp();
                self.emit(Instr::Binary {
                    dest: addr.clone(),
                    op: Op::Add,
                    left: base_addr,
                    right: offset,
                });
                addr
            }
            Expr::Member {
                expr,
                field,
                is_arrow,
                ..
            } => {
                let base_addr = if *is_arrow {
                    self.lower_expr(expr)
                } else {
                    self.lower_lvalue_addr(expr)
                };
                let ty = self
                    .get_expr_type(expr)
                    .expect("Cannot resolve struct type");
                let struct_name = match ty {
                    Type::Struct(n) => n,
                    Type::Pointer(inner) => {
                        if let Type::Struct(n) = *inner {
                            n
                        } else {
                            panic!("Not a struct pointer")
                        }
                    }
                    _ => panic!("Not a struct"),
                };
                let mut offset_bytes = 0;
                let mut found = false;
                if let Some(fields) = self.struct_defs.get(&struct_name) {
                    for (fname, fty) in fields {
                        if fname == field {
                            found = true;
                            break;
                        }
                        offset_bytes += self.get_size(fty);
                    }
                }
                if !found {
                    panic!("Field {} not found in struct {}", field, struct_name);
                }
                let final_addr = self.new_temp();
                self.emit(Instr::Binary {
                    dest: final_addr.clone(),
                    op: Op::Add,
                    left: base_addr,
                    right: Val::Const(offset_bytes as i64),
                });
                final_addr
            }
            _ => panic!("Not an lvalue"),
        }
    }

    fn lower_expr(&mut self, expr: &Expr) -> Val {
        match expr {
            Expr::Number { value, .. } => Val::Const(*value),
            Expr::StringLiteral { value, .. } => self.add_string(value.clone()),
            Expr::SizeOfType { ty, .. } => Val::Const(self.get_size(ty) as i64),
            Expr::SizeOfExpr { expr, .. } => {
                let ty = self.get_expr_type(expr).unwrap_or(Type::Int);
                Val::Const(self.get_size(&ty) as i64)
            }
            Expr::Cast { expr, .. } => self.lower_expr(expr),
            Expr::Variable { name, .. } => {
                if let Some(val) = self.enum_vals.get(name) {
                    return Val::Const(*val);
                }
                if let Some(ty) = self.global_types.get(name) {
                    if !self.scope_types.contains_key(name) {
                        let dest = self.new_temp();
                        let addr = Val::Global(name.clone());
                        if matches!(ty, Type::Char) {
                            self.emit(Instr::LoadByte {
                                dest: dest.clone(),
                                addr,
                            });
                        } else {
                            self.emit(Instr::Load {
                                dest: dest.clone(),
                                addr,
                            });
                        }
                        return dest;
                    }
                }
                if let Some(Type::Array(..)) = self.scope_types.get(name) {
                    let dest = self.new_temp();
                    self.emit(Instr::AddrOf {
                        dest: dest.clone(),
                        src_var: name.clone(),
                    });
                    return dest;
                }
                if let Some(Type::Char) = self.scope_types.get(name) {
                    let dest = self.new_temp();
                    let addr = self.new_temp();
                    self.emit(Instr::AddrOf {
                        dest: addr.clone(),
                        src_var: name.clone(),
                    });
                    self.emit(Instr::LoadByte {
                        dest: dest.clone(),
                        addr,
                    });
                    return dest;
                }
                Val::Var(name.clone())
            }
            Expr::Assign { target, value, .. } => {
                let val = self.lower_expr(value);
                let addr = self.lower_lvalue_addr(target);
                let ty = self.get_expr_type(target).unwrap_or(Type::Int);
                if matches!(ty, Type::Char) {
                    self.emit(Instr::StoreByte {
                        addr,
                        src: val.clone(),
                    });
                } else {
                    self.emit(Instr::Store {
                        addr,
                        src: val.clone(),
                    });
                }
                val
            }
            Expr::AddrOf { expr, .. } => self.lower_lvalue_addr(expr),
            Expr::Deref { expr, .. } => {
                let addr = self.lower_expr(expr);
                let dest = self.new_temp();
                let ptr_ty = self.get_expr_type(expr).unwrap_or(Type::Int);
                let target_ty = if let Type::Pointer(inner) = ptr_ty {
                    *inner
                } else {
                    Type::Int
                };
                if matches!(target_ty, Type::Char) {
                    self.emit(Instr::LoadByte {
                        dest: dest.clone(),
                        addr,
                    });
                } else {
                    self.emit(Instr::Load {
                        dest: dest.clone(),
                        addr,
                    });
                }
                dest
            }
            Expr::Index { .. } | Expr::Member { .. } => {
                let addr = self.lower_lvalue_addr(expr);
                let dest = self.new_temp();
                let ty = self.get_expr_type(expr).unwrap_or(Type::Int);
                if matches!(ty, Type::Char) {
                    self.emit(Instr::LoadByte {
                        dest: dest.clone(),
                        addr,
                    });
                } else {
                    self.emit(Instr::Load {
                        dest: dest.clone(),
                        addr,
                    });
                }
                dest
            }
            Expr::PostUnary { op, expr, .. } => {
                let addr = self.lower_lvalue_addr(expr);
                let old_val = self.new_temp();
                let ty = self.get_expr_type(expr).unwrap_or(Type::Int);
                if matches!(ty, Type::Char) {
                    self.emit(Instr::LoadByte {
                        dest: old_val.clone(),
                        addr: addr.clone(),
                    });
                } else {
                    self.emit(Instr::Load {
                        dest: old_val.clone(),
                        addr: addr.clone(),
                    });
                }
                let new_val = self.new_temp();
                let ir_op = match op {
                    PostOp::Inc => Op::Add,
                    PostOp::Dec => Op::Sub,
                };
                self.emit(Instr::Binary {
                    dest: new_val.clone(),
                    op: ir_op,
                    left: old_val.clone(),
                    right: Val::Const(1),
                });
                if matches!(ty, Type::Char) {
                    self.emit(Instr::StoreByte { addr, src: new_val });
                } else {
                    self.emit(Instr::Store { addr, src: new_val });
                }
                old_val
            }
            Expr::Binary {
                op, left, right, ..
            } => {
                if matches!(op, BinOp::LogicAnd) {
                    let dest = self.new_temp();
                    let check_right = self.new_label();
                    let is_true = self.new_label();
                    let is_false = self.new_label();
                    let end = self.new_label();
                    let l = self.lower_expr(left);
                    self.emit(Instr::Branch {
                        cond: l,
                        true_label: check_right.clone(),
                        false_label: is_false.clone(),
                    });
                    self.emit(Instr::Label(check_right));
                    let r = self.lower_expr(right);
                    self.emit(Instr::Branch {
                        cond: r,
                        true_label: is_true.clone(),
                        false_label: is_false.clone(),
                    });
                    self.emit(Instr::Label(is_true));
                    self.emit(Instr::Copy {
                        dest: dest.clone(),
                        src: Val::Const(1),
                    });
                    self.emit(Instr::Jump { label: end.clone() });
                    self.emit(Instr::Label(is_false));
                    self.emit(Instr::Copy {
                        dest: dest.clone(),
                        src: Val::Const(0),
                    });
                    self.emit(Instr::Label(end));
                    return dest;
                }
                if matches!(op, BinOp::LogicOr) {
                    let dest = self.new_temp();
                    let check_right = self.new_label();
                    let is_true = self.new_label();
                    let is_false = self.new_label();
                    let end = self.new_label();
                    let l = self.lower_expr(left);
                    self.emit(Instr::Branch {
                        cond: l,
                        true_label: is_true.clone(),
                        false_label: check_right.clone(),
                    });
                    self.emit(Instr::Label(check_right));
                    let r = self.lower_expr(right);
                    self.emit(Instr::Branch {
                        cond: r,
                        true_label: is_true.clone(),
                        false_label: is_false.clone(),
                    });
                    self.emit(Instr::Label(is_true));
                    self.emit(Instr::Copy {
                        dest: dest.clone(),
                        src: Val::Const(1),
                    });
                    self.emit(Instr::Jump { label: end.clone() });
                    self.emit(Instr::Label(is_false));
                    self.emit(Instr::Copy {
                        dest: dest.clone(),
                        src: Val::Const(0),
                    });
                    self.emit(Instr::Label(end));
                    return dest;
                }
                let l = self.lower_expr(left);
                let r = self.lower_expr(right);
                let dest = self.new_temp();
                let ir_op = match op {
                    BinOp::Add => Op::Add,
                    BinOp::Sub => Op::Sub,
                    BinOp::Mul => Op::Mul,
                    BinOp::Div => Op::Div,
                    BinOp::Eq => Op::Eq,
                    BinOp::NotEq => Op::Ne,
                    BinOp::Lt => Op::Lt,
                    BinOp::Gt => Op::Gt,
                    BinOp::BitAnd => Op::BitAnd,
                    BinOp::BitOr => Op::BitOr,
                    BinOp::BitXor => Op::BitXor,
                    BinOp::Shl => Op::Shl,
                    BinOp::Shr => Op::Shr,
                    _ => Op::Eq,
                };
                if matches!(op, BinOp::LtEq) {
                    let t1 = self.new_temp();
                    self.emit(Instr::Binary {
                        dest: t1.clone(),
                        op: Op::Lt,
                        left: r,
                        right: l,
                    });
                    self.emit(Instr::Binary {
                        dest: dest.clone(),
                        op: Op::Eq,
                        left: t1,
                        right: Val::Const(0),
                    });
                } else if matches!(op, BinOp::GtEq) {
                    let t1 = self.new_temp();
                    self.emit(Instr::Binary {
                        dest: t1.clone(),
                        op: Op::Lt,
                        left: l,
                        right: r,
                    });
                    self.emit(Instr::Binary {
                        dest: dest.clone(),
                        op: Op::Eq,
                        left: t1,
                        right: Val::Const(0),
                    });
                } else {
                    self.emit(Instr::Binary {
                        dest: dest.clone(),
                        op: ir_op,
                        left: l,
                        right: r,
                    });
                }
                dest
            }
            Expr::Unary { op, expr, .. } => {
                let src = self.lower_expr(expr);
                let dest = self.new_temp();
                match op {
                    UnOp::Neg => self.emit(Instr::Neg {
                        dest: dest.clone(),
                        src,
                    }),
                    UnOp::BitNot => self.emit(Instr::Not {
                        dest: dest.clone(),
                        src,
                    }),
                    UnOp::LogicNot => {
                        self.emit(Instr::Binary {
                            dest: dest.clone(),
                            op: Op::Eq,
                            left: src,
                            right: Val::Const(0),
                        });
                    }
                }
                dest
            }
            Expr::Call { name, args, .. } => {
                let arg_vals = args.iter().map(|a| self.lower_expr(a)).collect();
                let dest = self.new_temp();
                self.emit(Instr::Call {
                    dest: Some(dest.clone()),
                    func: name.clone(),
                    args: arg_vals,
                });
                dest
            }
            Expr::InitList { .. } => panic!("InitList expression not supported in this context"),
        }
    }
}
