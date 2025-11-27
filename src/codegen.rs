use crate::ir::{Instr, IrProgram, Op, Val};
use std::collections::{HashMap, HashSet};
use std::fmt::Write;

pub fn generate(program: &IrProgram) -> String {
    let mut asm = String::new();

    if !program.rodata.is_empty() {
        writeln!(asm, ".section .rodata").unwrap();
        for (label, content) in &program.rodata {
            writeln!(asm, "{}:", label).unwrap();
            writeln!(asm, "  .string \"{}\"", content).unwrap();
        }
    }

    if !program.globals.is_empty() {
        writeln!(asm, "\n.section .data").unwrap();
        for global in &program.globals {
            writeln!(asm, ".globl {}", global).unwrap();
            writeln!(asm, "{}:", global).unwrap();
            let val = program.global_inits.get(global).unwrap_or(&0);
            writeln!(asm, "  .quad {}", val).unwrap();
        }
    }

    writeln!(asm, "\n.text").unwrap();

    for func in &program.functions {
        writeln!(asm, "\n.globl {}", func.name).unwrap();
        writeln!(asm, ".type {}, @function", func.name).unwrap();
        writeln!(asm, "{}:", func.name).unwrap();

        let mut map: HashMap<String, i32> = HashMap::new();
        let mut temp_map: HashMap<usize, i32> = HashMap::new();
        let mut offset: i32 = 16;

        for p in &func.params {
            if !map.contains_key(p) {
                offset += 8;
                map.insert(p.clone(), offset);
            }
        }

        for instr in &func.instrs {
            if let Instr::StackAlloc { name, size } = instr {
                if !map.contains_key(name) {
                    offset += *size as i32;
                    map.insert(name.clone(), offset);
                }
            }
        }

        let mut temps_used: HashSet<usize> = HashSet::new();
        let mut add_val = |v: &Val| {
            if let Val::Temp(id) = v {
                temps_used.insert(*id);
            }
        };
        for instr in &func.instrs {
            match instr {
                Instr::Binary {
                    dest, left, right, ..
                } => {
                    add_val(dest);
                    add_val(left);
                    add_val(right);
                }
                Instr::Copy { dest, src } => {
                    add_val(dest);
                    add_val(src);
                }
                Instr::Neg { dest, src } | Instr::Not { dest, src } => {
                    add_val(dest);
                    add_val(src);
                }
                Instr::AddrOf { dest, .. } => add_val(dest),
                Instr::Load { dest, addr } | Instr::LoadByte { dest, addr } => {
                    add_val(dest);
                    add_val(addr);
                }
                Instr::Store { addr, src } | Instr::StoreByte { addr, src } => {
                    add_val(addr);
                    add_val(src);
                }
                Instr::Branch { cond, .. } => add_val(cond),
                Instr::Return(rv) => {
                    if let Some(v) = rv {
                        add_val(v);
                    }
                }
                Instr::Call { dest, args, .. } => {
                    if let Some(d) = dest {
                        add_val(d);
                    }
                    for a in args {
                        add_val(a);
                    }
                }
                Instr::Jump { .. } | Instr::Label(_) | Instr::StackAlloc { .. } => {}
            }
        }

        let mut temp_ids: Vec<_> = temps_used.into_iter().collect();
        temp_ids.sort_unstable();
        for id in temp_ids {
            offset += 8;
            temp_map.insert(id, offset);
        }

        let frame_size = (offset + 15) & !15;

        writeln!(asm, "  addi sp, sp, -{}", frame_size).unwrap();
        writeln!(asm, "  sd ra, {}(sp)", frame_size - 8).unwrap();
        writeln!(asm, "  sd s0, {}(sp)", frame_size - 16).unwrap();
        writeln!(asm, "  addi s0, sp, {}", frame_size).unwrap();

        let arg_regs = ["a0", "a1", "a2", "a3", "a4", "a5"];
        for (i, param) in func.params.iter().enumerate() {
            if i < arg_regs.len() {
                let off = *map.get(param).expect("internal: missing param slot");
                writeln!(asm, "  sd {}, -{}(s0)", arg_regs[i], off).unwrap();
            }
        }

        for instr in &func.instrs {
            writeln!(asm, "  # {:?}", instr).unwrap();
            match instr {
                Instr::StackAlloc { .. } => {}
                Instr::Label(l) => writeln!(asm, "{}:", l).unwrap(),
                Instr::Jump { label } => writeln!(asm, "  j {}", label).unwrap(),
                Instr::Branch {
                    cond,
                    true_label,
                    false_label,
                } => {
                    load_val(&mut asm, cond, "t0", &map, &temp_map);
                    writeln!(asm, "  bnez t0, {}", true_label).unwrap();
                    writeln!(asm, "  j {}", false_label).unwrap();
                }
                Instr::Return(val) => {
                    if let Some(v) = val {
                        load_val(&mut asm, v, "a0", &map, &temp_map);
                    }
                    writeln!(asm, "  ld ra, {}(sp)", frame_size - 8).unwrap();
                    writeln!(asm, "  ld s0, {}(sp)", frame_size - 16).unwrap();
                    writeln!(asm, "  addi sp, sp, {}", frame_size).unwrap();
                    writeln!(asm, "  ret").unwrap();
                }
                Instr::Copy { dest, src } => {
                    load_val(&mut asm, src, "t0", &map, &temp_map);
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Neg { dest, src } => {
                    load_val(&mut asm, src, "t0", &map, &temp_map);
                    writeln!(asm, "  neg t0, t0").unwrap();
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Not { dest, src } => {
                    load_val(&mut asm, src, "t0", &map, &temp_map);
                    writeln!(asm, "  not t0, t0").unwrap();
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Binary {
                    dest,
                    op,
                    left,
                    right,
                } => {
                    load_val(&mut asm, left, "t0", &map, &temp_map);
                    load_val(&mut asm, right, "t1", &map, &temp_map);
                    match op {
                        Op::Add => writeln!(asm, "  add t0, t0, t1").unwrap(),
                        Op::Sub => writeln!(asm, "  sub t0, t0, t1").unwrap(),
                        Op::Mul => writeln!(asm, "  mul t0, t0, t1").unwrap(),
                        Op::Div => writeln!(asm, "  div t0, t0, t1").unwrap(),
                        Op::Eq => {
                            writeln!(asm, "  sub t0, t0, t1").unwrap();
                            writeln!(asm, "  seqz t0, t0").unwrap();
                        }
                        Op::Ne => {
                            writeln!(asm, "  sub t0, t0, t1").unwrap();
                            writeln!(asm, "  snez t0, t0").unwrap();
                        }
                        Op::Lt => writeln!(asm, "  slt t0, t0, t1").unwrap(),
                        Op::Gt => writeln!(asm, "  slt t0, t1, t0").unwrap(),
                        Op::BitAnd => writeln!(asm, "  and t0, t0, t1").unwrap(),
                        Op::BitOr => writeln!(asm, "  or  t0, t0, t1").unwrap(),
                        Op::BitXor => writeln!(asm, "  xor t0, t0, t1").unwrap(),
                        Op::Shl => writeln!(asm, "  sll t0, t0, t1").unwrap(),
                        Op::Shr => writeln!(asm, "  sra t0, t0, t1").unwrap(),
                    }
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Call { dest, func, args } => {
                    let arg_regs = ["a0", "a1", "a2", "a3", "a4", "a5"];
                    for (i, arg) in args.iter().enumerate() {
                        if i < arg_regs.len() {
                            load_val(&mut asm, arg, arg_regs[i], &map, &temp_map);
                        } else {
                            // TODO: Later support varargs beyond 6, push onto stack here
                        }
                    }
                    writeln!(asm, "  call {}", func).unwrap();
                    if let Some(d) = dest {
                        store_val(&mut asm, d, "a0", &map, &temp_map);
                    }
                }
                Instr::AddrOf { dest, src_var } => {
                    if let Some(off) = map.get(src_var) {
                        writeln!(asm, "  addi t0, s0, -{}", off).unwrap();
                    } else {
                        writeln!(asm, "  la t0, {}", src_var).unwrap();
                    }
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Load { dest, addr } => {
                    load_val(&mut asm, addr, "t0", &map, &temp_map);
                    writeln!(asm, "  ld t0, 0(t0)").unwrap();
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::Store { addr, src } => {
                    load_val(&mut asm, src, "t0", &map, &temp_map);
                    load_val(&mut asm, addr, "t1", &map, &temp_map);
                    writeln!(asm, "  sd t0, 0(t1)").unwrap();
                }
                Instr::LoadByte { dest, addr } => {
                    load_val(&mut asm, addr, "t0", &map, &temp_map);
                    writeln!(asm, "  lb t0, 0(t0)").unwrap();
                    store_val(&mut asm, dest, "t0", &map, &temp_map);
                }
                Instr::StoreByte { addr, src } => {
                    load_val(&mut asm, src, "t0", &map, &temp_map);
                    load_val(&mut asm, addr, "t1", &map, &temp_map);
                    writeln!(asm, "  sb t0, 0(t1)").unwrap();
                }
            }
        }
    }
    asm
}

fn load_val(
    asm: &mut String,
    val: &Val,
    reg: &str,
    map: &HashMap<String, i32>,
    temp_map: &HashMap<usize, i32>,
) {
    match val {
        Val::Const(c) => writeln!(asm, "  li {}, {}", reg, c).unwrap(),
        Val::Var(name) => {
            let off = map.get(name).unwrap_or_else(|| {
                panic!("codegen: missing stack slot for local/param '{}'", name)
            });
            writeln!(asm, "  ld {}, -{}(s0)", reg, off).unwrap();
        }
        Val::Temp(id) => {
            let off = temp_map
                .get(id)
                .unwrap_or_else(|| panic!("codegen: missing temp slot for t{}", id));
            writeln!(asm, "  ld {}, -{}(s0)", reg, off).unwrap();
        }
        Val::StringLabel(lbl) => writeln!(asm, "  la {}, {}", reg, lbl).unwrap(),
        Val::Global(name) => writeln!(asm, "  la {}, {}", reg, name).unwrap(),
    }
}

fn store_val(
    asm: &mut String,
    val: &Val,
    reg: &str,
    map: &HashMap<String, i32>,
    temp_map: &HashMap<usize, i32>,
) {
    match val {
        Val::Var(name) => {
            let off = map.get(name).unwrap_or_else(|| {
                panic!("codegen: missing stack slot for local/param '{}'", name)
            });
            writeln!(asm, "  sd {}, -{}(s0)", reg, off).unwrap();
        }
        Val::Temp(id) => {
            let off = temp_map
                .get(id)
                .unwrap_or_else(|| panic!("codegen: missing temp slot for t{}", id));
            writeln!(asm, "  sd {}, -{}(s0)", reg, off).unwrap();
        }
        Val::Const(_) | Val::StringLabel(_) | Val::Global(_) => {
            panic!("codegen: cannot store to constant/label/global_direct")
        }
    }
}
