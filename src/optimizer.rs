use crate::ir::{Instr, IrFunction, IrProgram, Op, Val};
use std::collections::{HashMap, HashSet};
use std::mem;

pub fn optimize(program: &mut IrProgram) {
    for f in &mut program.functions {
        optimize_function(f);
    }
}

fn optimize_function(func: &mut IrFunction) {
    let mut changed = true;
    let mut rounds = 0;
    while changed && rounds < 8 {
        changed = false;

        changed |= const_fold(func);
        changed |= algebraic_simplify(func);
        changed |= branch_fold(func);
        changed |= copy_propagation(func);
        changed |= mem_forward_dse(func);
        changed |= local_cse(func);
        changed |= dce_temps(func);
        changed |= clean_noops(func);

        changed |= remove_unreachable(func);
        changed |= jump_thread(func);
        changed |= label_jump_peepholes(func);

        changed |= drop_unused_call_result(func);

        changed |= recompute_temp_count(func);
        rounds += 1;
    }

    let _ = remove_unreachable(func);
    let _ = jump_thread(func);
    let _ = label_jump_peepholes(func);
    let _ = drop_unused_call_result(func);
    let _ = recompute_temp_count(func);
}

fn const_fold(func: &mut IrFunction) -> bool {
    let mut changed = false;

    for i in 0..func.instrs.len() {
        let old = mem::replace(&mut func.instrs[i], Instr::Label(String::new()));
        let new_ins = match old {
            Instr::Binary {
                dest,
                op,
                left,
                right,
            } => match (left.clone(), right.clone()) {
                (Val::Const(a), Val::Const(b)) => {
                    let folded = match op {
                        Op::Add => Some(a + b),
                        Op::Sub => Some(a - b),
                        Op::Mul => Some(a * b),
                        Op::Div => {
                            if b != 0 {
                                Some(a / b)
                            } else {
                                None
                            }
                        }
                        Op::Eq => Some(((a - b) == 0) as i64),
                        Op::Ne => Some(((a - b) != 0) as i64),
                        Op::Lt => Some((a < b) as i64),
                        Op::Gt => Some((a > b) as i64),
                        Op::BitAnd => Some(a & b),
                        Op::BitOr => Some(a | b),
                        Op::BitXor => Some(a ^ b),
                        Op::Shl => {
                            if b >= 0 {
                                Some(a << b)
                            } else {
                                None
                            }
                        }
                        Op::Shr => {
                            if b >= 0 {
                                Some(a >> b)
                            } else {
                                None
                            }
                        }
                    };
                    if let Some(c) = folded {
                        changed = true;
                        Instr::Copy {
                            dest,
                            src: Val::Const(c),
                        }
                    } else {
                        Instr::Binary {
                            dest,
                            op,
                            left,
                            right,
                        }
                    }
                }
                _ => Instr::Binary {
                    dest,
                    op,
                    left,
                    right,
                },
            },
            other => other,
        };
        func.instrs[i] = new_ins;
    }

    changed
}

fn algebraic_simplify(func: &mut IrFunction) -> bool {
    let mut changed = false;

    for i in 0..func.instrs.len() {
        let old = mem::replace(&mut func.instrs[i], Instr::Label(String::new()));
        let new_ins = match old {
            Instr::Binary {
                dest,
                op,
                left,
                right,
            } => {
                if matches!(op, Op::Eq | Op::Ne) && left == right {
                    changed = true;
                    let v = if matches!(op, Op::Eq) { 1 } else { 0 };
                    Instr::Copy {
                        dest,
                        src: Val::Const(v),
                    }
                } else {
                    match (op, left.clone(), right.clone()) {
                        (Op::Add, x, Val::Const(0)) | (Op::Add, Val::Const(0), x) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::Sub, x, Val::Const(0)) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::Mul, _, Val::Const(0)) | (Op::Mul, Val::Const(0), _) => {
                            changed = true;
                            Instr::Copy {
                                dest,
                                src: Val::Const(0),
                            }
                        }
                        (Op::Mul, x, Val::Const(1)) | (Op::Mul, Val::Const(1), x) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::Div, x, Val::Const(1)) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::BitAnd, _, Val::Const(0)) | (Op::BitAnd, Val::Const(0), _) => {
                            changed = true;
                            Instr::Copy {
                                dest,
                                src: Val::Const(0),
                            }
                        }
                        (Op::BitOr, x, Val::Const(0)) | (Op::BitOr, Val::Const(0), x) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::BitXor, x, Val::Const(0)) | (Op::BitXor, Val::Const(0), x) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        (Op::Shl, x, Val::Const(0)) | (Op::Shr, x, Val::Const(0)) => {
                            changed = true;
                            Instr::Copy { dest, src: x }
                        }
                        _ => Instr::Binary {
                            dest,
                            op,
                            left,
                            right,
                        },
                    }
                }
            }
            other => other,
        };
        func.instrs[i] = new_ins;
    }

    changed
}

fn branch_fold(func: &mut IrFunction) -> bool {
    let mut changed = false;
    for ins in &mut func.instrs {
        match ins {
            Instr::Branch {
                cond,
                true_label,
                false_label,
            } => {
                if let Val::Const(v) = cond {
                    let target = if *v != 0 {
                        true_label.clone()
                    } else {
                        false_label.clone()
                    };
                    *ins = Instr::Jump { label: target };
                    changed = true;
                } else if true_label == false_label {
                    let lbl = true_label.clone();
                    *ins = Instr::Jump { label: lbl };
                    changed = true;
                }
            }
            _ => {}
        }
    }
    changed
}

fn copy_propagation(func: &mut IrFunction) -> bool {
    let mut changed = false;
    let mut env: Vec<(usize, Val)> = Vec::new();

    fn resolve(env: &[(usize, Val)], v: &Val) -> Val {
        let mut cur = v.clone();
        let mut guard = 0;
        loop {
            guard += 1;
            if guard > 128 {
                break;
            }
            match cur {
                Val::Temp(t) => {
                    if let Some((_, val)) = env.iter().rev().find(|(k, _)| *k == t) {
                        cur = val.clone();
                        continue;
                    }
                    break;
                }
                _ => break,
            }
        }
        cur
    }

    for i in 0..func.instrs.len() {
        if let Some(def_t) = def_temp(&func.instrs[i]) {
            env.retain(|(t, _)| *t != def_t);
        }

        let changed_here = replace_uses_with(&mut func.instrs[i], |v| resolve(&env, v));
        if changed_here {
            changed = true;
        }

        if let Instr::Copy { dest, src } = &func.instrs[i] {
            if let Val::Temp(t) = dest {
                let rhs = resolve(&env, src);
                if !matches!(rhs, Val::Temp(x) if x == *t) {
                    env.push((*t, rhs));
                }
            } else {
                env.clear();
            }
        }

        if !is_pure(&func.instrs[i]) {
            env.clear();
        }
    }
    changed
}

fn mem_forward_dse(func: &mut IrFunction) -> bool {
    let blocks = split_into_blocks(&func.instrs);
    if blocks.is_empty() {
        return false;
    }

    #[derive(Clone)]
    struct StoreInfo {
        idx: usize,
        src: Val,
        is_byte: bool,
        used: bool,
    }

    let mut changed = false;
    let mut out: Vec<Instr> = Vec::with_capacity(func.instrs.len());

    for (start, end) in blocks {
        let mut slice: Vec<Instr> = func.instrs[start..end].to_vec();
        let mut last_store: HashMap<ValKey, StoreInfo> = HashMap::new();
        let mut drop_idx: HashSet<usize> = HashSet::new();

        for i in 0..slice.len() {
            let mut need_barrier = false;
            let mut replacement: Option<Instr> = None;

            let cur = slice[i].clone();
            match cur.clone() {
                Instr::Load { dest, addr } => {
                    let key = ValKey::from(&addr);
                    if let Some(info) = last_store.get(&key).cloned() {
                        if !info.is_byte {
                            replacement = Some(Instr::Copy {
                                dest,
                                src: info.src.clone(),
                            });
                            if let Some(s) = last_store.get_mut(&key) {
                                s.used = true;
                            }
                            changed = true;
                        }
                    }
                    need_barrier = true; // conservative
                }
                Instr::LoadByte { dest, addr } => {
                    let key = ValKey::from(&addr);
                    if let Some(info) = last_store.get(&key).cloned() {
                        if info.is_byte {
                            replacement = Some(Instr::Copy {
                                dest,
                                src: info.src.clone(),
                            });
                            if let Some(s) = last_store.get_mut(&key) {
                                s.used = true;
                            }
                            changed = true;
                        }
                    }
                    need_barrier = true;
                }
                Instr::Store { addr, src } => {
                    let key = ValKey::from(&addr);
                    if let Some(prev) = last_store.get(&key).cloned() {
                        if !prev.used && !prev.is_byte {
                            drop_idx.insert(prev.idx);
                            changed = true;
                        }
                    }
                    last_store.insert(
                        key,
                        StoreInfo {
                            idx: i,
                            src,
                            is_byte: false,
                            used: false,
                        },
                    );
                }
                Instr::StoreByte { addr, src } => {
                    let key = ValKey::from(&addr);
                    if let Some(prev) = last_store.get(&key).cloned() {
                        if prev.is_byte && !prev.used {
                            drop_idx.insert(prev.idx);
                            changed = true;
                        } else if !prev.is_byte {
                            need_barrier = true;
                        }
                    }
                    last_store.insert(
                        key,
                        StoreInfo {
                            idx: i,
                            src,
                            is_byte: true,
                            used: false,
                        },
                    );
                }

                Instr::Binary { .. }
                | Instr::Copy { .. }
                | Instr::Neg { .. }
                | Instr::Not { .. }
                | Instr::AddrOf { .. } => {}

                Instr::Call { .. }
                | Instr::Return(_)
                | Instr::Jump { .. }
                | Instr::Branch { .. }
                | Instr::Label(_)
                | Instr::StackAlloc { .. } => {
                    need_barrier = true;
                }
            }

            if let Some(new_ins) = replacement {
                slice[i] = new_ins;
            }

            if need_barrier {
                last_store.clear();
            }
        }

        if drop_idx.is_empty() {
            out.extend(slice.into_iter());
        } else {
            for (i, ins) in slice.into_iter().enumerate() {
                if drop_idx.contains(&i) {
                } else {
                    out.push(ins);
                }
            }
        }
    }

    if changed {
        func.instrs = out;
    }
    changed
}

fn local_cse(func: &mut IrFunction) -> bool {
    let blocks = split_into_blocks(&func.instrs);
    if blocks.is_empty() {
        return false;
    }

    let mut changed = false;
    let mut out: Vec<Instr> = Vec::with_capacity(func.instrs.len());

    for (start, end) in blocks {
        let mut slice: Vec<Instr> = func.instrs[start..end].to_vec();

        let mut table: HashMap<ValueKey, usize> = HashMap::new();
        let mut env: HashMap<usize, Val> = HashMap::new();

        for i in 0..slice.len() {
            if !is_pure(&slice[i]) {
                table.clear();
                env.clear();
            }

            match &mut slice[i] {
                Instr::Binary {
                    dest,
                    op,
                    left,
                    right,
                } => {
                    let l = apply_env(left.clone(), &env);
                    let r = apply_env(right.clone(), &env);
                    let (tag, l_norm, r_norm) = canonicalize_bin_tag(op.clone(), l, r);
                    if is_cse_eligible_tag(tag, &l_norm, &r_norm) {
                        let key = ValueKey::bin(tag, &l_norm, &r_norm);
                        if let Some(existing_t) = table.get(&key).copied() {
                            let dst = dest.clone();
                            *slice.get_mut(i).unwrap() = Instr::Copy {
                                dest: dst,
                                src: Val::Temp(existing_t),
                            };
                            changed = true;
                        } else if let Val::Temp(dtid) = dest.clone() {
                            table.insert(key, dtid);
                            env.insert(dtid, Val::Temp(dtid));
                        }
                    }
                }
                Instr::Neg { dest, src } => {
                    let s = apply_env(src.clone(), &env);
                    let key = ValueKey::unary("neg", &s);
                    if let Some(existing_t) = table.get(&key).copied() {
                        let dst = dest.clone();
                        *slice.get_mut(i).unwrap() = Instr::Copy {
                            dest: dst,
                            src: Val::Temp(existing_t),
                        };
                        changed = true;
                    } else if let Val::Temp(dtid) = dest.clone() {
                        table.insert(key, dtid);
                        env.insert(dtid, Val::Temp(dtid));
                    }
                }
                Instr::Not { dest, src } => {
                    let s = apply_env(src.clone(), &env);
                    let key = ValueKey::unary("not", &s);
                    if let Some(existing_t) = table.get(&key).copied() {
                        let dst = dest.clone();
                        *slice.get_mut(i).unwrap() = Instr::Copy {
                            dest: dst,
                            src: Val::Temp(existing_t),
                        };
                        changed = true;
                    } else if let Val::Temp(dtid) = dest.clone() {
                        table.insert(key, dtid);
                        env.insert(dtid, Val::Temp(dtid));
                    }
                }
                Instr::AddrOf { dest, src_var } => {
                    let key = ValueKey::addr_of(src_var);
                    if let Some(existing_t) = table.get(&key).copied() {
                        let dst = dest.clone();
                        *slice.get_mut(i).unwrap() = Instr::Copy {
                            dest: dst,
                            src: Val::Temp(existing_t),
                        };
                        changed = true;
                    } else if let Val::Temp(dtid) = dest.clone() {
                        table.insert(key, dtid);
                        env.insert(dtid, Val::Temp(dtid));
                    }
                }
                Instr::Copy { dest, src } => {
                    let resolved = apply_env(src.clone(), &env);
                    if let Val::Temp(dtid) = dest.clone() {
                        env.insert(dtid, resolved);
                    }
                }
                Instr::Load { .. }
                | Instr::LoadByte { .. }
                | Instr::Store { .. }
                | Instr::StoreByte { .. }
                | Instr::Return(_)
                | Instr::Call { .. }
                | Instr::Jump { .. }
                | Instr::Branch { .. }
                | Instr::StackAlloc { .. }
                | Instr::Label(_) => {}
            }
        }

        out.extend(slice.into_iter());
    }

    if changed {
        func.instrs = out;
    }
    changed
}

fn dce_temps(func: &mut IrFunction) -> bool {
    let mut changed = false;
    let mut live: HashSet<usize> = HashSet::new();
    let mut new_instrs: Vec<Instr> = Vec::with_capacity(func.instrs.len());

    for ins in func.instrs.iter().rev() {
        let def = def_temp(ins);
        let side_effect = !is_pure(ins);
        let needed = def.map(|t| live.contains(&t)).unwrap_or(false);

        if side_effect || def.is_none() || needed {
            for u in use_temps(ins) {
                live.insert(u);
            }
            if let Some(t) = def {
                live.remove(&t);
            }
            new_instrs.push(ins.clone());
        } else {
            changed = true;
        }
    }
    new_instrs.reverse();
    if changed {
        func.instrs = new_instrs;
    }
    changed
}

fn clean_noops(func: &mut IrFunction) -> bool {
    let before_len = func.instrs.len();
    func.instrs.retain(|ins| match ins {
        Instr::Copy {
            dest: Val::Temp(dt),
            src: Val::Temp(st),
        } if dt == st => false,
        _ => true,
    });
    before_len != func.instrs.len()
}

fn remove_unreachable(func: &mut IrFunction) -> bool {
    let mut changed = false;
    let mut kept: Vec<Instr> = Vec::with_capacity(func.instrs.len());
    let mut dead = false;

    for ins in func.instrs.iter() {
        match ins {
            Instr::Label(_) => {
                dead = false;
                kept.push(ins.clone());
            }
            Instr::Jump { .. } | Instr::Return(_) => {
                kept.push(ins.clone());
                dead = true;
            }
            _ => {
                if !dead {
                    kept.push(ins.clone());
                } else {
                    changed = true;
                }
            }
        }
    }
    if changed {
        func.instrs = kept;
    }
    changed
}

fn jump_thread(func: &mut IrFunction) -> bool {
    let mut label_pos: HashMap<String, usize> = HashMap::new();
    for (idx, ins) in func.instrs.iter().enumerate() {
        if let Instr::Label(name) = ins {
            label_pos.insert(name.clone(), idx);
        }
    }
    let mut redirect: HashMap<String, String> = HashMap::new();
    for (name, &pos) in &label_pos {
        if pos + 1 < func.instrs.len() {
            if let Instr::Jump { label } = &func.instrs[pos + 1] {
                redirect.insert(name.clone(), label.clone());
            }
        }
    }

    fn final_target(mut l: String, redir: &HashMap<String, String>) -> String {
        let mut guard = 0;
        while let Some(nxt) = redir.get(&l).cloned() {
            l = nxt;
            guard += 1;
            if guard > 1024 {
                break;
            }
        }
        l
    }

    let mut changed = false;
    for ins in &mut func.instrs {
        match ins {
            Instr::Jump { label } => {
                let tgt = final_target(label.clone(), &redirect);
                if *label != tgt {
                    *label = tgt;
                    changed = true;
                }
            }
            Instr::Branch {
                true_label,
                false_label,
                ..
            } => {
                let t = final_target(true_label.clone(), &redirect);
                let f = final_target(false_label.clone(), &redirect);
                if *true_label != t {
                    *true_label = t;
                    changed = true;
                }
                if *false_label != f {
                    *false_label = f;
                    changed = true;
                }
            }
            _ => {}
        }
    }
    changed
}

fn label_jump_peepholes(func: &mut IrFunction) -> bool {
    if func.instrs.is_empty() {
        return false;
    }

    let mut redir: HashMap<String, String> = HashMap::new();
    let mut prev_label: Option<String> = None;
    for ins in &func.instrs {
        if let Instr::Label(l) = ins {
            if let Some(prev) = &prev_label {
                if l != prev {
                    redir.insert(l.clone(), prev.clone());
                }
            }
            prev_label = Some(l.clone());
        } else {
            prev_label = None;
        }
    }

    if !redir.is_empty() {
        for ins in &mut func.instrs {
            match ins {
                Instr::Jump { label } => {
                    if let Some(t) = chase(label.clone(), &redir) {
                        *label = t;
                    }
                }
                Instr::Branch {
                    true_label,
                    false_label,
                    ..
                } => {
                    if let Some(t) = chase(true_label.clone(), &redir) {
                        *true_label = t;
                    }
                    if let Some(f) = chase(false_label.clone(), &redir) {
                        *false_label = f;
                    }
                }
                _ => {}
            }
        }
    }

    let mut refs: HashSet<String> = HashSet::new();
    for ins in &func.instrs {
        match ins {
            Instr::Jump { label } => {
                refs.insert(label.clone());
            }
            Instr::Branch {
                true_label,
                false_label,
                ..
            } => {
                refs.insert(true_label.clone());
                refs.insert(false_label.clone());
            }
            _ => {}
        }
    }

    let mut changed = false;
    let mut compact: Vec<Instr> = Vec::with_capacity(func.instrs.len());
    for i in 0..func.instrs.len() {
        match &func.instrs[i] {
            Instr::Jump { label } => {
                if i + 1 < func.instrs.len() {
                    if let Instr::Label(next) = &func.instrs[i + 1] {
                        if next == label {
                            changed = true;
                            continue;
                        }
                    }
                }
                compact.push(func.instrs[i].clone());
            }
            Instr::Label(l) => {
                if refs.contains(l) {
                    compact.push(func.instrs[i].clone());
                } else {
                    changed = true;
                }
            }
            _ => compact.push(func.instrs[i].clone()),
        }
    }

    if changed {
        func.instrs = compact;
    }
    changed
}

fn chase(mut s: String, redir: &HashMap<String, String>) -> Option<String> {
    let mut changed = false;
    let mut guard = 0;
    while let Some(n) = redir.get(&s) {
        if n == &s {
            break;
        }
        s = n.clone();
        changed = true;
        guard += 1;
        if guard > 1024 {
            break;
        }
    }
    if changed { Some(s) } else { None }
}

fn drop_unused_call_result(func: &mut IrFunction) -> bool {
    let mut live: HashSet<usize> = HashSet::new();
    for ins in func.instrs.iter().rev() {
        for u in use_temps(ins) {
            live.insert(u);
        }
        if let Some(t) = def_temp(ins) {
            live.remove(&t);
        }
    }

    let mut changed = false;
    for ins in &mut func.instrs {
        if let Instr::Call {
            dest: Some(Val::Temp(t)),
            func: fname,
            args,
        } = ins
        {
            if !live.contains(t) {
                let f = fname.clone();
                let a = args.clone();
                *ins = Instr::Call {
                    dest: None,
                    func: f,
                    args: a,
                };
                changed = true;
            }
        }
    }
    changed
}

fn recompute_temp_count(func: &mut IrFunction) -> bool {
    let mut max_id: Option<usize> = None;
    for ins in &func.instrs {
        if let Some(t) = def_temp(ins) {
            max_id = Some(max_id.map_or(t, |m| m.max(t)));
        }
        for u in use_temps(ins) {
            max_id = Some(max_id.map_or(u, |m| m.max(u)));
        }
    }
    let new_cnt = max_id.map(|m| m + 1).unwrap_or(0);
    let changed = new_cnt != func.temp_count;
    if changed {
        func.temp_count = new_cnt;
    }
    changed
}

fn replace_with_copy(ins: &mut Instr, val: Val) {
    if let Instr::Binary { dest, .. } = mem::replace(ins, Instr::Label(String::new())) {
        *ins = Instr::Copy { dest, src: val };
    }
}

fn replace_with_const_copy(ins: &mut Instr, c: i64) {
    replace_with_copy(ins, Val::Const(c));
}

fn is_pure(ins: &Instr) -> bool {
    match ins {
        Instr::Binary { dest, .. }
        | Instr::Copy { dest, .. }
        | Instr::Neg { dest, .. }
        | Instr::Not { dest, .. }
        | Instr::AddrOf { dest, .. } => matches!(dest, Val::Temp(_)),

        Instr::Load { .. }
        | Instr::LoadByte { .. }
        | Instr::Label(_)
        | Instr::Jump { .. }
        | Instr::Branch { .. }
        | Instr::Return(_)
        | Instr::Call { .. }
        | Instr::StackAlloc { .. }
        | Instr::Store { .. }
        | Instr::StoreByte { .. } => false,
    }
}

fn def_temp(ins: &Instr) -> Option<usize> {
    match ins {
        Instr::Binary { dest, .. }
        | Instr::Copy { dest, .. }
        | Instr::Neg { dest, .. }
        | Instr::Not { dest, .. }
        | Instr::AddrOf { dest, .. }
        | Instr::Load { dest, .. }
        | Instr::LoadByte { dest, .. } => match dest {
            Val::Temp(t) => Some(*t),
            _ => None,
        },
        _ => None,
    }
}

fn use_temps(ins: &Instr) -> Vec<usize> {
    let mut v = Vec::new();
    let mut push_val = |val: &Val| {
        if let Val::Temp(t) = val {
            v.push(*t);
        }
    };
    match ins {
        Instr::Binary { left, right, .. } => {
            push_val(left);
            push_val(right);
        }
        Instr::Copy { src, .. } => {
            push_val(src);
        }
        Instr::Neg { src, .. } | Instr::Not { src, .. } => {
            push_val(src);
        }
        Instr::Branch { cond, .. } => {
            push_val(cond);
        }
        Instr::Call { args, .. } => {
            for a in args {
                push_val(a);
            }
        }
        Instr::Load { addr, .. } => {
            push_val(addr);
        }
        Instr::Store { addr, src } => {
            push_val(addr);
            push_val(src);
        }
        Instr::LoadByte { addr, .. } => {
            push_val(addr);
        }
        Instr::StoreByte { addr, src } => {
            push_val(addr);
            push_val(src);
        }
        Instr::Return(vopt) => {
            if let Some(x) = vopt {
                push_val(x);
            }
        }
        Instr::AddrOf { .. } => {}
        Instr::Jump { .. } | Instr::Label(_) | Instr::StackAlloc { .. } => {}
    }
    v
}

fn apply_env(v: Val, env: &HashMap<usize, Val>) -> Val {
    match v {
        Val::Temp(t) => env.get(&t).cloned().unwrap_or(Val::Temp(t)),
        _ => v,
    }
}

fn replace_uses_with<F: Fn(&Val) -> Val>(ins: &mut Instr, f: F) -> bool {
    let mut changed = false;
    let mut map = |v: &mut Val| {
        let newv = f(v);
        if *v != newv {
            *v = newv;
            changed = true;
        }
    };
    match ins {
        Instr::Binary { left, right, .. } => {
            map(left);
            map(right);
        }
        Instr::Copy { src, .. } => {
            map(src);
        }
        Instr::Neg { src, .. } | Instr::Not { src, .. } => {
            map(src);
        }
        Instr::Branch { cond, .. } => {
            map(cond);
        }
        Instr::Call { args, .. } => {
            for a in args {
                map(a);
            }
        }
        Instr::Load { addr, .. } => {
            map(addr);
        }
        Instr::Store { addr, src } => {
            map(addr);
            map(src);
        }
        Instr::LoadByte { addr, .. } => {
            map(addr);
        }
        Instr::StoreByte { addr, src } => {
            map(addr);
            map(src);
        }
        Instr::Return(v) => {
            if let Some(x) = v {
                map(x);
            }
        }
        Instr::AddrOf { .. } | Instr::Jump { .. } | Instr::Label(_) | Instr::StackAlloc { .. } => {}
    }
    changed
}

fn split_into_blocks(instrs: &[Instr]) -> Vec<(usize, usize)> {
    let mut leaders: HashSet<usize> = HashSet::new();
    if !instrs.is_empty() {
        leaders.insert(0);
    }

    for (i, ins) in instrs.iter().enumerate() {
        match ins {
            Instr::Label(_) => {
                leaders.insert(i);
            }
            Instr::Jump { .. } | Instr::Return(_) => {
                if i + 1 < instrs.len() {
                    leaders.insert(i + 1);
                }
            }
            Instr::Branch { .. } => {
                if i + 1 < instrs.len() {
                    leaders.insert(i + 1);
                }
            }
            _ => {}
        }
    }

    let mut ls: Vec<usize> = leaders.into_iter().collect();
    ls.sort_unstable();
    let mut blocks = Vec::new();
    for (idx, &start) in ls.iter().enumerate() {
        let end = if idx + 1 < ls.len() {
            ls[idx + 1]
        } else {
            instrs.len()
        };
        if start < end {
            blocks.push((start, end));
        }
    }
    blocks
}

#[derive(Clone, Eq, PartialEq, Hash, PartialOrd)]
enum ValKey {
    Const(i64),
    Temp(usize),
    Var(String),
    Global(String),
    Str(String),
}

impl ValKey {
    fn from(v: &Val) -> Self {
        match v {
            Val::Const(c) => ValKey::Const(*c),
            Val::Temp(t) => ValKey::Temp(*t),
            Val::Var(s) => ValKey::Var(s.clone()),
            Val::Global(s) => ValKey::Global(s.clone()),
            Val::StringLabel(s) => ValKey::Str(s.clone()),
        }
    }
}

#[derive(Clone, Eq, PartialEq, Hash)]
struct ValueKey {
    kind: &'static str,
    tag: u8,
    a: Option<ValKey>,
    b: Option<ValKey>,
    s: Option<String>,
}

impl ValueKey {
    fn bin(tag: u8, l: &Val, r: &Val) -> Self {
        ValueKey {
            kind: "bin",
            tag,
            a: Some(ValKey::from(l)),
            b: Some(ValKey::from(r)),
            s: None,
        }
    }
    fn unary(tagname: &'static str, v: &Val) -> Self {
        ValueKey {
            kind: tagname,
            tag: 0,
            a: Some(ValKey::from(v)),
            b: None,
            s: None,
        }
    }
    fn addr_of(name: &str) -> Self {
        ValueKey {
            kind: "addr",
            tag: 0,
            a: None,
            b: None,
            s: Some(name.to_string()),
        }
    }
}

fn op_tag(op: Op) -> u8 {
    match op {
        Op::Add => 1,
        Op::Sub => 2,
        Op::Mul => 3,
        Op::Div => 4,
        Op::Eq => 5,
        Op::Ne => 6,
        Op::Lt => 7,
        Op::Gt => 8,
        Op::BitAnd => 9,
        Op::BitOr => 10,
        Op::BitXor => 11,
        Op::Shl => 12,
        Op::Shr => 13,
    }
}

fn is_commutative(op: &Op) -> bool {
    matches!(
        op,
        Op::Add | Op::Mul | Op::Eq | Op::Ne | Op::BitAnd | Op::BitOr | Op::BitXor
    )
}

fn canonicalize_bin_tag(op: Op, mut l: Val, mut r: Val) -> (u8, Val, Val) {
    if is_commutative(&op) {
        let lk = ValKey::from(&l);
        let rk = ValKey::from(&r);
        if rk < lk {
            std::mem::swap(&mut l, &mut r);
        }
    }
    (op_tag(op), l, r)
}

fn is_cse_eligible_tag(tag: u8, l: &Val, r: &Val) -> bool {
    let pure_args = !matches!(l, Val::StringLabel(_)) && !matches!(r, Val::StringLabel(_));
    pure_args && (1..=13).contains(&tag)
}
