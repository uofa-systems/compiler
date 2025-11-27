#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Val {
    Const(i64),
    Temp(usize),
    Var(String),
    Global(String),
    StringLabel(String),
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Ne,
    Lt,
    Gt,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}

#[derive(Debug, Clone)]
pub enum Instr {
    Binary {
        dest: Val,
        op: Op,
        left: Val,
        right: Val,
    },
    Copy {
        dest: Val,
        src: Val,
    },
    Neg {
        dest: Val,
        src: Val,
    },
    Not {
        dest: Val,
        src: Val,
    },
    Jump {
        label: String,
    },
    Branch {
        cond: Val,
        true_label: String,
        false_label: String,
    },
    Label(String),
    Call {
        dest: Option<Val>,
        func: String,
        args: Vec<Val>,
    },
    Return(Option<Val>),
    StackAlloc {
        name: String,
        size: usize,
    },
    AddrOf {
        dest: Val,
        src_var: String,
    },
    Load {
        dest: Val,
        addr: Val,
    },
    Store {
        addr: Val,
        src: Val,
    },
    LoadByte {
        dest: Val,
        addr: Val,
    },
    StoreByte {
        addr: Val,
        src: Val,
    },
}

#[derive(Debug)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<String>,
    pub instrs: Vec<Instr>,
    pub temp_count: usize,
}

#[derive(Debug)]
pub struct IrProgram {
    pub globals: Vec<String>,
    pub global_inits: std::collections::HashMap<String, i64>,
    pub functions: Vec<IrFunction>,
    pub rodata: Vec<(String, String)>,
}
