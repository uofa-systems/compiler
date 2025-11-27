use crate::span::Span;

#[derive(Debug, Clone)]
pub struct Program {
    pub structs: Vec<StructDecl>,
    pub globals: Vec<GlobalDecl>,
    pub functions: Vec<Function>,
    pub enums: Vec<EnumDecl>,
    pub typedefs: Vec<TypedefDecl>,
}

#[derive(Debug, Clone)]
pub struct StructDecl {
    pub name: String,
    pub fields: Vec<(String, Type)>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct EnumDecl {
    pub name: String,
    pub variants: Vec<(String, i64)>, // Name, Value
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct TypedefDecl {
    pub new_name: String,
    pub ty: Type,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct GlobalDecl {
    pub name: String,
    pub ty: Type,
    pub init: Option<Expr>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Type {
    Void,
    Int,
    Char,
    Pointer(Box<Type>),
    Array(Box<Type>, usize),
    Struct(String),
    Alias(String),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub params: Vec<(String, Type)>,
    pub body: Option<Stmt>,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum Stmt {
    Return(Option<Expr>, Span),
    If {
        condition: Expr,
        then_branch: Box<Stmt>,
        else_branch: Option<Box<Stmt>>,
        span: Span,
    },
    While {
        condition: Expr,
        body: Box<Stmt>,
        span: Span,
    },
    DoWhile {
        body: Box<Stmt>,
        condition: Expr,
        span: Span,
    },
    For {
        init: Option<Box<Stmt>>,
        condition: Option<Expr>,
        step: Option<Expr>,
        body: Box<Stmt>,
        span: Span,
    },
    Switch {
        condition: Expr,
        cases: Vec<(i64, Vec<Stmt>)>,
        default: Option<Vec<Stmt>>,
        span: Span,
    },
    Block(Vec<Stmt>, Span),
    Declare {
        name: String,
        ty: Type,
        initializer: Option<Expr>,
        span: Span,
    },
    Expr(Expr),
    Break(Span),
    Continue(Span),
}

#[derive(Debug, Clone)]
pub enum Expr {
    Number {
        value: i64,
        span: Span,
    },
    StringLiteral {
        value: String,
        span: Span,
    },
    Variable {
        name: String,
        span: Span,
    },
    InitList {
        values: Vec<Expr>,
        span: Span,
    },
    Binary {
        op: BinOp,
        left: Box<Expr>,
        right: Box<Expr>,
        span: Span,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
        span: Span,
    },
    PostUnary {
        op: PostOp,
        expr: Box<Expr>,
        span: Span,
    },
    Assign {
        target: Box<Expr>,
        value: Box<Expr>,
        span: Span,
    },
    Call {
        name: String,
        args: Vec<Expr>,
        span: Span,
    },
    Deref {
        expr: Box<Expr>,
        span: Span,
    },
    AddrOf {
        expr: Box<Expr>,
        span: Span,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Member {
        expr: Box<Expr>,
        field: String,
        is_arrow: bool,
        span: Span,
    },
    SizeOfType {
        ty: Type,
        span: Span,
    },
    SizeOfExpr {
        expr: Box<Expr>,
        span: Span,
    },
    Cast {
        ty: Type,
        expr: Box<Expr>,
        span: Span,
    },
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    NotEq,
    Lt,
    LtEq,
    Gt,
    GtEq,
    LogicAnd,
    LogicOr,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnOp {
    Neg,
    LogicNot,
    BitNot,
}
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum PostOp {
    Inc,
    Dec,
}
