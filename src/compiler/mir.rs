#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Str,
    Unit,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

type Ident = u64;
#[derive(Debug, Clone, PartialEq)]
pub enum Mir {
    Lit(Literal),
    Binary(Box<Mir>, BinOp, Box<Mir>),
    Let(Ident, Type, Box<Mir>),
    ExternFunction {
        name: String,
        params: Vec<Type>,
        return_type: Type,
    },
    FunctionDef {
        name: String,
        params: Vec<(Ident, Type)>,
        return_type: Type,
        exprs: Vec<Mir>,
    },
    Call(String, Vec<Mir>),
    GetVar(u64),
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Eq,
    Neq,
    Lt,
    Gt,
    Le,
    Ge,
    Pow,
    And,
    Or,
    BitXor,
    BitOr,
}
