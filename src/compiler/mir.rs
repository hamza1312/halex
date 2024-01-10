#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    I64,
    I32,
    F64,
    F32,
    Str,
    Void,
    Unit,
    Pointer(Box<Type>),
}
impl Type {
    pub fn is_int(&self) -> bool {
        use Type::*;
        match self {
            F64 | F32 => false,
            _ => true,
        }
    }
    pub fn is_void(&self) -> bool {
        use Type::*;
        match self {
            Void => true,
            _ => false,
        }
    }
    pub fn get_ty_from_ptr(&self) -> Type {
        match self {
            Self::Pointer(x) => x.as_ref().to_owned(),
            _ => panic!("Expected pointer"),
        }
    }
    pub fn is_unit(&self) -> bool {
        use Type::*;
        match self {
            Unit => true,
            _ => false,
        }
    }
    pub fn is_float(&self) -> bool {
        use Type::*;
        match self {
            F64 | F32 => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Unit,
}

type Ident = usize;
#[derive(Debug, Clone, PartialEq)]
pub enum Mir {
    Lit(Literal),
    Deref(Box<Mir>),
    Binary(Box<Mir>, BinOp, Box<Mir>),
    Let(Ident, Type, Box<Mir>),
    Ref(Box<Mir>),
    ExternFunction {
        name: String,
        params: Vec<Type>,
        return_type: Type,
        is_varadic: bool,
    },
    FunctionDef {
        name: String,
        params: Vec<(Ident, Type)>,
        return_type: Type,
        exprs: Vec<Mir>,
    },
    Call(String, Vec<Mir>),
    GetVar(Ident),
    Unit,
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
