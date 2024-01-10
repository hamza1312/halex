use std::collections::HashMap;

use crate::compiler::mir::*;

use crate::parser::{BinOp as PBinOp, Expr, ExprKind, Type as PType};

pub fn lower_ast(ast: Vec<Expr>) -> Vec<Mir> {
    let mut handler = LowerHandler::new(ast);
    let m = handler.lower_ast();
    println!("{:?}", m);
    m
}

pub struct LowerHandler {
    ast: Vec<Expr>,
    id_counter: usize,
    names: HashMap<String, usize>,
}
impl LowerHandler {
    pub fn new(ast: Vec<Expr>) -> Self {
        println!("{:?}", &ast);
        Self {
            ast,
            id_counter: 0,
            names: HashMap::new(),
        }
    }
    fn convert_type(&self, ty: PType) -> Type {
        match ty {
            PType::I64 => Type::I64,
            PType::I32 => Type::I32,
            PType::F64 => Type::F64,
            PType::F32 => Type::F32,
            PType::Str => Type::Str,
            PType::Unit => Type::Unit,
            PType::Pointer(x) => Type::Pointer(Box::new(self.convert_type(*x))),
            PType::Invalid => unreachable!(),
            _ => todo!(),
        }
    }
    fn lower_expr(&mut self, expr: Expr) -> Mir {
        match expr.inner {
            ExprKind::Int(i) => Mir::Lit(Literal::Int(i)),
            ExprKind::Float(f) => Mir::Lit(Literal::Float(f)),
            ExprKind::String(str) => Mir::Lit(Literal::Str(str)),
            ExprKind::Bool(b) => Mir::Lit(Literal::Bool(b)),
            ExprKind::Ref(x) => Mir::Ref(Box::new(self.lower_expr(*x))),
            ExprKind::Deref(x) => Mir::Deref(Box::new(self.lower_expr(*x))),
            ExprKind::Unit => Mir::Unit,
            ExprKind::ExternFunction(name, params, return_type, varadic) => {
                let mut types = Vec::new();
                for p in params {
                    types.push(self.convert_type(p))
                }
                let return_ty = if return_type.is_some() {
                    self.convert_type(return_type.unwrap())
                } else {
                    Type::Unit
                };
                Mir::ExternFunction {
                    name,
                    params: types,
                    is_varadic: varadic,
                    return_type: return_ty,
                }
            }
            ExprKind::Infix(left, op, right) => {
                let l = self.lower_expr(*left);
                let r = self.lower_expr(*right);
                let op = match op {
                    PBinOp::Add => BinOp::Add,
                    PBinOp::Sub => BinOp::Sub,
                    PBinOp::Mul => BinOp::Mul,
                    PBinOp::Div => BinOp::Div,
                    PBinOp::Eq => BinOp::Eq,
                    PBinOp::Neq => BinOp::Neq,
                    PBinOp::Lt => BinOp::Lt,
                    PBinOp::Gt => BinOp::Gt,
                    PBinOp::Le => BinOp::Le,
                    PBinOp::Ge => BinOp::Ge,
                    PBinOp::And => BinOp::And,
                    PBinOp::Or => BinOp::Or,
                };
                Mir::Binary(Box::new(l), op, Box::new(r))
            }
            ExprKind::Let(name, ty, value) => {
                let id = self.id_counter;
                self.names.insert(name, id);
                self.id_counter += 1;
                let ty = self.convert_type(ty);
                let value = self.lower_expr(*value);
                Mir::Let(id, ty, Box::new(value))
            }
            ExprKind::Ident(x) => {
                let id = self.names.get(&x).unwrap();
                Mir::GetVar(*id)
            }
            ExprKind::Call(name, args) => {
                let args = args
                    .iter()
                    .map(|e| self.lower_expr(e.clone()))
                    .collect::<Vec<_>>();
                Mir::Call(name, args)
            }
            _ => todo!(),
        }
    }
    pub fn lower_ast(&mut self) -> Vec<Mir> {
        let mut exprs = Vec::new();
        for expr in self.ast.clone() {
            exprs.push(self.lower_expr(expr))
        }
        exprs
    }
}
