use crate::lexer::{LLexer, LogosToken, Token};
use miette::{miette, LabeledSpan};
use std::ops::Range;

/// Binary operator used for infix operations (1 + 1) etc
#[derive(Debug, Clone)]
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
    And,
    Or,
}

#[derive(Debug, Clone)]
pub enum Type {
    I64,
    F64,
    I32,
    F32,
    Bool,
    Str,
    Unit,
    Pointer(Box<Type>),
    Invalid,
}

#[derive(Debug, Clone)]
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Ident(String),
    Bool(bool),
    String(String),
    Call(String, Vec<Expr>),
    Return(Box<Expr>),
    Function(String, Vec<(String, Type)>, Option<Type>, Vec<Expr>),
    ExternFunction(String, Vec<Type>, Option<Type>, bool),
    Assign {
        var: String,
        value: Box<Expr>,
        deref: bool,
    },
    Let(String, Type, Box<Expr>),
    Ref(Box<Expr>),
    Deref(Box<Expr>),
    Infix(Box<Expr>, BinOp, Box<Expr>),
    Unit,
    Error,
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub inner: ExprKind,
    pub span: Range<usize>,
}

impl Expr {
    pub fn new(span: Range<usize>, inner: ExprKind) -> Self {
        Self { inner, span }
    }
    fn boxed(self) -> Box<Self> {
        Box::new(self)
    }
}

pub struct Parser<'a> {
    input: &'a str,
    lexer: LLexer<'a>,
    errored: bool,
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: LLexer::new(source),
            input: source,
            errored: false,
        }
    }

    fn current(&mut self) -> Token {
        let tok = self.lexer.current().unwrap_or(Token {
            span: self.lexer.span(),
            kind: LogosToken::Eof,
        });
        tok
    }

    fn text(&self, tok: &Token) -> &'a str {
        tok.text(&self.input)
    }

    fn next(&mut self) -> Token {
        self.lexer.next().unwrap_or(Token {
            span: self.lexer.span(),
            kind: LogosToken::Eof,
        })
    }

    fn skip_separator(&mut self) {
        loop {
            let c = self.current();
            if c.kind == LogosToken::Newline || c.kind == LogosToken::Semicolon {
                self.next();
                continue;
            }
            break;
        }
    }

    fn infix_binding_power(&self, op: &LogosToken) -> Option<(u8, u8)> {
        use LogosToken::*;
        Some(match op {
            Or => (1, 2),
            And => (3, 4),
            Eqq | Neq => (5, 6),
            Gt | Lt | Ge | Le => (7, 8),
            Plus | Minus => (9, 10),
            Times | Slash => (11, 12),
            _ => return None,
        })
    }
    fn expect(&mut self, token: LogosToken, message: &'a str) {
        if self.current().kind != token {
            let span = self.lexer.span();
            self.err(message, span);
        }
    }
    fn expect_next(&mut self, token: LogosToken, message: &'a str) {
        if self.current().kind != token {
            let span = self.lexer.span();
            self.err(message, span);
        }
        self.next();
    }
    fn err(&mut self, msg: &'a str, span: Range<usize>) -> Expr {
        let i = self.input.to_string();
        let report = miette!(
            labels = vec![LabeledSpan::at(span.clone(), msg)],
            "Parsing Error: {msg}"
        )
        .with_source_code(i);
        println!("{report:?}");
        self.errored = true;
        Expr::new(self.lexer.span(), ExprKind::Error)
    }

    /// Parse the input until EOF
    pub fn parse(&mut self) -> Vec<Expr> {
        self.lexer.next();
        let mut exprs: Vec<Expr> = Vec::new();
        self.skip_separator();
        loop {
            if self.current().is_eof() {
                break;
            }
            exprs.push(self.declaration());
            let c = self.current();
            // Validate the separators
            if c.kind != LogosToken::Newline
                && c.kind != LogosToken::Semicolon
                && c.kind != LogosToken::Eof
            {
                self.err("Expected <newline> or semicolon", self.lexer.span());
            } else {
                self.skip_separator();
            }
        }
        if self.errored {
            std::process::exit(1);
        }
        exprs
    }

    fn block(&mut self) -> Vec<Expr> {
        let mut exprs: Vec<Expr> = Vec::new();
        if self.current().kind == LogosToken::LBrace {
            self.next();
            self.skip_separator();
            loop {
                if self.current().kind == LogosToken::RBrace {
                    self.next();
                    break;
                }
                exprs.push(self.declaration());
                let c = self.current();
                // Validate the separators
                if c.kind != LogosToken::Newline
                    && c.kind != LogosToken::Semicolon
                    && c.kind != LogosToken::RBrace
                    && c.kind != LogosToken::Eof
                {
                    self.err("Expected <newline> or semicolon", self.lexer.span());
                } else {
                    self.skip_separator();
                }
            }
        } else {
            self.expect_next(LogosToken::Colon, "Expected colon for inline function");
            exprs.push(self.declaration())
        }
        exprs
    }
    fn declaration(&mut self) -> Expr {
        use LogosToken::*;
        match self.current().kind {
            KwLet => self.parse_let(),
            KwReturn => self.parse_return(),
            KwFn => self.parse_fn(),
            KwExtern => self.parse_extern(),
            _ => self.expr(0),
        }
    }
    fn parse_extern(&mut self) -> Expr {
        let start = self.lexer.span().start;
        self.next();
        self.expect_next(LogosToken::KwFn, "Expected `fn` keyword after extern");
        let name = self.current();
        let name = self.text(&name);
        self.expect_next(LogosToken::Ident, "Expected external function's name");
        let mut params: Vec<Type> = Vec::new();
        let mut varadic: bool = false;
        let mut return_type: Option<Type> = None;
        if self.current().kind == LogosToken::LParen {
            self.next();
            loop {
                if self.current().kind == LogosToken::RParen {
                    break;
                }
                if self.current().kind == LogosToken::Comma {
                    self.next();
                    continue;
                }
                if self.current().kind == LogosToken::Ellipsis {
                    break;
                }
                let ty = self.parse_type();
                params.push(ty);
            }
            if self.current().kind == LogosToken::Ellipsis {
                varadic = true;
                self.next();
            }
            self.expect_next(
                LogosToken::RParen,
                "Expected ) end after function parameters",
            )
        }

        if self.current().kind == LogosToken::Arrow {
            self.next();
            let ty = self.parse_type();
            return_type = Some(ty)
        }
        Expr::new(
            start..self.lexer.span().end,
            ExprKind::ExternFunction(name.to_string(), params, return_type, varadic),
        )
    }
    fn parse_return(&mut self) -> Expr {
        let start = self.lexer.span().start;
        self.next();
        let expr = self.expr(0);
        Expr::new(start..self.lexer.span().end, ExprKind::Return(expr.boxed()))
    }
    fn parse_fn(&mut self) -> Expr {
        let start = self.lexer.span().start;
        self.next();

        let c = self.current();
        let name = self.text(&c);

        self.expect_next(LogosToken::Ident, "Expected function's name");
        let mut params: Vec<(String, Type)> = Vec::new();
        let mut return_type: Option<Type> = None;
        if self.current().kind == LogosToken::LParen {
            self.next();
            loop {
                if self.current().kind == LogosToken::RParen {
                    break;
                }
                if self.current().kind == LogosToken::Comma {
                    self.lexer.next();
                    continue;
                }
                let c = self.current();
                let name = self.text(&c);
                self.expect_next(LogosToken::Ident, "Expected parameter name");
                self.expect_next(LogosToken::Colon, "Expected colon");
                let ty = self.parse_type();

                params.push((name.into(), ty.into()))
            }
            self.expect_next(LogosToken::RParen, "Expected )");
        }

        if self.current().kind == LogosToken::Arrow {
            self.next();
            let ty = self.parse_type();
            return_type = Some(ty)
        }
        let block = self.block();
        Expr::new(
            start..self.lexer.span().end,
            ExprKind::Function(name.to_string(), params, return_type, block),
        )
    }

    fn parse_let(&mut self) -> Expr {
        let start = self.lexer.span().start;
        self.next();
        let c = self.current();
        let name = self.text(&c);

        self.expect_next(LogosToken::Ident, "Expected variable name");

        self.expect_next(LogosToken::Colon, "Expected colon");
        let ty = self.parse_type();
        self.expect_next(LogosToken::Eq, "Expected =");
        let expr = self.expr(0);

        Expr::new(
            start..self.lexer.span().end,
            ExprKind::Let(name.to_string(), ty, expr.boxed()),
        )
    }

    fn expr(&mut self, min_bp: u8) -> Expr {
        let span = self.lexer.span();

        let mut lhs = self.term();
        loop {
            let op = self.current();
            if op.is_eof() {
                break;
            }

            if let Some((l_bp, r_bp)) = self.infix_binding_power(&op.kind) {
                if l_bp < min_bp {
                    break;
                }
                self.lexer.next();
                let rhs = self.expr(r_bp);
                lhs = Expr::new(
                    span.start..self.lexer.span().end,
                    ExprKind::Infix(lhs.boxed(), op.kind.to_infix_op(), rhs.boxed()),
                );
                continue;
            }
            break;
        }
        lhs
    }

    fn parse_type(&mut self) -> Type {
        let token = self.current();
        let span = token.span.clone();
        let kind = token.kind;
        match kind {
            LogosToken::Times => {
                self.next();
                let t = self.parse_type();
                Type::Pointer(Box::new(t))
            }
            LogosToken::Ident => {
                let text = self.text(&token);
                println!("{:?}", text);
                self.next();
                match text {
                    "i64" => Type::I64,
                    "i32" => Type::F32,
                    "f64" => Type::F64,
                    "f32" => Type::F32,
                    "bool" => Type::Bool,
                    "str" => Type::Str,
                    _ => {
                        // We'll add user types later
                        self.err("Invalid type", span);
                        Type::Invalid
                    }
                }
            }
            LogosToken::LParen => {
                self.next();
                self.expect_next(LogosToken::RParen, "Expected )");
                Type::Unit
            }
            _ => {
                println!("{:?}", kind);
                self.err("Invalid type", span);
                Type::Invalid
            }
        }
    }

    fn term(&mut self) -> Expr {
        let token = self.current();
        let kind = token.kind;
        let span = token.span.clone();
        let expr = match kind {
            LogosToken::Int => {
                let int: i64 = self.text(&token).parse().unwrap();
                Expr::new(span.start..self.lexer.span().end, ExprKind::Int(int))
            }
            LogosToken::True => Expr::new(span.start..self.lexer.span().end, ExprKind::Bool(true)),
            LogosToken::False => {
                Expr::new(span.start..self.lexer.span().end, ExprKind::Bool(false))
            }
            LogosToken::Float => {
                let float: f64 = self.text(&token).parse().unwrap();
                Expr::new(span.start..self.lexer.span().end, ExprKind::Float(float))
            }

            LogosToken::String => {
                let mut str = self.text(&token).to_string();
                str.remove(0);
                str.remove(str.len() - 1);
                Expr::new(span.start..self.lexer.span().end, ExprKind::String(str))
            }

            LogosToken::Ident => {
                let ident = self.text(&token).to_string();
                self.next();
                if self.current().kind == LogosToken::LParen {
                    self.next();
                    let mut exprs: Vec<Expr> = Vec::new();
                    loop {
                        if self.current().kind == LogosToken::RParen {
                            break;
                        }
                        if self.current().kind == LogosToken::Comma {
                            self.next();
                            continue;
                        }

                        exprs.push(self.expr(0))
                    }
                    self.expect(LogosToken::RParen, "Expected ) at the end of function call");
                    self.lexer.next();
                    return Expr::new(
                        span.start..self.lexer.span().end,
                        ExprKind::Call(ident, exprs),
                    );
                } else if self.current().kind == LogosToken::Eq {
                    self.next();
                    let value = self.expr(0);
                    return Expr::new(
                        span.start..self.lexer.span().end,
                        ExprKind::Assign {
                            var: ident,
                            value: value.boxed(),
                            deref: false,
                        },
                    );
                }
                return Expr::new(span.start..self.lexer.span().end, ExprKind::Ident(ident));
            }
            LogosToken::BitAnd => {
                self.next();
                let expr = self.expr(0);
                return Expr::new(
                    span.start..self.lexer.span().end,
                    ExprKind::Ref(expr.boxed()),
                );
            }

            LogosToken::Times => {
                self.next();
                let expr = self.expr(0);
                return Expr::new(
                    span.start..self.lexer.span().end,
                    ExprKind::Deref(expr.boxed()),
                );
            }
            LogosToken::LParen => {
                self.next();
                if self.current().kind != LogosToken::RParen {
                    let expr = self.expr(0);
                    self.expect(LogosToken::RParen, "Expected ) after expression");
                    Expr::new(span.start..self.lexer.span().end, expr.inner)
                } else {
                    Expr::new(span.start..self.lexer.span().end, ExprKind::Unit)
                }
            }

            _ => self.err("Expected valid expression", span),
        };
        self.next();
        expr
    }
}
