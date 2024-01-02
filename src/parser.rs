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
    Pow,
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
pub enum ExprKind {
    Int(i64),
    Float(f64),
    Ident(String),
    Infix(Box<Expr>, BinOp, Box<Expr>),
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
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            lexer: LLexer::new(source),
            input: source,
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
            Pow => (22, 21),
            _ => return None,
        })
    }

    fn err(&self, msg: &'a str, span: Range<usize>) -> Expr {
        let i = self.input.to_string();
        let report = miette!(
            labels = vec![LabeledSpan::at(span.clone(), msg)],
            "Parsing Error: {msg}"
        )
        .with_source_code(i);
        println!("{report:?}");
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
        exprs
    }

    fn declaration(&mut self) -> Expr {
        match self.current() {
            _ => self.expr(0),
        }
    }

    fn expr(&mut self, min_bp: u8) -> Expr {
        let span = self.lexer.span();

        let mut lhs = self.term();
        loop {
            let op = self.next();
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
    fn term(&mut self) -> Expr {
        let token = self.current();
        let kind = token.kind;
        let span = token.span.clone();
        match kind {
            LogosToken::Int => {
                let int: i64 = self.text(&token).parse().unwrap();
                Expr::new(span.start..self.lexer.span().end, ExprKind::Int(int))
            }

            LogosToken::Float => {
                let float: f64 = self.text(&token).parse().unwrap();
                Expr::new(span.start..self.lexer.span().end, ExprKind::Float(float))
            }

            LogosToken::Ident => {
                let ident = self.text(&token).to_string();
                Expr::new(span.start..self.lexer.span().end, ExprKind::Ident(ident))
            }
            _ => self.err("Expected valid expression", span),
        }
    }
}
