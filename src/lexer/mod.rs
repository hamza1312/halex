use std::ops::Range;

use logos::{Lexer, Logos};

use crate::parser::BinOp;

#[derive(Debug, PartialEq, Eq, Clone, Copy, Logos)]
#[logos(skip r"[ \r\f]+")]
#[logos(skip r"#.*")]
pub enum LogosToken {
    #[regex(r#"\d+"#, priority = 2)]
    Int,
    #[regex(r#"((\d+(\.\d+)?)|((\.\d+))|(\.\d+))([Ee](\+|-)?\d+)?"#)]
    Float,
    #[regex(r#"[\p{L}a-zA-Z_][\p{L}\p{N}a-zA-Z0-9_]*"#)]
    Ident,
    #[regex(r#""([^"\\]|\\[\s\S])*""#)]
    String,

    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Times,
    #[token("/")]
    Slash,
    #[token("**")]
    Pow,
    #[token("or")]
    Or,
    #[token("and")]
    And,
    #[token("&")]
    BitAnd,
    #[token("|")]
    BitOr,
    #[token("^")]
    BitXor,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token(";")]
    Semicolon,
    #[token("->")]
    Arrow,
    #[token("fn")]
    KwFn,
    #[token("let")]
    KwLet,
    #[token("\n")]
    Newline,
    #[token("==")]
    Eqq,
    #[token("!=")]
    Neq,
    #[token("=")]
    Eq,
    #[token(">")]
    Gt,
    #[token("<")]
    Lt,
    #[token(">=")]
    Ge,
    #[token("<=")]
    Le,
    #[token("{")]
    LBrace,
    #[token("}")]
    RBrace,
    #[token("(")]
    LParen,
    #[token(")")]
    RParen,
    Eof,
    Error,
}
impl LogosToken {
    pub fn to_infix_op(&self) -> BinOp {
        match self {
            Self::Plus => BinOp::Add,
            Self::Minus => BinOp::Sub,
            Self::Times => BinOp::Mul,
            Self::Slash => BinOp::Div,
            Self::Pow => BinOp::Pow,
            Self::Eqq => BinOp::Eq,
            Self::Neq => BinOp::Neq,
            Self::Lt => BinOp::Lt,
            Self::Gt => BinOp::Gt,
            Self::Le => BinOp::Le,
            Self::Ge => BinOp::Ge,
            Self::And => BinOp::And,
            Self::Or => BinOp::Or,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone)]
pub struct Token {
    pub kind: LogosToken,
    pub span: Range<usize>,
}

impl Token {
    pub fn is_eof(&self) -> bool {
        if self.kind == LogosToken::Eof {
            return true;
        }
        false
    }
    pub fn len(&self) -> usize {
        (self.span.end - self.span.start) as usize
    }

    pub fn text<'input>(&self, input: &'input str) -> &'input str {
        &input[self.span.clone()]
    }
}

pub struct LLexer<'a> {
    generated: logos::SpannedIter<'a, LogosToken>,
    current: Option<Token>,
    pos: usize,
    eof: bool,
}

impl<'a> Iterator for LLexer<'a> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        self.pos += 1;
        let token = match self.generated.next() {
            Some((token, span)) => {
                let token = token.unwrap_or(LogosToken::Error);
                Some(Token { kind: token, span })
            }
            None if self.eof => None,
            None => {
                self.eof = true;
                Some(Token {
                    kind: LogosToken::Eof,
                    span: self.span(),
                })
            }
        };
        self.current = token.clone();

        token
    }
}
impl<'a> LLexer<'a> {
    pub fn new(input: &'a str) -> Self {
        Self {
            generated: LogosToken::lexer(input).spanned(),
            eof: false,
            pos: 0,
            current: None,
        }
    }
    pub fn span(&self) -> Range<usize> {
        self.generated.span()
    }
    pub fn current(&self) -> Option<Token> {
        self.current.clone()
    }
    pub fn pos(&self) -> usize {
        self.pos
    }
    pub fn tokenize(&mut self) -> Vec<Token> {
        self.collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn numbers() {
        let mut tokens = LogosToken::lexer("1000 10.5");
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Int)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Float)));
    }
    #[test]
    fn operators() {
        let mut tokens = LogosToken::lexer("+ - * / ** and or ^ & |");
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Plus)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Minus)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Times)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Slash)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Pow)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::And)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::Or)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::BitXor)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::BitAnd)));
        assert_eq!(tokens.next(), Some(Ok(LogosToken::BitOr)));
    }
}
