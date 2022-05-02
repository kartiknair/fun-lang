use derive_more::{From, TryInto};

use crate::{common::Span, token};

#[derive(Debug, Clone)]
pub struct FunLit {
    pub parameters: Vec<token::Token>,
    pub body: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarDecl {
    pub ident: token::Token,
    pub init: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IfExpr {
    pub condition: Box<Expr>,
    pub if_block: Block,
    pub elif_stmts: Vec<(Expr, Block)>,
    pub else_block: Option<Block>,
}

#[derive(Debug, Clone)]
pub struct WhileStmt {
    pub condition: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct ForStmt {
    pub ident: token::Token,
    pub iterable: Box<Expr>,
    pub block: Block,
}

#[derive(Debug, Clone)]
pub struct ReturnStmt {
    pub value: Box<Expr>,
    pub err: bool,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub stmts: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub op: token::Token,
    pub expr: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub op: token::Token,
    pub left: Box<Expr>,
    pub right: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct IdxExpr {
    pub target: Box<Expr>,
    pub idx: Box<Expr>,
}

#[derive(Debug, Clone)]
pub struct VarExpr {
    pub ident: token::Token,
}

#[derive(Debug, Clone)]
pub struct CallExpr {
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct CatchExpr {
    pub target: Box<Expr>,
    pub callback: FunLit,
}

#[derive(Debug, Clone)]
pub struct ObjectLit {
    pub inits: Vec<(token::Token, Expr)>,
}

#[derive(Debug, Clone)]
pub struct ArrLit {
    pub elements: Vec<Expr>,
}

#[derive(Debug, Clone)]
pub struct Lit {
    pub token: token::Token,
}

#[derive(Debug, Clone, From, TryInto)]
pub enum ExprKind {
    // Statements that evaluate to null
    FunLit(FunLit),
    VarDecl(VarDecl),
    ForStmt(ForStmt),
    WhileStmt(WhileStmt),
    ReturnStmt(ReturnStmt),

    // Statement looking things that evaluate to values
    If(IfExpr),
    Block(Block),

    // Regular ol' expressions
    Unary(UnaryExpr),
    Binary(BinaryExpr),
    Idx(IdxExpr),
    Var(VarExpr),
    Call(CallExpr),
    Catch(CatchExpr),
    ObjectLit(ObjectLit),
    ArrLit(ArrLit),
    Lit(Lit),
}

#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub struct File {
    pub source: Vec<char>,
    pub exprs: Vec<Expr>,
}

impl File {
    pub fn lexeme(&self, span: &Span) -> String {
        self.source[span.clone()].iter().collect()
    }
}
