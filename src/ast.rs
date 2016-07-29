
#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Lte,
    Gt,
    Gte,
    Ne,
    Eq,
    And,
    Or,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnOp {
    Not,
    Neg
}

#[derive(Debug, Clone, PartialEq)]
pub enum Stmt {
    Decl(P<Decl>),
    Expr(P<Expr>),
    VarAssign(P<Ident>, P<Expr>),
    ConstAssign(P<Ident>, P<Expr>),
    If(P<Expr>, P<Block>, Option<P<Block>>),
    While(P<Expr>, P<Block>),
    Block(P<Block>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Method(P<Ident>, Vec<Ident>, P<Block>),
    Class(P<Ident>, Vec<Decl>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(P<Ident>),
    Const(P<Ident>),
    Binary(BinOp, P<Expr>, P<Expr>),
    Unary(UnOp, P<Expr>),
    Message(Option<P<Expr>>, P<Ident>, Vec<Expr>),
    Array(Vec<P<Expr>>),
    Map(Vec<(P<Expr>, P<Expr>)>),
    Literal(P<Literal>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Integer(i64),
    Float(f64),
    Str(String),
    Bool(bool)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub stmts: Vec<Stmt>
}

#[derive(Debug, Clone, PartialEq)]
pub enum Ident {
    Const(String),
    Var(String),
    Meth(String)
}

pub type P<T> = Box<T>;

pub fn P<T>(value: T) -> P<T> {
    Box::new(value)
}
