
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
    Decl(Box<Decl>),
    Expr(Box<Expr>),
    VarAssign(Box<Ident>, Box<Expr>),
    ConstAssign(Box<Ident>, Box<Expr>),
    If(Box<Expr>, Box<Block>, Option<Box<Block>>),
    While(Box<Expr>, Box<Block>) 
}

#[derive(Debug, Clone, PartialEq)]
pub enum Decl {
    Method(Ident, Vec<Ident>, Box<Block>),
    Class(Box<Ident>, Vec<Decl>)
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Var(Box<Ident>),
    Const(Box<Ident>),
    Binary(BinOp, Box<Expr>, Box<Expr>),
    Unary(UnOp, Box<Expr>),
    Message(Option<Box<Expr>>, Ident, Vec<Expr>),   
    Array(Vec<Box<Expr>>),
    Map(Vec<(Box<Expr>, Box<Expr>)>),
    Literal(Box<Literal>)
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
