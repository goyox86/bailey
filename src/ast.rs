#[derive(Debug, Clone)]
pub enum Node {
    Identifier(String),
    Constant(String),
    IntegerLiteral(u32),
    FloatLiteral(f32),
    StringLiteral(String),
    BinaryExpr(BinOp, PNode, PNode),
    AssingExpr(PNode, PNode),
    ClassDecl(PNode, Vec<PNode>),
    IfStatement(PNode, Vec<PNode>, Option<Vec<PNode>>),
    MethodDecl(PNode, Option<Vec<PNode>>, Vec<PNode>),
    WhileStatement(PNode, Vec<PNode>),
    Message(Option<PNode>, PNode, Option<Vec<PNode>>),
    Block(Vec<PNode>)

}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add, Sub, Mul, Div, Lt, Lte, Gt, Gte, Ne, Eq, And, Or
}

pub type PNode = Box<Node>;
