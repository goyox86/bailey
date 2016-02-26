#[derive(Debug, Clone)]
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

#[derive(Debug, Clone)]
pub enum Node {
    Ident(String),
    Const(String),
    IntLit(u64),
    FltLit(f64),
    StrLit(String),
    ArrLit(Vec<PNode>),
    MapLit(Vec<(PNode, PNode)>),
    Block(Vec<PNode>),
    AssignExpr {
        var: PNode,
        expr: PNode,
    },
    BinExpr {
        op: BinOp,
        left: PNode,
        right: PNode,
    },
    Message {
        recv: Option<PNode>,
        meth: PNode,
        args: Vec<PNode>,
    },
    WhileStmt {
        cond: PNode,
        blk: PNode,
    },
    IfStmt {
        cond: PNode,
        true_blk: PNode,
        false_blk: Option<PNode>,
    },
    ClassDecl {
        class: PNode,
        meths: Vec<PNode>,
    },
    MethodDecl {
        meth: PNode,
        params: Vec<PNode>,
        blk: PNode,
    },
}

pub type PNode = Box<Node>;

#[allow(non_snake_case)]
pub fn PNode(value: Node) -> PNode {
    Box::new(value)
}
