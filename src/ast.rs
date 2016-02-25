#[derive(Debug, Clone)]
pub struct Message {
    pub receiver: Option<PNode>,
    pub method: PNode,
    pub args: Vec<PNode>,
}

#[derive(Debug, Clone)]
pub struct Block {
    pub instructions: Vec<PNode>,
}

#[derive(Debug, Clone)]
pub struct Class {
    pub identifier: PNode,
    pub methods: Vec<PNode>,
}

#[derive(Debug, Clone)]
pub struct Method {
    pub method: PNode,
    pub params: Vec<PNode>,
    pub block: PNode,
}

#[derive(Debug, Clone)]
pub struct If {
    pub condition: PNode,
    pub true_block: PNode,
    pub false_block: Option<PNode>,
}

#[derive(Debug, Clone)]
pub struct While {
    pub condition: PNode,
    pub block: PNode,
}

#[derive(Debug, Clone)]
pub struct Assignment {
    pub identifier: PNode,
    pub expression: PNode,
}

#[derive(Debug, Clone)]
pub struct Identifier {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct Constant {
    pub name: String,
}

#[derive(Debug, Clone)]
pub struct IntegerLiteral {
    pub value: u32,
}

#[derive(Debug, Clone)]
pub struct FloatLiteral {
    pub value: f32,
}

#[derive(Debug, Clone)]
pub struct StringLiteral {
    pub value: String,
}

#[derive(Debug, Clone)]
pub struct ArrayLiteral {
    pub elements: Vec<PNode>,
}

#[derive(Debug, Clone)]
pub struct MapLiteral {
    pub entries: Vec<(PNode, PNode)>,
}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub op: BinOp,
    pub left: PNode,
    pub right: PNode,
}

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
    IdentExpr(Identifier),
    ConstExpr(Constant),
    IntExpr(IntegerLiteral),
    FltExpr(FloatLiteral),
    StrExpr(StringLiteral),
    ArrExpr(ArrayLiteral),
    MapExpr(MapLiteral),
    BinExpr(BinaryExpression),
    AssignExpr(Assignment),
    MessageExpr(Message),
    BlockExpr(Block),
    WhileStmt(While),
    IfStmt(If),
    ClassDecl(Class),
    MethodDecl(Method),
}

pub type PNode = Box<Node>;

#[allow(non_snake_case)]
pub fn PNode(value: Node) -> PNode {
    Box::new(value)
}
