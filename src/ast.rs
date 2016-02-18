
#[derive(Debug, Clone)]
pub enum Node {
    Identifier { name: String },
    Constant { name: String },
    IntegerLiteral { value: u32 },
    FloatLiteral { value: f32 },
    StringLiteral { value: String },
    BinaryExpression { op: BinOp, left: PNode, right: PNode },
    AssingExpression { identifier: PNode, expression: PNode },
    ClassDeclaration { identifier: PNode, methods: Vec<PNode> },
    MethodDeclaration { method: PNode, params: Option<Vec<PNode>>, block: PNode },
    IfStatement { condition: PNode, true_block: PNode, false_block: Option<PNode> },
    WhileStatement { condition: PNode, block: PNode },
    Message { receiver: Option<PNode>, method: PNode, args: Option<Vec<PNode>> },
    Block { instructions: Vec<PNode> }
}

#[derive(Debug, Clone)]
pub enum BinOp {
    Add, Sub, Mul, Div, Lt, Lte, Gt, Gte, Ne, Eq, And, Or
}

pub type PNode = Box<Node>;

#[allow(non_snake_case)]
pub fn PNode(value: Node) -> PNode {
    Box::new(value)
}
