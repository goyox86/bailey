use oak_runtime::stream::*;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

use ast::PNode;

grammar! bailey {
    use ast::Assignment;
    use ast::Block;
    use ast::Class;
    use ast::Constant;
    use ast::Identifier;
    use ast::If;
    use ast::IntegerLiteral;
    use ast::FloatLiteral;
    use ast::StringLiteral;
    use ast::ArrayLiteral;
    use ast::MapLiteral;
    use ast::BinaryExpression;
    use ast::Method;
    use ast::Message;
    use ast::While;
    use ast::Node::*;
    use ast::BinOp;
    use ast::BinOp::*;
    use ast::PNode;
    use std::str::FromStr;

    program
        = ((expr / decl / stmt) terminator?)* > program

    decl
        = class_decl
        / method_decl

    class_decl
        = kw_class constant class_body > class_decl

    class_body
        = lbracket method_decl* rbracket

    method_decl
        = kw_def identifier lparen param_list rparen block > method_decl

    block
        = lbracket ((stmt / expr) terminator?)* rbracket > block

    stmt
        = if_stmt
        / while_stmt

    if_stmt
        = kw_if expr block (kw_else block)? > if_stmt

    while_stmt
        = kw_while expr block > while_stmt

    function_call
        = identifier lparen arg_list rparen

    method_call
        = callable_expr (dot function_call)+

    assign_expr
        = identifier bind_op expr > assign_expr

    expr
        = cond_expr

    cond_expr
        = add_expr (cond_expr_op add_expr)* > fold_left

    add_expr
        = mult_expr (add_expr_op mult_expr)* > fold_left

    mult_expr
        = primary_expr (mult_expr_op primary_expr)* > fold_left

    primary_expr
        = assign_expr
          / method_call > method_call
          / callable_expr

    callable_expr
        = function_call > function_call
        / literal
        / constant
        / identifier
        / lparen expr rparen

    arg_list
        = empty_list
        / expr (comma expr)* > arg_list

    param_list
        = empty_list
        / identifier (comma identifier)* > param_list

    empty_list = &rparen > empty_list

    digit = ["0-9"]
    identifier_char = ["A-Za-z0-9_"]

    identifier = !digit !keyword identifier_char+ spacing > identifier
    constant = !keyword !digit ["A-Z"]+ identifier_char+ spacing > constant

    literal
        = lit_string
        / lit_float
        / lit_int
        / lit_array
        / lit_map

    lit_int = digit+ spacing > lit_int
    lit_float = digit+ "." digit+ spacing > lit_float
    lit_string = (dbl_quot_lit_string / sng_quot_lit_string) spacing > lit_string
    lit_array = lsqbracket expr ("," expr)* rsqbracket > lit_array
    lit_map = lbracket (expr ":" expr)* rbracket > lit_map

    dbl_quot_lit_string = dbl_quot (spacing_char / !dbl_quot .)* dbl_quot
    sng_quot_lit_string = sng_quot (spacing_char / !sng_quot .)* sng_quot

    spacing_char = [" \n\r\t"]
    spacing = spacing_char* -> ()

    kw_class = "class" spacing
    kw_def = "def" spacing
    kw_else = "else" spacing
    kw_false = "false" spacing
    kw_if = "if" spacing
    kw_nil = "nil" spacing
    kw_true = "true" spacing
    kw_while = "while" spacing

    keyword
        = kw_def
        / kw_if
        / kw_else
        / kw_class
        / kw_true
        / kw_false
        / kw_nil

    add_expr_op
        = add_op > add_bin_op
        / sub_op > sub_bin_op

    mult_expr_op
        = mul_op > mul_bin_op
        / div_op > div_bin_op

    cond_expr_op
        = lt_op > lt_bin_op /
        lte_op > lte_bin_op /
        gt_op > gt_bin_op /
        gte_op > gte_bin_op /
        ne_op > ne_bin_op /
        eq_op > eq_bin_op /
        andand_op > and_bin_op /
        oror_op > or_bin_op

    bind_op = "=" spacing
    add_op = "+" spacing
    sub_op = "-" spacing
    mul_op = "*" spacing
    div_op = "/" spacing
    lt_op = "<" spacing
    lte_op = "<=" spacing
    gt_op = ">" spacing
    gte_op = ">=" spacing
    ne_op = "!=" spacing
    eq_op = "==" spacing
    andand_op  = "&&" spacing
    oror_op = "||" spacing

    lparen = "(" spacing
    rparen = ")" spacing
    lbracket = "{" spacing
    rbracket = "}" spacing
    lsqbracket = "[" spacing
    rsqbracket = "]" spacing
    dbl_quot = "\"" spacing
    sng_quot = "'" spacing
    terminator = ";" spacing
    dot = "." spacing
    comma = "," spacing

    fn lit_int(raw_text: Vec<char>) -> PNode {
        PNode(IntExpr(IntegerLiteral { value: to_number(raw_text) }))
    }

    // FIXME: Please remove this hacky wacky '.' thingy
    fn lit_float(mut integer: Vec<char>, fractional: Vec<char>) -> PNode {
        integer.push('.');
        PNode(FltExpr(FloatLiteral { value: to_float(combine(integer, fractional)) }))
    }

    fn lit_string(raw_text: Vec<char>) -> PNode {
        PNode(StrExpr(StringLiteral { value: to_string(raw_text) }))
    }

    fn lit_array(first: PNode, rest: Vec<PNode>) -> PNode {
        PNode(ArrExpr(ArrayLiteral { elements: combine_one_with_many(first, rest) }))
    }

    fn lit_map(entries: Vec<(PNode, PNode)>) -> PNode {
        PNode(MapExpr(MapLiteral { entries: entries }))
    }

    fn constant(first: Vec<char>, rest: Vec<char>) -> PNode {
        PNode(ConstExpr(Constant { name: to_string(combine(first, rest)) }))
    }

    fn identifier(raw_text: Vec<char>) -> PNode {
        PNode(IdentExpr(Identifier { name: to_string(raw_text) }))
    }

    fn add_bin_op() -> BinOp { Add }
    fn sub_bin_op() -> BinOp { Sub }
    fn mul_bin_op() -> BinOp { Mul }
    fn div_bin_op() -> BinOp { Div }
    fn lt_bin_op() -> BinOp { Lt }
    fn lte_bin_op() -> BinOp { Lte }
    fn gt_bin_op() -> BinOp { Gt }
    fn gte_bin_op() -> BinOp { Gte }
    fn ne_bin_op() -> BinOp { Ne }
    fn eq_bin_op() -> BinOp { Eq }
    fn and_bin_op() -> BinOp { And }
    fn or_bin_op() -> BinOp { Or }

    fn function_call(method: PNode, args: Vec<PNode>) -> PNode {
        PNode(MessageExpr(Message { receiver: None, method: method, args: args }))
    }

    fn method_call(receiver: PNode, call: Vec<(PNode, Vec<PNode>)>) -> PNode {
        call.into_iter().fold(receiver, { |accu, (method, args)|
            PNode(MessageExpr(Message { receiver: Some(accu), method: method, args: args }))
        })
    }

    fn assign_expr(identifier: PNode, expr: PNode) -> PNode {
        PNode(AssignExpr(Assignment { identifier: identifier, expression: expr }))
    }

    fn class_decl(class_name: PNode, methods: Vec<PNode>) -> PNode {
        PNode(ClassDecl(Class { identifier: class_name, methods: methods }))
    }

    fn method_decl(method: PNode, params: Vec<PNode>, block: PNode) -> PNode {
        PNode(MethodDecl(Method { method: method, params: params, block: block }))
    }

    fn block(instructions: Vec<(PNode, Option<()>)>) -> PNode {
        let instructions = instructions.into_iter().map(|expr| {
            match expr { (e, _) => e }
        }).collect();

        PNode(BlockExpr(Block {instructions: instructions}))
    }

    fn program(instructions: Vec<(PNode, Option<()>)>) -> Vec<PNode> {
        instructions.into_iter().map(|inst| {
            match inst { (i, _) => i }
        }).collect()
    }

    fn if_stmt(condition: PNode, true_block: PNode, false_block: Option<PNode>) -> PNode {
        PNode(IfStmt(If { condition: condition, true_block: true_block, false_block: false_block }))
    }

    fn while_stmt(condition: PNode,  block: PNode) -> PNode {
      PNode(WhileStmt(While { condition: condition, block: block }))
    }

    fn arg_list(first: PNode, mut rest: Vec<PNode>) -> Vec<PNode> {
        combine_one_with_many(first, rest)
    }

    fn param_list(first: PNode, mut rest: Vec<PNode>) -> Vec<PNode> {
        combine_one_with_many(first, rest)
    }

    fn empty_list() -> Vec<PNode> {
        vec![]
    }

    fn to_string(raw_text: Vec<char>) -> String {
        raw_text.into_iter().collect()
    }

    fn to_number(raw_text: Vec<char>) -> u32 {
        u32::from_str(&*to_string(raw_text)).unwrap()
    }

    fn to_float(raw_text: Vec<char>) -> f32 {
        f32::from_str(&*to_string(raw_text)).unwrap()
    }

    fn fold_left(head: PNode, rest: Vec<(BinOp, PNode)>) -> PNode {
        rest.into_iter().fold(head,
          |accu, (op, expr)| PNode(BinExpr(BinaryExpression { op: op, left: accu, right: expr })))
    }

    fn fold_right(front: Vec<(PNode, BinOp)>, last: PNode) -> PNode {
        front.into_iter().rev().fold(last,
          |accu, (expr, op)| PNode(BinExpr(BinaryExpression { op: op, left: expr, right: accu })))
    }

    fn combine<T: Clone>(mut left: Vec<T>, right: Vec<T>) -> Vec<T> {
        left.extend(right.into_iter());
        left
    }

    fn combine_one_with_many<T: Clone>(first: T, rest: Vec<T>) -> Vec<T> {
        combine(vec![first], rest)
    }
}

pub struct Parser {
    pub ast: Option<Vec<PNode>>
}

impl Parser {
    pub fn new() -> Parser {
        Parser { ast: None }
    }

    pub fn parse_file(&mut self, file_path: String) -> Result<Vec<PNode>, String> {
        match File::open(file_path) {
            Ok(file) => {
                let mut reader = BufReader::new(file);
                let mut code = String::new();
                reader.read_to_string(&mut code);
                self.parse(code)
            }
            Err(error) => Err(format!("Error: {}", error))
        }
    }

    pub fn parse(&mut self, code: String) -> Result<Vec<PNode>, String> {
        let state = bailey::parse_program(code.stream());

        match state.into_result() {
          Ok((success, error)) => {
            if success.partial_read() {
              Err(format!("Partial match: {:?} because: {}", success.data, error))
            }
            else {
                self.ast = Some(success.data.clone());
                Ok(success.data)
            }
          }
          Err(error) => {
              Err(format!("Error: {}", error))
          }
        }
    }
}
