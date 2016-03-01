use oak_runtime::stream::*;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

use ast::PNode;

grammar! bailey {
    use ast::PNode;
    use ast::Node::*;
    use ast::BinOp;
    use ast::BinOp::*;
    use std::str::FromStr;

    program
        = ((decl / stmt) terminator?)* > program

    decl
        = class_decl
        / method_decl

    class_decl
        = class_kw constant class_body > class_decl

    class_body
        = lbracket method_decl* rbracket

    method_decl
        = def_kw ident lparen param_list rparen block > method_decl

    block
        = lbracket ((stmt / expr) terminator?)* rbracket > block

    stmt
        = assign_stmt
        / if_stmt
        / while_stmt
        / expr

    if_stmt
        = if_kw expr block (else_kw block)? > if_stmt

    while_stmt
        = while_kw expr block > while_stmt

    function_call
        = ident lparen arg_list rparen

    method_call
        = callable_expr (dot function_call)+

    assign_stmt
        = ident bind_op expr > assign_stmt

    expr
        = cond_expr

    cond_expr
        = add_expr (cond_expr_op add_expr)* > fold_left

    add_expr
        = mult_expr (add_expr_op mult_expr)* > fold_left

    mult_expr
        = primary_expr (mult_expr_op primary_expr)* > fold_left

    primary_expr
        = method_call > method_call
        / callable_expr

    callable_expr
        = function_call > function_call
        / literal
        / constant
        / ident
        / lparen expr rparen

    arg_list
        = empty_list
        / expr (comma expr)* > arg_list

    param_list
        = empty_list
        / ident (comma ident)* > param_list

    empty_list = &rparen > empty_list

    digit = ["0-9"]
    ident_char = ["A-Za-z0-9_"]

    ident = !digit !keyword ident_char+ spacing > ident
    constant = !keyword !digit ["A-Z"]+ ident_char+ spacing > constant

    literal
        = string_lit
        / float_lit
        / int_lit
        / array_lit
        / map_lit

    int_lit = digit+ spacing > int_lit
    float_lit = digit+ dot digit+ spacing > float_lit
    string_lit = (dbl_quot_string_lit / sng_quot_string_lit) spacing > string_lit
    array_lit = lsqbracket expr (comma expr)* rsqbracket > array_lit

    map_entry =  (expr colon expr)
    map_lit = lbracket map_entry (comma map_entry)* rbracket > map_lit

    dbl_quot_string_lit = dbl_quot (spacing_char / !dbl_quot .)* dbl_quot
    sng_quot_string_lit = sng_quot (spacing_char / !sng_quot .)* sng_quot

    spacing_char = [" \n\r\t"]
    spacing = spacing_char* -> ()

    class_kw = "class" spacing
    def_kw = "def" spacing
    else_kw = "else" spacing
    false_kw = "false" spacing
    if_kw = "if" spacing
    nil_kw = "nil" spacing
    true_kw = "true" spacing
    while_kw = "while" spacing

    keyword
        = def_kw
        / if_kw
        / else_kw
        / class_kw
        / true_kw
        / false_kw
        / nil_kw

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
    colon = ":" spacing

    fn int_lit(raw_text: Vec<char>) -> PNode {
        PNode(IntLit(to_number(raw_text)))
    }

    fn float_lit(mut integer: Vec<char>, fractional: Vec<char>) -> PNode {
        integer.push('.');
        PNode(FltLit(to_float(combine(integer, fractional))))
    }

    fn string_lit(raw_text: Vec<char>) -> PNode {
        PNode(StrLit(to_string(raw_text)))
    }

    fn array_lit(first: PNode, rest: Vec<PNode>) -> PNode {
        PNode(ArrLit(combine_one_with_many(first, rest)))
    }

    fn map_lit(first: (PNode, PNode), rest: Vec<(PNode, PNode)>) -> PNode {
        PNode(MapLit(combine_one_with_many(first, rest)))
    }

    fn constant(first: Vec<char>, rest: Vec<char>) -> PNode {
        PNode(Const(to_string(combine(first, rest))))
    }

    fn ident(raw_text: Vec<char>) -> PNode {
        PNode(Ident(to_string(raw_text)))
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

    fn function_call(func: PNode, args: Vec<PNode>) -> PNode {
        PNode(Message { recv: None, meth: func, args: args })
    }

    fn method_call(recv: PNode, call: Vec<(PNode, Vec<PNode>)>) -> PNode {
        call.into_iter().fold(recv, { |accu, (method, args)|
            PNode(Message { recv: Some(accu), meth: method, args: args })
        })
    }

    fn assign_stmt(var: PNode, expr: PNode) -> PNode {
        PNode(AssignStmt { var: var, expr: expr })
    }

    fn class_decl(class: PNode, methods: Vec<PNode>) -> PNode {
        PNode(ClassDecl { class: class, meths: methods })
    }

    fn method_decl(meth: PNode, params: Vec<PNode>, block: PNode) -> PNode {
        PNode(MethodDecl { meth: meth, params: params, blk: block })
    }

    fn block(stmts: Vec<(PNode, Option<()>)>) -> PNode {
        let stmts = stmts.into_iter().map(|stmt| {
            match stmt { (s, _) => s }
        }).collect();

        PNode(Block(stmts))
    }

    fn program(stmts: Vec<(PNode, Option<()>)>) -> Vec<PNode> {
        stmts.into_iter().map(|stmt| {
            match stmt { (s, _) => s }
        }).collect()
    }

    fn if_stmt(cond: PNode, true_blk: PNode, false_blk: Option<PNode>) -> PNode {
        PNode(IfStmt { cond: cond, true_blk: true_blk, false_blk: false_blk })
    }

    fn while_stmt(cond: PNode, blk: PNode) -> PNode {
        PNode(WhileStmt { cond: cond, blk: blk })
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

    fn to_number(raw_text: Vec<char>) -> u64 {
        u64::from_str(&*to_string(raw_text)).unwrap()
    }

    fn to_float(raw_text: Vec<char>) -> f64 {
        f64::from_str(&*to_string(raw_text)).unwrap()
    }

    fn fold_left(head: PNode, rest: Vec<(BinOp, PNode)>) -> PNode {
        rest.into_iter().fold(head,
          |accu, (op, expr)| PNode(BinExpr { op: op, left: accu, right: expr }))
    }

    fn fold_right(front: Vec<(PNode, BinOp)>, last: PNode) -> PNode {
        front.into_iter().rev().fold(last,
          |accu, (expr, op)| PNode(BinExpr { op: op, left: expr, right: accu }))
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
    pub ast: Option<Vec<PNode>>,
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
                let _ = reader.read_to_string(&mut code);
                self.parse(code)
            }
            Err(error) => Err(format!("Error: {}", error)),
        }
    }

    pub fn parse(&mut self, code: String) -> Result<Vec<PNode>, String> {
        let state = bailey::parse_program(code.stream());

        match state.into_result() {
            Ok((success, error)) => {
                if success.partial_read() {
                    Err(format!("Partial match: {:?} because: {}", success.data, error))
                } else {
                    self.ast = Some(success.data.clone());
                    Ok(success.data)
                }
            }
            Err(error) => Err(format!("Error: {}", error)),
        }
    }
}
