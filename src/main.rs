#![feature(plugin)]
#![plugin(oak)]

extern crate oak_runtime;
use oak_runtime::*;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

grammar! awesome {
    program
        = ((expr / decl / stmt) terminator?)*

    decl
        = class_decl > class_decl
        / method_decl > method_decl

    class_decl
        = kw_class constant class_body

    class_body
        = lbracket method_decl* rbracket

    method_decl
        = kw_def identifier lparen param_list? rparen block

    block
        = lbracket ((stmt / expr) terminator?)* rbracket

    stmt
        = if_stmt > if_stmt
        / while_stmt > while_stmt

    if_stmt
        = kw_if expr block (kw_else block)?

    while_stmt
        = kw_while expr block

    call
        = (receiver_expr ".")? identifier lparen arg_list? rparen

    assign_expr
        = identifier bind_op expr > assign_expr

    expr
        = spacing cond_expr

    cond_expr
        = add_expr (cond_expr_op add_expr)* > fold_left

    add_expr
        = mult_expr (add_expr_op mult_expr)* > fold_left

    mult_expr
        = primary_expr (mult_expr_op primary_expr)* > fold_left

    primary_expr
        = call > call_expr
          / receiver_expr

    receiver_expr
        = assign_expr
        / literal
        / constant
        / identifier
        / lparen expr rparen

    arg_list
        = (expr ("," spacing expr)*)

    param_list
        = (identifier ("," spacing identifier)*)

    digit = ["0-9"]
    identifier_char = ["A-Za-z0-9_"]

    identifier = !digit !keyword identifier_char+ spacing > identifier
    constant = !keyword !digit ["A-Z"]+ identifier_char+ spacing > constant

    literal
        = lit_string
        / lit_float
        / lit_int

    lit_int = digit+ spacing > lit_int
    lit_float = digit+ "." digit+ spacing > lit_float
    lit_string = (dbl_quot_lit_string / sng_quot_lit_string) spacing > lit_string

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
    dbl_quot = "\"" spacing
    sng_quot = "'" spacing
    terminator = ";" spacing

    fn lit_int(raw_text: Vec<char>) -> PExpr {
        Box::new(IntegerLiteral(to_number(raw_text)))
    }

    // FIXME: Please remove this hacky wacky '.' thingy
    fn lit_float(integer: Vec<char>, fractional: Vec<char>) -> PExpr {
        let mut buf = integer;
        buf.push('.');
        Box::new(FloatLiteral(to_float(combine(buf, fractional))))
    }

    fn lit_string(raw_text: Vec<char>) -> PExpr {
        Box::new(StringLiteral(to_string(raw_text)))
    }

    fn constant(first: Vec<char>, rest: Vec<char>) -> PExpr {
        Box::new(Constant(to_string(combine(first, rest))))
    }

    fn identifier(raw_text: Vec<char>) -> PExpr {
        Box::new(Identifier(to_string(raw_text)))
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

    fn call_expr(receiver: Option<PExpr>, method: PExpr, args: Option<(PExpr, Vec<PExpr>)>) -> PExpr {
        Box::new(Call(receiver, method, args))
    }

    fn assign_expr(identifier: PExpr, expr: PExpr) -> PExpr {
        Box::new(AssingExpr(identifier, expr))
    }

    fn class_decl(class: PExpr, methods: Vec<(PExpr, Option<(PExpr, Vec<PExpr>)>, Vec<(PExpr, Option<()>)>)>) -> PExpr {
        Box::new(ClassDecl(class, methods))
    }

    fn method_decl(identifier: PExpr, params: Option<(PExpr, Vec<PExpr>)>, body: Vec<(PExpr, Option<()>)>) -> PExpr {
        Box::new(MethodDecl(identifier, params, body))
    }

    fn if_stmt(condition: PExpr, block: Vec<(PExpr, Option<()>)>, else_block: Option<Vec<(PExpr, Option<()>)>>) -> PExpr {
        Box::new(IfStatement(condition, block, else_block))
    }

    fn while_stmt(condition: PExpr,  block: Vec<(PExpr, Option<()>)>) -> PExpr {
      Box::new(WhileStatement(condition, block))
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

    fn fold_left(head: PExpr, rest: Vec<(BinOp, PExpr)>) -> PExpr {
        rest.into_iter().fold(head,
          |accu, (op, expr)| Box::new(BinaryExpr(op, accu, expr)))
    }

    fn fold_right(front: Vec<(PExpr, BinOp)>, last: PExpr) -> PExpr {
        front.into_iter().rev().fold(last,
          |accu, (expr, op)| Box::new(BinaryExpr(op, expr, accu)))
    }

    fn combine<T: Clone>(left: Vec<T>, right: Vec<T>) -> Vec<T> {
        let mut result = left.clone();
        result.extend(right.into_iter());
        result
    }

    use std::str::FromStr;
    use self::Expression::*;
    use self::BinOp::*;

    pub type PExpr = Box<Expression>;

    #[derive(Debug)]
    pub enum Expression {
        Identifier(String),
        Constant(String),
        IntegerLiteral(u32),
        FloatLiteral(f32),
        StringLiteral(String),
        BinaryExpr(BinOp, PExpr, PExpr),
        Call(Option<PExpr>, PExpr, Option<(PExpr, Vec<PExpr>)>),
        AssingExpr(PExpr, PExpr),
        MethodDecl(PExpr, Option<(PExpr, Vec<PExpr>)>, Vec<(PExpr, Option<()>)>),
        ClassDecl(PExpr, Vec<(PExpr, Option<(PExpr, Vec<PExpr>)>, Vec<(PExpr, Option<()>)>)>),
        IfStatement(PExpr, Vec<(PExpr, Option<()>)>, Option<Vec<(PExpr, Option<()>)>>),
        WhileStatement(PExpr, Vec<(PExpr, Option<()>)>),
        Block(Vec<PExpr>)
    }

    #[derive(Debug)]
    pub enum BinOp {
        Add, Sub, Mul, Div, Lt, Lte, Gt, Gte, Ne, Eq, And, Or
    }
}

fn main() {
    let f = File::open("./src/example.awm").ok().expect("failed to open file");
    let mut reader = BufReader::new(f);
    let mut contents = String::new();
    reader.read_to_string(&mut contents);
    println!("{:#?}", awesome::parse_program(contents.stream()));
}
