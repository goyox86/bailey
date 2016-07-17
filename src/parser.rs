use oak_runtime::parse_state::*;
use oak_runtime::parse_state::ParseResult::*;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

use ast::Stmt;

grammar! bailey {
    use ast::Ident;
    use ast::Literal;
    use ast::Expr;
    use ast::Stmt;
    use ast::Decl;
    use ast::BinOp;
    use ast::Block;
    use ast::BinOp::*;
    use std::str::FromStr;

    program
        = newlines (stmt terminator)*

    stmt
        = const_assign_stmt
        / assign_stmt
        / if_stmt
        / while_stmt
        / decl > decl_to_stmt
        / expr > expr_to_stmt

    assign_stmt
        = var_ident bind_op expr > assign_stmt

    const_assign_stmt
        = const_ident bind_op expr > const_assign_stmt

    if_stmt
        = if_kw expr block (else_kw block)? > if_stmt

    while_stmt
        = while_kw expr block > while_stmt

    decl
        = class_decl
        / method_decl

    class_decl
        = class_kw const_ident class_body > class_decl

    class_body
        = lbracket (newlines method_decl)* rbracket

    method_decl
        = def_kw meth_ident lparen param_list rparen block > method_decl

    block
        = newlines lbracket stmt_list rbracket spacing > block

    stmt_list
        = empty_stmt_list
        / stmt (terminator stmt)* > stmt_list

    function_call
        = newlines meth_ident lparen arg_list rparen

    method_call
        = callable_expr (dot function_call)+

    expr
        = newlines cond_expr 

    var_expr
        = var_ident > var_expr

    const_expr
        = const_ident > const_expr

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
        / literal > lit_to_exp
        / const_expr
        / var_expr
        / array
        / map
        / lparen expr rparen

    arg_list
        = empty_list
        / expr (comma expr)* > arg_list

    param_list
        = empty_param_list
        / var_ident (comma var_ident)* > param_list

    empty_list = &rparen > empty_list
    empty_param_list = &rparen > empty_param_list
    empty_stmt_list = &rbracket > empty_stmt_list

    digit = ["0-9"]
    upcase_ltr = ["A-Z"]
    lowcase_ltr = ["a-z_"]
    ident_char = (upcase_ltr / lowcase_ltr / digit)
    
    var_ident = !digit !keyword &lowcase_ltr ident_char+ spacing > var_ident
    meth_ident = !digit !keyword ident_char+ spacing > meth_ident
    const_ident = !keyword !digit &upcase_ltr ident_char+ spacing > const_ident

    literal
        = string_lit
        / float_lit
        / int_lit

    int_lit = digit+ spacing > int_lit
    float_lit = digit+ dot digit+ spacing > float_lit
    string_lit = (dbl_quot_string_lit / sng_quot_string_lit) spacing > string_lit
    
    array_elem = expr > array_elem
    array = lsqbracket array_elem (comma array_elem)* rsqbracket > array

    map_entry =  (expr colon expr) > map_entry
    map = lbracket map_entry (comma map_entry)* rbracket > map

    dbl_quot_string_lit = dbl_quot (spacing_char / !dbl_quot .)* dbl_quot
    sng_quot_string_lit = sng_quot (spacing_char / !sng_quot .)* sng_quot

    spacing_char = [" \r\t"]
    spacing = spacing_char* -> ()
    newline = "\n" spacing -> ()
    semicolon = ";" spacing -> ()
    newlines = newline* spacing -> ()
    terminator = (newline / semicolon) newlines -> ()

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

    lparen = "(" newlines -> ()
    rparen = newlines ")" -> ()
    lbracket = "{" newlines -> ()
    rbracket = newlines "}" -> ()
    lsqbracket = "[" newlines
    rsqbracket = newlines "]"
    dbl_quot = "\"" spacing
    sng_quot = "'" spacing
    dot = "." spacing
    comma = "," spacing
    colon = ":" spacing

    fn decl_to_stmt(decl: Decl) -> Stmt {
        Stmt::Decl(Box::new(decl))
    }
    
    fn expr_to_stmt(expr: Expr) -> Stmt {
        Stmt::Expr(Box::new(expr))
    }

    fn lit_to_exp(literal: Literal) -> Expr {
        Expr::Literal(Box::new(literal))
    }
    
    fn int_lit(raw_text: Vec<char>) -> Literal {
        Literal::Integer(to_number(raw_text))
    }

    fn float_lit(mut integer: Vec<char>, fractional: Vec<char>) -> Literal {
        integer.push('.');
        Literal::Float(to_float(combine(integer, fractional)))
    }

    fn string_lit(raw_text: Vec<char>) -> Literal {
        Literal::Str(to_string(raw_text))
    }

    fn array(first: Box<Expr>, rest: Vec<Box<Expr>>) -> Expr {
        Expr::Array(combine_one_with_many(first, rest))
    }

    fn array_elem(expr: Expr) -> Box<Expr> {
        Box::new(expr)
    }

    fn map(first: (Box<Expr>, Box<Expr>), rest: Vec<(Box<Expr>, Box<Expr>)>) -> Expr {
        Expr::Map(combine_one_with_many(first, rest))
    }

    fn map_entry(key: Expr, val: Expr) -> (Box<Expr>, Box<Expr>) {
        (Box::new(key), Box::new(val))
    }

    fn const_expr(ident: Ident) -> Expr {
        Expr::Const(Box::new(ident))
    }

    fn const_ident(raw_text: Vec<char>) -> Ident {
        Ident::Const(to_string(raw_text))
    }

    fn var_expr(ident: Ident) -> Expr {
        Expr::Var(Box::new(ident))
    }

    fn var_ident(raw_text: Vec<char>) -> Ident {
        Ident::Var(to_string(raw_text))
    }

    fn meth_ident(raw_text: Vec<char>) -> Ident {
        Ident::Meth(to_string(raw_text))
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

    fn function_call(func: Ident, args: Vec<Expr>) -> Expr {
        Expr::Message(None, func, args)
    }

    fn method_call(recv: Expr, call: Vec<(Ident, Vec<Expr>)>) -> Expr {
        call.into_iter().fold(recv, { |accu, (method, args)|
            Expr::Message(Some(Box::new(accu)), method, args)
        })
    }

    fn assign_stmt(var: Ident, expr: Expr) -> Stmt {
        Stmt::VarAssign(Box::new(var), Box::new(expr))
    }

    fn const_assign_stmt(constant: Ident, expr: Expr) -> Stmt {
        Stmt::ConstAssign(Box::new(constant), Box::new(expr))
    }

    fn class_decl(class: Ident, methods: Vec<Decl>) -> Decl {
        Decl::Class(Box::new(class), methods)
    }

    fn method_decl(meth: Ident, params: Vec<Ident>, block: Block) -> Decl {
        Decl::Method(meth, params, Box::new(block))
    }

    fn block(stmts: Vec<Stmt>) -> Block {
        Block { stmts: stmts }
    }

    fn program(first: Stmt, mut rest: Vec<Stmt>) -> Vec<Stmt> {
        combine_one_with_many(first, rest)
    }

    fn if_stmt(cond: Expr, true_blk: Block, false_blk: Option<Block>) -> Stmt {
        let false_blk = match false_blk {
            Some(blk) => Some(Box::new(blk)),
            None => None
        };

        Stmt::If(Box::new(cond), Box::new(true_blk), false_blk)
    }

    fn while_stmt(cond: Expr, blk: Block) -> Stmt {
        Stmt::While(Box::new(cond), Box::new(blk))
    }

    fn arg_list(first: Expr, mut rest: Vec<Expr>) -> Vec<Expr> {
        combine_one_with_many(first, rest)
    }

    fn param_list(first: Ident, mut rest: Vec<Ident>) -> Vec<Ident> {
        combine_one_with_many(first, rest)
    }

    fn empty_param_list() -> Vec<Ident> {
        vec![]
    }

    fn stmt_list(first: Stmt, mut rest: Vec<Stmt>) -> Vec<Stmt> {
        combine_one_with_many(first, rest)
    }

    fn empty_list() -> Vec<Expr> {
        vec![]
    }

    fn empty_stmt_list() -> Vec<Stmt> {
        vec![]
    }

    fn to_string(raw_text: Vec<char>) -> String {
        raw_text.into_iter().collect()
    }

    fn to_number(raw_text: Vec<char>) -> i64 {
        i64::from_str(&*to_string(raw_text)).unwrap()
    }

    fn to_float(raw_text: Vec<char>) -> f64 {
        f64::from_str(&*to_string(raw_text)).unwrap()
    }

    fn fold_left(head: Expr, rest: Vec<(BinOp, Expr)>) -> Expr {
        rest.into_iter().fold(head,
          |accu, (op, expr)| Expr::Binary(op, Box::new(accu), Box::new(expr)))
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
    pub ast: Option<Vec<Stmt>>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser { ast: None }
    }

    pub fn parse_file(&mut self, file_path: String) -> Result<Vec<Stmt>, String> {
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

    pub fn parse(&mut self, code: String) -> Result<Vec<Stmt>, String> {
        let state = bailey::parse_program(code.into_state());

        match state.into_result() {
            Success(data) => Ok(data.clone()),
            Partial(data, expectation) => {
                Err(format!("Partial match: {:?} because: {:?}", data, expectation))
            }
            Failure(expectation) => {
                Err(format!("Error: {:?}", expectation))
            }
        }
    }
}

