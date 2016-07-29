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
    use ast::BinOp::*;
    use ast::BinOp;
    use ast::Block;
    use ast::P;
    use std::str::FromStr;

    program
        = newlines (stmt terminator)*

    stmt
        = const_assign_stmt
        / assign_stmt
        / if_stmt
        / while_stmt
        / block > block_stmt
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
        Stmt::Decl(P(decl))
    }
    
    fn expr_to_stmt(expr: Expr) -> Stmt {
        Stmt::Expr(P(expr))
    }

    fn lit_to_exp(literal: Literal) -> Expr {
        Expr::Literal(P(literal))
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

    fn array(first: P<Expr>, rest: Vec<P<Expr>>) -> Expr {
        Expr::Array(combine_one_with_many(first, rest))
    }

    fn array_elem(expr: Expr) -> P<Expr> {
        P(expr)
    }

    fn map(first: (P<Expr>, P<Expr>), rest: Vec<(P<Expr>, P<Expr>)>) -> Expr {
        Expr::Map(combine_one_with_many(first, rest))
    }

    fn map_entry(key: Expr, val: Expr) -> (P<Expr>, P<Expr>) {
        (P(key), P(val))
    }

    fn const_expr(ident: Ident) -> Expr {
        Expr::Const(P(ident))
    }

    fn const_ident(raw_text: Vec<char>) -> Ident {
        Ident::Const(to_string(raw_text))
    }

    fn var_expr(ident: Ident) -> Expr {
        Expr::Var(P(ident))
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
        Expr::Message(None, P(func), args)
    }

    fn method_call(recv: Expr, call: Vec<(Ident, Vec<Expr>)>) -> Expr {
        call.into_iter().fold(recv, { |accu, (method, args)|
            Expr::Message(Some(P(accu)), P(method), args)
        })
    }

    fn assign_stmt(var: Ident, expr: Expr) -> Stmt {
        Stmt::VarAssign(P(var), P(expr))
    }

    fn const_assign_stmt(constant: Ident, expr: Expr) -> Stmt {
        Stmt::ConstAssign(P(constant), P(expr))
    }

    fn class_decl(class: Ident, methods: Vec<Decl>) -> Decl {
        Decl::Class(P(class), methods)
    }

    fn method_decl(meth: Ident, params: Vec<Ident>, block: Block) -> Decl {
        Decl::Method(P(meth), params, P(block))
    }

    fn block(stmts: Vec<Stmt>) -> Block {
        Block { stmts: stmts }
    }

    fn if_stmt(cond: Expr, true_blk: Block, false_blk: Option<Block>) -> Stmt {
        let false_blk = match false_blk {
            Some(blk) => Some(P(blk)),
            None => None
        };

        Stmt::If(P(cond), P(true_blk), false_blk)
    }

    fn while_stmt(cond: Expr, blk: Block) -> Stmt {
        Stmt::While(P(cond), P(blk))
    }

    fn block_stmt(blk: Block) -> Stmt {
        Stmt::Block(P(blk))
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
          |accu, (op, expr)| Expr::Binary(op, P(accu), P(expr)))
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

#[allow(unused_imports)]
mod tests {
    use super::Parser;
    use ast::Stmt;
    use ast::Expr;
    use ast::Decl;
    use ast::Literal;
    use ast::Ident;
    use ast::Block;
    use ast::BinOp;
    use ast::P;
    

    macro_rules! expr_stmt {
        ($e:expr) => (
            Stmt::Expr($e)
        );
    }
    
    macro_rules! var_assign_stmt {
        ($ident:expr, $e:expr) => (
            Stmt::VarAssign($ident, $e)
        );
    }

    macro_rules! decl_stmt {
        ($e:expr) => (
            Stmt::Decl($e)
        );
    }

    macro_rules! if_stmt {
        ($cond:expr, $true_blk:expr, $false_blk:expr) => (
            Stmt::If($cond, $true_blk, $false_blk)
        );
    }
    
    macro_rules! while_stmt {
        ($cond:expr, $blk:expr) => (
            Stmt::While($cond, $blk)
        );
    }

    macro_rules! block_stmt {
       ($e:expr) => {
           Stmt::Block($e)
       };
    }

    macro_rules! block {
       ( $( $x:expr ),* ) => {
           P(Block { stmts: vec![$($x,)*] })
       };
    }

    macro_rules! meth_decl {
        ($ident:expr, $args:expr, $blk:expr) => (
            P(Decl::Method($ident, $args, $blk))
        );
    }

    macro_rules! meth_args {
        ( $( $e:expr ),* ) => {
            vec![$(Ident::Var($e.to_string()),)*]
        };
    }

    macro_rules! class_decl {
        ($ident:expr, $meths:expr) => (
            P(Decl::Class($ident, $meths))
        );
    }

    macro_rules! lit_expr {
        ($e:expr) => (
            P(Expr::Literal($e))
        );
    }
    
    macro_rules! int_lit {
        ($e:expr) => (
            P(Literal::Integer($e))
        );
    }

    macro_rules! flt_lit {
        ($e: expr) => (
            P(Literal::Float($e))
        );
    }

    macro_rules! str_lit {
        ($e:expr) => (
            P(Literal::Str($e.to_string()))
        );
    }

   macro_rules! arr_expr {
        ( $( $x:expr ),* ) => {
            P(Expr::Array(vec![$($x,)*]))
        };
   }

   macro_rules! map_expr {
        ( $( ($k:expr, $v:expr) ),* ) => {
            P(Expr::Map(vec![$(($k, $v),)*]))
        };
   }

   macro_rules! var_expr {
        ($e:expr) => {
            P(Expr::Var($e))
        };
   }

   macro_rules! bin_expr {
        ($op:expr, $l:expr, $r:expr) => {
            P(Expr::Binary($op, $l, $r))
        };
   }

   macro_rules! message_expr {
        ($recv:expr, $meth:expr, $args:expr) => {
            P(Expr::Message($recv, $meth, $args))
        };
   }

   macro_rules! const_expr {
        ($e:expr) => {
            P(Expr::Const($e))
        };
   }

   macro_rules! var_ident {
        ($e:expr) => {
            P(Ident::Var($e.to_string()))
        };
   }

   macro_rules! const_ident {
       ($e:expr) => {
            P(Ident::Const($e.to_string()))
        };
   }

   macro_rules! meth_ident {
       ($e:expr) => {
            P(Ident::Meth($e.to_string()))
        };
   }
 
   macro_rules! class_ident {
       ($e:expr) => {
            P(Ident::Const($e.to_string()))
        };
   }

   #[test]
    fn test_parse_int_lit() {
        let mut parser = Parser::new();
        let code = "42;".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
                expr_stmt!(
                    lit_expr!(
                        int_lit!(42)
                    )
                )
            ]
        );
    }

    #[test]
    fn test_parse_float_lit() {
        let mut parser = Parser::new();
        let code = "42.00;".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
                expr_stmt!(
                    lit_expr!(
                        flt_lit!(42.00)
                    )
                )
            ]
        );
    }

    #[test]
    fn test_parse_string_lit() {
        let mut parser = Parser::new();
        let code = "\"fooooo\";".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
                expr_stmt!(
                    lit_expr!(
                        str_lit!("fooooo")
                    )
                )
            ]
        );
    }

    #[test]
    fn test_parse_array() {
        let mut parser = Parser::new();
        let code = "[1, 2.0, \"foo\"];".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                arr_expr!(
                    lit_expr!(int_lit!(1)),
                    lit_expr!(flt_lit!(2.0)),
                    lit_expr!(str_lit!("foo"))
                )
            )
        ]);
    }

    #[test]
    fn test_parse_map() {
        let mut parser = Parser::new();
        let code = "{a: 1, b: 2};".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                map_expr!(
                    (var_expr!(var_ident!("a")), lit_expr!(int_lit!(1))),
                    (var_expr!(var_ident!("b")), lit_expr!(int_lit!(2)))
                )
            )
        ]);
    }

    #[test]
    fn test_parse_const() {
        let mut parser = Parser::new();
        let code = "Awesome;".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                const_expr!(const_ident!("Awesome"))
            )
        ]);
    }

    #[test]
    fn test_parse_var() {
        let mut parser = Parser::new();
        let code = "foo;".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                var_expr!(var_ident!("foo"))
            )
        ]);
    }

    #[test]
    fn test_parse_block() {
        let mut parser = Parser::new();
        let code = r#"
            {
                1
                2 / 3
            }
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            block_stmt!(
                block!(
                    expr_stmt!(lit_expr!(int_lit!(1))),
                    expr_stmt!(
                        bin_expr!(
                            BinOp::Div,
                            lit_expr!(int_lit!(2)),
                            lit_expr!(int_lit!(3))
                        )
                    )
                )
            )
        ]);
    }

    #[test]
    fn test_parse_method_no_args() {
        let mut parser = Parser::new();
        let code = "def method1() { };".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            decl_stmt!(
                meth_decl!(
                    meth_ident!("method1"),
                    vec![],
                    block!()
                )
            )
        ]);
    }
    
    #[test]
    fn test_parse_method() {
        let mut parser = Parser::new();
        let code = "def method2(arg1, arg2) { };".to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            decl_stmt!(
                meth_decl!(
                    meth_ident!("method2"),
                    meth_args!("arg1", "arg2"),
                    block!()
                )
            )
        ]);
    }
    
    #[test]
    fn test_parse_method_with_body() {
        let mut parser = Parser::new();
        let code = r#"
            def method3(arg1, arg2) {
                arg1 + arg2
            };
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            decl_stmt!(
                meth_decl!(
                    meth_ident!("method3"),
                    meth_args!("arg1", "arg2"),
                    block!(
                        expr_stmt!(
                            bin_expr!(BinOp::Add,
                                var_expr!(var_ident!("arg1")),
                                var_expr!(var_ident!("arg2"))
                            )
                        )
                    )
                )
            )
        ]);
    }
    
    #[test]
    fn test_parse_class() {
        let mut parser = Parser::new();
        let code = r#"
            class TestClass { }
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            decl_stmt!(
                class_decl!(
                    class_ident!("TestClass"),
                    vec![]
                )
            )
        ]);
    }
    
    #[test]
    fn test_parse_assign_stmt() {
        let mut parser = Parser::new();
        let code = r#"
            foo = 4 - 2
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            var_assign_stmt!(
                var_ident!("foo"),
                bin_expr!(BinOp::Sub,
                    lit_expr!(int_lit!(4)),
                    lit_expr!(int_lit!(2))
                )
            ) 
        ]);
    }
    
    #[test]
    fn test_parse_bin_expr() {
        let mut parser = Parser::new();
        let code = r#"
            4.0 * 2
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                bin_expr!(BinOp::Mul,
                    lit_expr!(flt_lit!(4.0)),
                    lit_expr!(int_lit!(2))
                )
            )
        ]);
    }

    #[test]
    fn test_parse_msg_recv_no_args() {
        let mut parser = Parser::new();
        let code = r#"
            1.foo()
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                message_expr!(Some(lit_expr!(int_lit!(1))),
                    meth_ident!("foo"),
                    vec![]
                )    
            ) 
        ]);
    }

    // [TODO]: Replace Expr::Literal with macros or modify ast to have a Vec<Box<Expr>> type - 2016-07-29 03:23P
    #[test]
    fn test_parse_msg_recv_args() {
        let mut parser = Parser::new();
        let code = r#"
            1.bar(2, "baz", 2.0)
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                message_expr!(Some(lit_expr!(int_lit!(1))),
                    meth_ident!("bar"),
                    vec![
                        Expr::Literal(int_lit!(2)),
                        Expr::Literal(str_lit!("baz")),
                        Expr::Literal(flt_lit!(2.0))
                    ]
                )    
            ) 
        ]);
    }

    #[test]
    fn test_parse_msg_no_recv_no_args() {
        let mut parser = Parser::new();
        let code = r#"
            bar()
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                message_expr!(None,
                    meth_ident!("bar"),
                    vec![]
                )    
            ) 
        ]);
    }

    #[test]
    fn test_parse_msg_no_recv_args() {
        let mut parser = Parser::new();
        let code = r#"
            quux(2, "baz", 2.0)
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            expr_stmt!(
                message_expr!(None,
                    meth_ident!("quux"),
                    vec![
                        Expr::Literal(int_lit!(2)),
                        Expr::Literal(str_lit!("baz")),
                        Expr::Literal(flt_lit!(2.0))
                    ]
                )    
            ) 
        ]);
    }
    
    #[test]
    fn test_parse_while_stmt() {
        let mut parser = Parser::new();
        let code = r#"
            while i < n {
                i = i + 1
            }
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            while_stmt!(
                bin_expr!(BinOp::Lt,
                    var_expr!(var_ident!("i")),
                    var_expr!(var_ident!("n"))
                ),
                block!(
                    var_assign_stmt!(
                        var_ident!("i"),
                        bin_expr!(BinOp::Add,
                            var_expr!(var_ident!("i")),
                            lit_expr!(int_lit!(1))
                        )
                    )
                )
            )
        ]);
    }
    
    #[test]
    fn test_parse_if_stmt() {
        let mut parser = Parser::new();
        let code = r#"
            if n == 0 {
                1
            }
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            if_stmt!(
                bin_expr!(BinOp::Eq,
                    var_expr!(var_ident!("n")),
                    lit_expr!(int_lit!(0))
                ),
                block!(
                    expr_stmt!(
                        lit_expr!(int_lit!(1))
                    )
                ),
                None
            )
        ]);
    }

    #[test]
    fn test_parse_if_else_stmt() {
        let mut parser = Parser::new();
        let code = r#"
            if n == 0 {
                1
            } else {
                0
            }
        "#.to_string();
        let ast = parser.parse(code).unwrap();
        assert_eq!(ast, vec![
            if_stmt!(
                bin_expr!(BinOp::Eq,
                    var_expr!(var_ident!("n")),
                    lit_expr!(int_lit!(0))
                ),
                block!(
                    expr_stmt!(
                        lit_expr!(int_lit!(1))
                    )
                ),
                Some(block!(
                    expr_stmt!(
                        lit_expr!(int_lit!(0))
                    )
                ))
            )
        ]);
    }
}
