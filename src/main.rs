#![feature(plugin)]
#![plugin(oak)]

extern crate oak_runtime;
use oak_runtime::*;

grammar! awesome {
  #![show_api]

  program = declaration_list /
            statement_list /
            expression_list

  declaration_list
    = declaration (terminator declaration)*

  expression_list
    = expression (terminator expression)*

  statement_list
    = statement (terminator statement)*

  declaration
    = class_declaration > to_class_declaration
    / method_declaration > to_method_declaration

  class_declaration
    = kw_class spacing identifier class_body

  class_body
    = lbracket spacing method_declaration* rbracket

  method_declaration
    = kw_def spacing identifier lparen params_list? rparen block

  block
    = lbracket spacing (statement / expression)+ spacing rbracket

  statement
    = if_statement > to_if_statement
    / while_statement > to_while_statement

  if_statement
    = kw_if expression block

  while_statement
    = kw_while expression block

  assignment_expression
    = identifier bind_op expression

  expression
    = spacing term (term_op term)* > fold_left

  call
    = identifier lparen arg_list? rparen

  arg_list
    = (expression ("," spacing expression)*)

  params_list
    = (identifier ("," spacing identifier)*)

  term
    = exponent (factor_op exponent)* > fold_left

  exponent
    = (factor exponent_op)* factor > fold_right

  factor
    = call > call_expr
    / number > number_expr
    / assignment_expression > assign_expr
    / identifier > variable_expr
    / lparen expression rparen

  term_op
    = add_op > add_bin_op
    / sub_op > sub_bin_op

  factor_op
    = mul_op > mul_bin_op
    / div_op > div_bin_op

  exponent_op = exp_op > exp_bin_op

  identifier = !digit !keyword ident_char+ spacing > to_string
  ident_char = ["a-zA-Z0-9_"]

  digit = ["0-9"]
  number = digit+ spacing > to_number
  spacing = [" \r\t"]* -> ()

  kw_def = "def"
  kw_class = "class"
  kw_true = "true"
  kw_false = "false"
  kw_nil = "nil"
  kw_if = "if"
  kw_while = "while"
  kw_tail = !ident_char spacing

  keyword
    = kw_def
    / kw_class
    / kw_true
    / kw_false
    / kw_nil
    / kw_tail

  bind_op = "=" spacing
  add_op = "+" spacing
  sub_op = "-" spacing
  mul_op = "*" spacing
  div_op = "/" spacing
  exp_op = "^" spacing
  lparen = "(" spacing
  rparen = ")" spacing
  lbracket = "{"
  rbracket = "}"

  terminator = ";" / "\n"

  use std::str::FromStr;
  use self::Expression::*;
  use self::BinOp::*;

  pub type PExpr = Box<Expression>;

  #[derive(Debug)]
  pub enum Expression {
    Variable(String),
    Number(u32),
    BinaryExpr(BinOp, PExpr, PExpr),
    Call(String, Option<(PExpr, Vec<PExpr>)>),
    AssingExpr(String, PExpr),
    ClassDecl(String),
    MethodDecl(String, Option<(String, Vec<String>)>, Vec<PExpr>),
    IfStatement(PExpr, Vec<PExpr>),
    WhileStatement(PExpr, Vec<PExpr>)
  }

  #[derive(Debug)]
  pub enum BinOp {
    Add, Sub, Mul, Div, Exp
  }

  fn to_number(raw_text: Vec<char>) -> u32 {
    u32::from_str(&*to_string(raw_text)).unwrap()
  }

  fn number_expr(value: u32) -> PExpr {
    Box::new(Number(value))
  }

  fn variable_expr(ident: String) -> PExpr {
    Box::new(Variable(ident))
  }

  fn to_string(raw_text: Vec<char>) -> String {
    raw_text.into_iter().collect()
  }

  fn fold_left(head: PExpr, rest: Vec<(BinOp, PExpr)>) -> PExpr {
    rest.into_iter().fold(head,
      |accu, (op, expr)| Box::new(BinaryExpr(op, accu, expr)))
  }

  fn fold_right(front: Vec<(PExpr, BinOp)>, last: PExpr) -> PExpr {
    front.into_iter().rev().fold(last,
      |accu, (expr, op)| Box::new(BinaryExpr(op, expr, accu)))
  }

  fn add_bin_op() -> BinOp { Add }
  fn sub_bin_op() -> BinOp { Sub }
  fn mul_bin_op() -> BinOp { Mul }
  fn div_bin_op() -> BinOp { Div }
  fn exp_bin_op() -> BinOp { Exp }

  fn call_expr(method: String, args: Option<(PExpr, Vec<PExpr>)>) -> PExpr {
      Box::new(Call(method, args))
  }

  fn assign_expr(variable: String, exp: PExpr) -> PExpr {
      Box::new(AssingExpr(variable, exp))
  }

  fn to_class_declaration(foo: String, bar: Vec<(String, Option<(String, Vec<String>)>, Vec<PExpr>)>) -> PExpr {
      Box::new(ClassDecl(foo))
  }

  fn to_method_declaration(name: String, params: Option<(String, Vec<String>)>, body: Vec<PExpr>) -> PExpr {
      Box::new(MethodDecl(name, params, body))
  }

  fn to_if_statement(condition: PExpr, block: Vec<PExpr>) -> PExpr {
      Box::new(IfStatement(condition, block))
  }

  fn to_while_statement(condition: PExpr,  block: Vec<PExpr>) -> PExpr {
      Box::new(WhileStatement(condition, block))
  }
}

fn analyse_state(state: ParseState<StrStream, awesome::PExpr>) {
  match state.into_result() {
    Ok((success, error)) => {
      if success.partial_read() {
        println!("Partial match: {:?} because: {}", success.data, error);
      }
      else {
        println!("Full match: {:?}", success.data);
      }
    }
    Err(error) => {
      println!("Error: {}", error);
    }
  }
}

fn main() {
  let program1 = "def foo(a, b) { if 1 { 1 + 1 } }";
  println!("{:?}", awesome::parse_program(program1.stream()));
  //analyse_state(awesome::parse_program(program1.stream()));
}
