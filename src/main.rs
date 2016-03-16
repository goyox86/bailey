extern crate getopts;

use std::env;

mod ast;
mod cli;

pub mod parser;

fn main() {
    print!("{:#?}", parser::parse_expr("1"));
}

#[test]
fn test_parser() {
    assert!(parser::parse_Expr("22").is_ok());
    assert!(parser::parse_Expr("(22)").is_ok());
    assert!(parser::parse_Expr("((((22))))").is_ok());
    assert!(parser::parse_Expr("((22)").is_err());
}
