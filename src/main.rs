#![feature(plugin)]
#![plugin(oak)]
#![allow(non_snake_case)]
#![allow(dead_code)]

extern crate oak_runtime;
extern crate getopts;

use std::env;

mod ast;
mod parser;
mod cli;

fn main() {
    let args: Vec<String> = env::args().collect();
    cli::run(args);
}
