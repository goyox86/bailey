use oak_runtime::stream::*;

use getopts::Options;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

use parser::bailey;
use ast::PNode;

fn print_usage(program: &String, opts: &Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

pub fn run(args: Vec<String>) {
    let program = &args[0].clone();

    let mut opts = &mut Options::new();

    opts.optopt("e", "", "evaluate expression", "EXP");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if matches.opt_present("h") {
        print_usage(program, opts);
        return;
    }

    if matches.opt_present("e") {
        match matches.opt_str("e") {
            Some(e) => {
                println!("{:#?}", parse(e));
                return
            },
            None => print_usage(program, opts)
        }
    }

    if args.len() < 2 {
        print_usage(program, opts);
        return;
    }

    let file_path = args[1].clone();
    println!("{:#?}", parse_file(file_path));
}

pub fn parse_file(file_path: String) -> Result<Vec<PNode>, String> {
    match File::open(file_path) {
        Ok(file) => {
            let mut reader = BufReader::new(file);
            let mut code = String::new();
            reader.read_to_string(&mut code);
            parse(code)
        }
        Err(error) => Err(format!("Error: {}", error))
    }
}

pub fn parse(code: String) -> Result<Vec<PNode>, String> {
    let state = bailey::parse_program(code.stream());

    match state.into_result() {
      Ok((success, error)) => {
        if success.partial_read() {
          Err(format!("Partial match: {:#?} because: {}", success.data, error))
        }
        else {
          Ok(success.data)
        }
      }
      Err(error) => {
          Err(format!("Error: {}", error))
      }
    }
}
