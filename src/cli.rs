use oak_runtime::stream::*;

use getopts::Options;
use std::path::Path;

use std::io::prelude::*;
use std::fs::File;
use std::io::BufReader;

use parser::bailey;

pub fn print_usage(program: String, opts: Options) {
    let brief = format!("Usage: {} FILE [options]", program);
    print!("{}", opts.usage(&brief));
}

pub fn run(args: Vec<String>) {
    let program = args[0].clone();

    let mut opts = Options::new();

    opts.optopt("e", "", "evaluate expression", "EXP");
    opts.optflag("h", "help", "print this help menu");
    let matches = match opts.parse(&args[1..]) {
        Ok(m) => { m }
        Err(f) => { panic!(f.to_string()) }
    };

    if args.len() < 2 {
        print_usage(program, opts);
        return;
    }

    if matches.opt_present("h") {
        print_usage(program, opts);
        return;
    }

    let expression = matches.opt_str("e");
    match expression {
        Some(e) => {
            println!("{:#?}", bailey::parse_program(e.stream()).unwrap_data());
            return
        },
        None => print_usage(program, opts)
    };

    let file_path = args[1].clone();
    if Path::new(&file_path).exists() {
        let f = File::open(file_path).ok().expect("failed to open file");
        let mut reader = BufReader::new(f);
        let mut code = String::new();
        reader.read_to_string(&mut code);
        println!("{:#?}", bailey::parse_program(code.stream()).unwrap_data());
        return
    };
}
