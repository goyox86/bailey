use getopts::Options;

// use parser::Parser;
//
// fn print_usage(program: &str, opts: &Options) {
//     let brief = format!("Usage: {} FILE [options]", program);
//     print!("{}", opts.usage(&brief));
// }
//
// pub fn run(args: Vec<String>) {
//     let mut parser = Parser::new();
//     let program = &args[0].clone();
//
//     let mut opts = &mut Options::new();
//
//     opts.optopt("e", "", "evaluate expression", "EXP");
//     opts.optflag("h", "help", "print this help menu");
//     let matches = match opts.parse(&args[1..]) {
//         Ok(m) => m,
//         Err(f) => panic!(f.to_string()),
//     };
//
//     if matches.opt_present("h") {
//         print_usage(program, opts);
//         return;
//     }
//
//     if matches.opt_present("e") {
//         match matches.opt_str("e") {
//             Some(e) => {
//                 println!("{:#?}", parser.parse(e));
//                 return;
//             }
//             None => print_usage(program, opts),
//         }
//     }
//
//     if args.len() < 2 {
//         print_usage(program, opts);
//         return;
//     }
//
//     let file_path = args[1].clone();
//     println!("{:#?}", parser.parse_file(file_path));
// }
