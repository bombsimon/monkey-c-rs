use monkey_c_formatter::Formatter;

use std::env;
use std::fs;
use std::io::{self, Read};

fn main() -> io::Result<()> {
    let args: Vec<String> = env::args().collect();
    let mut source = String::new();

    if args.len() > 1 {
        // Read from file
        fs::File::open(&args[1])?.read_to_string(&mut source)?;
    } else {
        // Read from stdin
        eprintln!("Reading from stdin");
        io::stdin().read_to_string(&mut source)?;
    }

    let parser = monkey_c_parser::parser::Parser::new(&source);
    match parser.parse() {
        Ok(output) => {
            let formatter = Formatter::new(&source)
                .with_line_width(80)
                .with_aligned_dict_pairs();

            print!("{}", formatter.format(&output));
            Ok(())
        }
        Err(e) => {
            eprintln!("error: {}", e);
            std::process::exit(1);
        }
    }
}
