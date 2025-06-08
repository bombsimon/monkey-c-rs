use monkey_c_parser::parser::Parser;
use std::fs;

fn main() {
    let source = fs::read_to_string("examples/SpeedConverter.mc").expect("Failed to read file");
    let mut parser = Parser::new(&source);
    match parser.parse() {
        Ok(ast) => {
            println!("Parsed successfully!");
            println!("{:#?}", ast);
        }
        Err(e) => {
            eprintln!("Error parsing: {:?}", e);
        }
    }
}
