fn main() {
    let input = include_str!("SpeedConverter.mc");

    let mut parser = monkey_c_parser::parser::Parser::new(input);

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
