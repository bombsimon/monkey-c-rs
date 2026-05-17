#![allow(dead_code)]
use monkey_c_formatter::Formatter;
use monkey_c_parser::parser::Parser;

pub fn format(src: &str) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).format(&ast)
}

pub fn format_aligned(src: &str) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_aligned_dict_pairs().format(&ast)
}

pub fn format_width(src: &str, width: usize) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_line_width(width).format(&ast)
}

pub fn format_aligned_width(src: &str, width: usize) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src)
        .with_aligned_dict_pairs()
        .with_line_width(width)
        .format(&ast)
}
