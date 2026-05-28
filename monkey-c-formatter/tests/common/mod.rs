#![allow(dead_code)]
use monkey_c_formatter::Formatter;
use monkey_c_parser::parser::Parser;

pub fn format(src: &str) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).format(&output)
}

pub fn format_aligned(src: &str) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_alignment().format(&output)
}

pub fn format_width(src: &str, width: usize) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_line_width(width).format(&output)
}

pub fn format_aligned_width(src: &str, width: usize) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    Formatter::new(src)
        .with_alignment()
        .with_line_width(width)
        .format(&output)
}
