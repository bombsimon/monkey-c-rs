#![allow(dead_code)]
use monkey_c_formatter::Formatter;
use monkey_c_parser::parser::Parser;

/// Run `formatter` over `src` and enforce the "no comment left behind"
/// invariant: every test/fixture that flows through these helpers fails
/// loudly if the formatter drops (or duplicates) a comment.
fn run(src: &str, formatter: Formatter) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    let formatted = formatter.format(&output);

    let lost = Formatter::lost_comments(&output, &formatted);
    assert!(
        lost.is_empty(),
        "no comment left behind violated — {} comment(s) dropped:\n{:#?}",
        lost.len(),
        lost.iter().map(|c| c.text.as_str()).collect::<Vec<_>>(),
    );

    formatted
}

pub fn format(src: &str) -> String {
    run(src, Formatter::new(src))
}

pub fn format_aligned(src: &str) -> String {
    run(src, Formatter::new(src).with_alignment(true))
}

pub fn format_all_enabled(src: &str) -> String {
    run(
        src,
        Formatter::new(src)
            .with_alignment(true)
            .with_decl_wrap(true),
    )
}

pub fn format_width(src: &str, width: usize) -> String {
    run(src, Formatter::new(src).with_line_width(width))
}

pub fn format_aligned_width(src: &str, width: usize) -> String {
    run(
        src,
        Formatter::new(src)
            .with_alignment(true)
            .with_line_width(width),
    )
}
