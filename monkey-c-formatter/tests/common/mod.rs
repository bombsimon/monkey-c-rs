use monkey_c_formatter::Formatter;
use monkey_c_parser::parser::Parser;

/// Run `formatter` over `src` and enforce the invariants every output must hold: no comment is
/// dropped (or duplicated), and no line ends in whitespace.
pub fn run(src: &str, formatter: Formatter) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    let formatted = formatter.format(&output);

    let lost = Formatter::lost_comments(&output, &formatted);
    assert!(
        lost.is_empty(),
        "no comment left behind violated — {} comment(s) dropped:\n{:#?}",
        lost.len(),
        lost.iter().map(|c| c.text.as_str()).collect::<Vec<_>>(),
    );

    let trailing_whitespace: Vec<&str> = formatted
        .lines()
        .filter(|line| line.ends_with(char::is_whitespace))
        .collect();
    assert!(
        trailing_whitespace.is_empty(),
        "lines end in whitespace:\n{trailing_whitespace:#?}",
    );

    formatted
}

pub fn format(src: &str) -> String {
    run(src, Formatter::new(src))
}

pub fn format_aligned(src: &str) -> String {
    run(src, Formatter::new(src).with_alignment(true))
}
