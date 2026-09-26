use monkey_c_formatter::Formatter;
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::parser::Parser;
use monkey_c_parser::token;

/// Run `formatter` over `src` and enforce the invariants every output must hold: the code keeps
/// the same tokens, no comment is dropped (or duplicated), and no line ends in whitespace.
pub fn run(src: &str, formatter: Formatter) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    let formatted = formatter.format(&output);

    assert_eq!(
        code_tokens(&formatted),
        code_tokens(src),
        "formatting changed the code's tokens:\n{formatted}",
    );

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

/// The tokens of `src` without comments, with the one rewrite the formatter makes on purpose
/// applied: `new Foo` gets its `()`.
fn code_tokens(src: &str) -> Vec<token::Type> {
    let mut lexer = Lexer::new(src);
    let mut tokens = Vec::new();
    loop {
        match lexer.next_token().1 {
            token::Type::Eof => break,
            token::Type::Comment(_) | token::Type::BlockComment(_) => {}
            other => tokens.push(other),
        }
    }

    with_constructor_parens(tokens)
}

fn with_constructor_parens(tokens: Vec<token::Type>) -> Vec<token::Type> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        let is_new = token == token::Type::New;
        result.push(token);

        if !is_new || !matches!(tokens.peek(), Some(token::Type::Identifier(_))) {
            continue;
        }

        while let Some(path) =
            tokens.next_if(|t| matches!(t, token::Type::Identifier(_) | token::Type::Dot))
        {
            result.push(path);
        }

        if tokens.peek() != Some(&token::Type::LParen) {
            result.extend([token::Type::LParen, token::Type::RParen]);
        }
    }

    result
}
