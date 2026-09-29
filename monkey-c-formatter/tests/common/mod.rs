use monkey_c_formatter::Formatter;
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::parser::Parser;
use monkey_c_parser::token;

/// Run `formatter` over `src` and enforce the invariants every output must hold: the code keeps
/// the same tokens with every comment in the same place between them, and no line ends in
/// whitespace.
pub fn run(src: &str, formatter: Formatter) -> String {
    let output = Parser::new(src).parse().expect("should parse");
    let formatted = formatter.format(&output);

    let before = tokens(src);
    let after = tokens(&formatted);
    if let Some(index) = before.iter().zip(&after).position(|(a, b)| a != b) {
        let from = index.saturating_sub(3);
        panic!(
            "formatting moved a token or comment:\n  source: {:?}\n  output: {:?}\n{formatted}",
            &before[from..(index + 4).min(before.len())],
            &after[from..(index + 4).min(after.len())],
        );
    }

    assert_eq!(
        before.len(),
        after.len(),
        "formatting added or dropped tokens or comments:\n{formatted}",
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

/// The tokens of `src`, comments included, with the one rewrite the formatter makes on purpose
/// applied: `new Foo` gets its `()`. Comments are compared by their words, since re-indenting a
/// block comment and trimming the end of a line comment are expected.
fn tokens(src: &str) -> Vec<token::Type> {
    let mut lexer = Lexer::new(src);
    let mut tokens = Vec::new();
    loop {
        match lexer.next_token().1 {
            token::Type::Eof => break,
            token::Type::Comment(text) => tokens.push(token::Type::Comment(words(&text))),
            token::Type::BlockComment(text) => {
                tokens.push(token::Type::BlockComment(words(&text)));
            }
            other => tokens.push(other),
        }
    }

    with_constructor_parens(tokens)
}

fn words(text: &str) -> String {
    text.split_whitespace().collect::<Vec<_>>().join(" ")
}

fn is_comment(token: &token::Type) -> bool {
    matches!(
        token,
        token::Type::Comment(_) | token::Type::BlockComment(_)
    )
}

fn with_constructor_parens(tokens: Vec<token::Type>) -> Vec<token::Type> {
    let mut result = Vec::with_capacity(tokens.len());
    let mut tokens = tokens.into_iter().peekable();
    while let Some(token) = tokens.next() {
        let is_new = token == token::Type::New;
        result.push(token);

        if !is_new {
            continue;
        }

        while let Some(comment) = tokens.next_if(is_comment) {
            result.push(comment);
        }

        if !matches!(tokens.peek(), Some(token::Type::Identifier(_))) {
            continue;
        }

        while let Some(path) =
            tokens.next_if(|t| matches!(t, token::Type::Identifier(_) | token::Type::Dot))
        {
            result.push(path);
        }

        // Comments between the class and `(` stay there, so look past them for the `(`.
        let rest: Vec<token::Type> = tokens.by_ref().collect();
        let opens_args = rest
            .iter()
            .find(|t| !is_comment(t))
            .is_some_and(|t| *t == token::Type::LParen);
        if !opens_args {
            result.extend([token::Type::LParen, token::Type::RParen]);
        }

        result.extend(with_constructor_parens(rest));
        break;
    }

    result
}
