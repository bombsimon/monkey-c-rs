use monkey_c_jungle::lexer::Lexer;
use monkey_c_jungle::token;

fn tokens(source: &str) -> Vec<token::Type> {
    let mut lexer = Lexer::new(source);
    let mut tokens = Vec::new();

    loop {
        let (_, token, _) = lexer.next_token();
        if token == token::Type::Eof {
            return tokens;
        }

        tokens.push(token);
    }
}

fn word(text: &str) -> token::Type {
    token::Type::Word(text.to_string())
}

fn quoted(text: &str) -> token::Type {
    token::Type::QuotedWord(text.to_string())
}

fn deref(name: &str) -> token::Type {
    token::Type::Deref(name.to_string())
}

fn comment(text: &str) -> token::Type {
    token::Type::Comment(text.to_string())
}

#[test]
fn scans_the_language() {
    let cases = vec![
        (
            "assignment",
            "base.sourcePath = source",
            vec![word("base.sourcePath"), token::Type::Assign, word("source")],
        ),
        (
            "value list",
            "a = b;c",
            vec![
                word("a"),
                token::Type::Assign,
                word("b"),
                token::Type::Semicolon,
                word("c"),
            ],
        ),
        (
            "group",
            "[a;b]",
            vec![
                token::Type::LBracket,
                word("a"),
                token::Type::Semicolon,
                word("b"),
                token::Type::RBracket,
            ],
        ),
        (
            "dereference",
            "$(base.sourcePath)",
            vec![deref("base.sourcePath")],
        ),
        (
            "quoted value",
            "\"my projects/f.mc\"",
            vec![quoted("my projects/f.mc")],
        ),
        // A `$` only starts a dereference when a `(` follows.
        ("bare dollar", "a$b", vec![word("a$b")]),
        // Raw value text stays in one piece, separators and globs included.
        ("windows path", ".\\**.mc", vec![word(".\\**.mc")]),
        (
            "relative path",
            "../foo/bar.jungle",
            vec![word("../foo/bar.jungle")],
        ),
        ("device name", "round-360x360", vec![word("round-360x360")]),
        (
            "multibyte",
            "fēnix-resources",
            vec![word("fēnix-resources")],
        ),
        // A comment takes its own line break with it, so no newline follows one.
        (
            "comment",
            "a = b # why\nc",
            vec![
                word("a"),
                token::Type::Assign,
                word("b"),
                comment(" why"),
                word("c"),
            ],
        ),
        (
            "comment then blank line",
            "# why\n\na",
            vec![comment(" why"), token::Type::Newline, word("a")],
        ),
        // A `\` continues a line only where it starts a token; glued to a word it is just text.
        (
            "continuation",
            "a = b;\\\n  c",
            vec![
                word("a"),
                token::Type::Assign,
                word("b"),
                token::Type::Semicolon,
                word("c"),
            ],
        ),
        (
            "backslash in a word",
            "a = b\\\nc",
            vec![
                word("a"),
                token::Type::Assign,
                word("b\\"),
                token::Type::Newline,
                word("c"),
            ],
        ),
        (
            "blank lines",
            "a\n\n\nb",
            vec![
                word("a"),
                token::Type::Newline,
                token::Type::Newline,
                token::Type::Newline,
                word("b"),
            ],
        ),
        (
            "carriage returns",
            "a = b\r\nc",
            vec![
                word("a"),
                token::Type::Assign,
                word("b"),
                token::Type::Newline,
                word("c"),
            ],
        ),
        // Unclosed forms carry their raw text so the error can quote it back.
        (
            "unclosed string",
            "a = \"oops\nb",
            vec![
                word("a"),
                token::Type::Assign,
                token::Type::UnclosedString("\"oops".to_string()),
                token::Type::Newline,
                word("b"),
            ],
        ),
        (
            "unclosed dereference",
            "$(base.sourcePath",
            vec![token::Type::UnclosedDeref("$(base.sourcePath".to_string())],
        ),
        // A `#` starts a comment even inside quotes, leaving the string unclosed.
        (
            "hash inside quotes",
            "\"a#b\"",
            vec![
                token::Type::UnclosedString("\"a".to_string()),
                comment("b\""),
            ],
        ),
    ];

    for (name, source, expected) in cases {
        assert_eq!(tokens(source), expected, "{name}");
    }
}

#[test]
fn spacing_between_tokens_is_not_significant() {
    assert_eq!(tokens("a=b"), tokens("a   =\tb"));
}

#[test]
fn spans_cover_the_token_text() {
    let mut lexer = Lexer::new("ab = cd");

    assert_eq!(lexer.next_token(), (0, word("ab"), 2));
    assert_eq!(lexer.next_token(), (3, token::Type::Assign, 4));
    assert_eq!(lexer.next_token(), (5, word("cd"), 7));
    assert_eq!(lexer.next_token(), (7, token::Type::Eof, 7));
}

/// A comment's span stops before the line break it consumes, so it covers the comment alone.
#[test]
fn comment_span_excludes_the_line_break() {
    let mut lexer = Lexer::new("# why\na");

    assert_eq!(lexer.next_token(), (0, comment(" why"), 5));
    assert_eq!(lexer.next_token(), (6, word("a"), 7));
}
