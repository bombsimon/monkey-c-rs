use monkey_c_parser::ast::{DoubleLit, FloatLit};
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::token::Type;

fn tokens(input: &str) -> Vec<Type> {
    let mut lexer = Lexer::new(input);
    let mut out = Vec::new();
    loop {
        let (_, tok, _) = lexer.next_token();
        if tok == Type::Eof {
            break;
        }
        out.push(tok);
    }
    out
}

fn spans(input: &str) -> Vec<(usize, Type, usize)> {
    let mut lexer = Lexer::new(input);
    let mut out = Vec::new();
    loop {
        let (start, tok, end) = lexer.next_token();
        if tok == Type::Eof {
            break;
        }
        out.push((start, tok, end));
    }
    out
}

#[test]
fn test_var_declaration() {
    assert_eq!(
        tokens("var x = 5.0;"),
        vec![
            Type::Var,
            Type::Identifier("x".into()),
            Type::Assign,
            Type::Float(FloatLit {
                digits: "5.0".into(),
                has_dot: true,
                leading_dot: false,
                suffix: None,
                exponent: None,
            }),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_string_literal() {
    assert_eq!(
        tokens(r#"x = "hello world";"#),
        vec![
            Type::Identifier("x".into()),
            Type::Assign,
            Type::String("hello world".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_string_escape_sequences() {
    // The lexer preserves the raw source text between the quotes, with escape
    // sequences left intact — `\n` stays the two characters `\` and `n`, and
    // escapes the lexer doesn't decode (`\x41`) survive verbatim rather than
    // being mangled. A `\"` does not terminate the string.
    assert_eq!(
        tokens(r#"x = "line1\nline2\x41\"end";"#),
        vec![
            Type::Identifier("x".into()),
            Type::Assign,
            Type::String(r#"line1\nline2\x41\"end"#.into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_string_multibyte_utf8() {
    assert_eq!(
        tokens(r#"x = "°😀";"#),
        vec![
            Type::Identifier("x".into()),
            Type::Assign,
            Type::String("°😀".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_comment() {
    assert_eq!(
        tokens("// a comment\nvar x = 1;"),
        vec![
            Type::Comment(" a comment".into()),
            Type::Var,
            Type::Identifier("x".into()),
            Type::Assign,
            Type::Number("1".to_string()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_comment_excludes_crlf_line_ending() {
    assert_eq!(
        tokens("// a comment\r\nvar x;"),
        vec![
            Type::Comment(" a comment".into()),
            Type::Var,
            Type::Identifier("x".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_comment_keeps_lone_carriage_return() {
    assert_eq!(tokens("// a\rb\n"), vec![Type::Comment(" a\rb".into())]);
}

#[test]
fn test_hex_literal() {
    assert_eq!(
        tokens("0xffcc00 0xFFCC00 0xFfCc00 0XFF"),
        vec![
            Type::Hex("0xffcc00".into()),
            Type::Hex("0xFFCC00".into()),
            Type::Hex("0xFfCc00".into()),
            Type::Hex("0XFF".into()),
        ]
    );
}

#[test]
fn test_float_suffix() {
    assert_eq!(
        tokens("0f 5F 0.5f"),
        vec![
            Type::Float(FloatLit {
                digits: "0".into(),
                has_dot: false,
                leading_dot: false,
                suffix: Some('f'),
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: "5".into(),
                has_dot: false,
                leading_dot: false,
                suffix: Some('F'),
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: "0.5".into(),
                has_dot: true,
                leading_dot: false,
                suffix: Some('f'),
                exponent: None,
            }),
        ]
    );
}

#[test]
fn test_leading_dot_with_any_suffix() {
    assert_eq!(
        tokens(".5d .5l"),
        vec![
            Type::Double(DoubleLit {
                digits: ".5".into(),
                has_dot: true,
                leading_dot: true,
                exponent: None,
                suffix: 'd',
            }),
            Type::Long(".5l".into()),
        ]
    );
}

#[test]
fn test_leading_dot_float() {
    assert_eq!(
        tokens(".978 .5f"),
        vec![
            Type::Float(FloatLit {
                digits: ".978".into(),
                has_dot: true,
                leading_dot: true,
                suffix: None,
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: ".5".into(),
                has_dot: true,
                leading_dot: true,
                suffix: Some('f'),
                exponent: None,
            }),
        ]
    );
}

#[test]
fn test_integer_method_call() {
    // `0.toFloat()` — the `.` is member access, not a decimal point, since
    // it isn't followed by a digit.
    assert_eq!(
        tokens("0.toFloat()"),
        vec![
            Type::Number("0".to_string()),
            Type::Dot,
            Type::Identifier("toFloat".into()),
            Type::LParen,
            Type::RParen,
        ]
    );
}

#[test]
fn test_char_literal() {
    assert_eq!(
        tokens(r#"'a' 'B' '\n' '\'' '°'"#),
        vec![
            Type::Char("a".into()),
            Type::Char("B".into()),
            // Raw text is preserved: `\n` stays two characters, and the
            // escaped quote `\'` does not terminate the literal.
            Type::Char(r#"\n"#.into()),
            Type::Char(r#"\'"#.into()),
            Type::Char("°".into()),
        ]
    );
}

#[test]
fn test_hex_long_literal() {
    assert_eq!(
        tokens("0x800l 0xCAFEL 0X1l"),
        vec![
            Type::HexLong("0x800l".into()),
            Type::HexLong("0xCAFEL".into()),
            Type::HexLong("0X1l".into()),
        ]
    );
}

#[test]
fn test_symbol_literal() {
    assert_eq!(
        tokens(":mySymbol"),
        vec![Type::Colon, Type::Identifier("mySymbol".into())]
    );
    assert_eq!(
        tokens(":_private"),
        vec![Type::Colon, Type::Identifier("_private".into())]
    );
    // plain colon with no following identifier stays as Colon
    assert_eq!(tokens(":"), vec![Type::Colon]);
}

#[test]
fn test_bling() {
    assert_eq!(
        tokens("$.globalFn()"),
        vec![
            Type::Bling,
            Type::Dot,
            Type::Identifier("globalFn".into()),
            Type::LParen,
            Type::RParen,
        ]
    );
}

#[test]
fn test_stray_control_character_skipped() {
    // `monkeyc` tolerates stray C0 control characters (e.g. a leftover `DC3`
    // byte, 0x13) between tokens.
    assert_eq!(
        tokens("extends Foo \x13{"),
        vec![Type::Extends, Type::Identifier("Foo".into()), Type::LBrace,]
    );
}

#[test]
fn test_stray_backtick_skipped() {
    // `monkeyc` ignores stray backticks next to identifiers, e.g. an enum
    // value written as `BATTERY\``.
    assert_eq!(
        tokens("BATTERY`,"),
        vec![Type::Identifier("BATTERY".into()), Type::Comma]
    );
}

#[test]
fn test_at_resource_ref() {
    // `@Rez.Strings.foo` — legacy resource reference syntax.
    assert_eq!(
        tokens("@Rez.Strings.foo"),
        vec![
            Type::At,
            Type::Identifier("Rez".into()),
            Type::Dot,
            Type::Identifier("Strings".into()),
            Type::Dot,
            Type::Identifier("foo".into()),
        ]
    );
}

#[test]
fn test_exponent_notation() {
    assert_eq!(
        tokens("6371e3 6371E3 10e-2 1.5e+10 6371e3d"),
        vec![
            Type::Float(FloatLit {
                digits: "6371".into(),
                has_dot: false,
                leading_dot: false,
                suffix: None,
                exponent: Some("e3".into()),
            }),
            Type::Float(FloatLit {
                digits: "6371".into(),
                has_dot: false,
                leading_dot: false,
                suffix: None,
                exponent: Some("E3".into()),
            }),
            Type::Float(FloatLit {
                digits: "10".into(),
                has_dot: false,
                leading_dot: false,
                suffix: None,
                exponent: Some("e-2".into()),
            }),
            Type::Float(FloatLit {
                digits: "1.5".into(),
                has_dot: true,
                leading_dot: false,
                suffix: None,
                exponent: Some("e+10".into()),
            }),
            Type::Double(DoubleLit {
                digits: "6371".into(),
                has_dot: false,
                leading_dot: false,
                exponent: Some("e3".into()),
                suffix: 'd',
            }),
        ]
    );
}

#[test]
fn test_line_continuation() {
    // A `\` immediately followed by a newline lets an expression span
    // multiple lines and is skipped like whitespace.
    assert_eq!(
        tokens(
            r#"a + \
b"#
        ),
        vec![
            Type::Identifier("a".into()),
            Type::Plus,
            Type::Identifier("b".into()),
        ]
    );
}

#[test]
fn test_parser_continues_on_illegal_token() {
    assert_eq!(
        tokens("var x; # var y;"),
        vec![
            Type::Var,
            Type::Identifier("x".into()),
            Type::Semicolon,
            Type::Illegal,
            Type::Var,
            Type::Identifier("y".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_illegal_multibyte_character_is_consumed_whole() {
    // "€" is three bytes; stepping a single byte would leave the cursor mid-character and panic the
    // next slice of the input.
    assert_eq!(
        spans("€ var"),
        vec![(0, Type::Illegal, 3), (4, Type::Var, 7)]
    );
}

#[test]
fn test_block_comment() {
    assert_eq!(
        tokens("/* line 1\n   line 2 */ var"),
        vec![Type::BlockComment(" line 1\n   line 2 ".into()), Type::Var]
    );
}

#[test]
fn test_operators() {
    assert_eq!(
        tokens("+= -= *= /= %= &= |= ^= <<= >>= << >> == != <= >= < > ++ -- => = && ||"),
        vec![
            Type::AddAssign,
            Type::SubAssign,
            Type::MulAssign,
            Type::DivAssign,
            Type::ModAssign,
            Type::BitAndAssign,
            Type::BitOrAssign,
            Type::BitXorAssign,
            Type::LeftShiftAssign,
            Type::RightShiftAssign,
            Type::LeftShift,
            Type::RightShift,
            Type::EqualEqual,
            Type::BangEqual,
            Type::LessEqual,
            Type::GreaterEqual,
            Type::Less,
            Type::Greater,
            Type::PlusPlus,
            Type::MinusMinus,
            Type::FatArrow,
            Type::Assign,
            Type::And,
            Type::Or,
        ]
    );
}

#[test]
fn test_keywords_and_literals() {
    assert_eq!(
        tokens("private hidden static true false null NaN me self"),
        vec![
            Type::Private,
            Type::Hidden,
            Type::Static,
            Type::Boolean(true),
            Type::Boolean(false),
            Type::Null,
            Type::NaN,
            Type::Me,
            Type::Self_,
        ]
    );
}

#[test]
fn test_span_positions() {
    assert_eq!(
        spans("var   x;"),
        vec![
            (0, Type::Var, 3),
            (6, Type::Identifier("x".into()), 7),
            (7, Type::Semicolon, 8),
        ]
    );
}
