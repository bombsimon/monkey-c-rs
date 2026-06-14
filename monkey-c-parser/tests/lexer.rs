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
                has_suffix: false,
                exponent: None,
            }),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_class_declaration() {
    assert_eq!(
        tokens("class MyClass { var x as Float; }"),
        vec![
            Type::Class,
            Type::Identifier("MyClass".into()),
            Type::LBrace,
            Type::Var,
            Type::Identifier("x".into()),
            Type::As,
            Type::Identifier("Float".into()),
            Type::Semicolon,
            Type::RBrace,
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
    assert_eq!(
        tokens(r#"x = "line1\nline2";"#),
        vec![
            Type::Identifier("x".into()),
            Type::Assign,
            Type::String("line1\nline2".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_shift_operators() {
    assert_eq!(
        tokens("x << 1 >> 2 <<= 3 >>= 4"),
        vec![
            Type::Identifier("x".into()),
            Type::LeftShift,
            Type::Number(1),
            Type::RightShift,
            Type::Number(2),
            Type::LeftShiftAssign,
            Type::Number(3),
            Type::RightShiftAssign,
            Type::Number(4),
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
            Type::Number(1),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_block_comment() {
    assert_eq!(
        tokens("/* block */ var x;"),
        vec![
            Type::BlockComment(" block ".into()),
            Type::Var,
            Type::Identifier("x".into()),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_block_comment_multi_line() {
    assert_eq!(
        tokens("/* line 1\n   line 2 */"),
        vec![Type::BlockComment(" line 1\n   line 2 ".into())]
    );
}

#[test]
fn test_symbol_after_call_paren() {
    // `:foo` lexes as Colon + Identifier — the parser decides context.
    assert_eq!(
        tokens("method(:foo)"),
        vec![
            Type::Identifier("method".into()),
            Type::LParen,
            Type::Colon,
            Type::Identifier("foo".into()),
            Type::RParen,
        ]
    );
}

#[test]
fn test_annotation_bytes_are_paren_colon_ident_paren() {
    // The lexer is context-free — `(:test)` is four tokens. The parser
    // reconstructs an annotation at declaration position.
    assert_eq!(
        tokens("(:test)"),
        vec![
            Type::LParen,
            Type::Colon,
            Type::Identifier("test".into()),
            Type::RParen,
        ]
    );
}

#[test]
fn test_hex_literal() {
    assert_eq!(
        tokens("0xffcc00 0xFFCC00 0xFfCc00"),
        vec![
            Type::Hex("ffcc00".into()),
            Type::Hex("FFCC00".into()),
            Type::Hex("FfCc00".into()),
        ]
    );
}

#[test]
fn test_float_suffix() {
    assert_eq!(
        tokens("0f 5f 0.5f"),
        vec![
            Type::Float(FloatLit {
                digits: "0".into(),
                has_dot: false,
                leading_dot: false,
                has_suffix: true,
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: "5".into(),
                has_dot: false,
                leading_dot: false,
                has_suffix: true,
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: "0.5".into(),
                has_dot: true,
                leading_dot: false,
                has_suffix: true,
                exponent: None,
            }),
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
                has_suffix: false,
                exponent: None,
            }),
            Type::Float(FloatLit {
                digits: ".5".into(),
                has_dot: true,
                leading_dot: true,
                has_suffix: true,
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
            Type::Number(0),
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
            Type::Char("\n".into()),
            Type::Char("'".into()),
            Type::Char("°".into()),
        ]
    );
}

#[test]
fn test_hex_long_literal() {
    assert_eq!(
        tokens("0x800l 0xCAFEl"),
        vec![Type::HexLong("800".into()), Type::HexLong("CAFE".into()),]
    );
}

#[test]
fn test_annotation() {
    assert_eq!(
        tokens("(:test)\nfunction foo() {}"),
        vec![
            Type::LParen,
            Type::Colon,
            Type::Identifier("test".into()),
            Type::RParen,
            Type::Function,
            Type::Identifier("foo".into()),
            Type::LParen,
            Type::RParen,
            Type::LBrace,
            Type::RBrace,
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
fn test_fat_arrow() {
    assert_eq!(
        tokens(":key => 1"),
        vec![
            Type::Colon,
            Type::Identifier("key".into()),
            Type::FatArrow,
            Type::Number(1),
        ]
    );
    // = alone is still Assign, == is still EqualEqual
    assert_eq!(tokens("= =>"), vec![Type::Assign, Type::FatArrow]);
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
fn test_compound_operators() {
    assert_eq!(
        tokens("x += 1; y -= 2; z *= 3;"),
        vec![
            Type::Identifier("x".into()),
            Type::AddAssign,
            Type::Number(1),
            Type::Semicolon,
            Type::Identifier("y".into()),
            Type::SubAssign,
            Type::Number(2),
            Type::Semicolon,
            Type::Identifier("z".into()),
            Type::MulAssign,
            Type::Number(3),
            Type::Semicolon,
        ]
    );
}

#[test]
fn test_comparison_operators() {
    assert_eq!(
        tokens("a == b != c <= d >= e < f > g"),
        vec![
            Type::Identifier("a".into()),
            Type::EqualEqual,
            Type::Identifier("b".into()),
            Type::BangEqual,
            Type::Identifier("c".into()),
            Type::LessEqual,
            Type::Identifier("d".into()),
            Type::GreaterEqual,
            Type::Identifier("e".into()),
            Type::Less,
            Type::Identifier("f".into()),
            Type::Greater,
            Type::Identifier("g".into()),
        ]
    );
}

#[test]
fn test_span_positions() {
    let toks = spans("var x;");
    assert_eq!(toks[0], (0, Type::Var, 3));
    assert_eq!(toks[1], (4, Type::Identifier("x".into()), 5));
    assert_eq!(toks[2], (5, Type::Semicolon, 6));
}

#[test]
fn test_span_skips_whitespace() {
    let toks = spans("var   x;");
    // `x` starts at offset 6 (after "var   ")
    assert_eq!(toks[1].0, 6);
}

#[test]
fn test_integer_literal() {
    assert_eq!(tokens("42"), vec![Type::Number(42)]);
}

#[test]
fn test_float_literal() {
    assert_eq!(
        tokens("1.5"),
        vec![Type::Float(FloatLit {
            digits: "1.5".into(),
            has_dot: true,
            leading_dot: false,
            has_suffix: false,
            exponent: None,
        })]
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
                has_suffix: false,
                exponent: Some("e3".into()),
            }),
            Type::Float(FloatLit {
                digits: "6371".into(),
                has_dot: false,
                leading_dot: false,
                has_suffix: false,
                exponent: Some("E3".into()),
            }),
            Type::Float(FloatLit {
                digits: "10".into(),
                has_dot: false,
                leading_dot: false,
                has_suffix: false,
                exponent: Some("e-2".into()),
            }),
            Type::Float(FloatLit {
                digits: "1.5".into(),
                has_dot: true,
                leading_dot: false,
                has_suffix: false,
                exponent: Some("e+10".into()),
            }),
            Type::Double(DoubleLit {
                digits: "6371".into(),
                has_dot: false,
                leading_dot: false,
                exponent: Some("e3".into()),
            }),
        ]
    );
}

#[test]
fn test_boolean_literals() {
    assert_eq!(
        tokens("true false"),
        vec![Type::Boolean(true), Type::Boolean(false)]
    );
}

#[test]
fn test_null_nan() {
    assert_eq!(tokens("null NaN"), vec![Type::Null, Type::NaN]);
}

#[test]
fn test_increment_decrement() {
    assert_eq!(
        tokens("i++ --j"),
        vec![
            Type::Identifier("i".into()),
            Type::PlusPlus,
            Type::MinusMinus,
            Type::Identifier("j".into()),
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
fn test_visibility_keywords() {
    assert_eq!(
        tokens("private protected public hidden static"),
        vec![
            Type::Private,
            Type::Protected,
            Type::Public,
            Type::Hidden,
            Type::Static,
        ]
    );
}
