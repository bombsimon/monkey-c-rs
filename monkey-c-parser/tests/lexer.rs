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
            Type::Double(5.0),
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
fn test_comment() {
    assert_eq!(
        tokens("// a comment\nvar x = 1;"),
        vec![
            Type::Comment(" a comment".into()),
            Type::Var,
            Type::Identifier("x".into()),
            Type::Assign,
            Type::Long(1),
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
fn test_symbol_after_call_paren_is_not_annotation() {
    // `method(:foo)` must lex as Identifier, LParen, Symbol, RParen — not as
    // Identifier + Annotation.
    assert_eq!(
        tokens("method(:foo)"),
        vec![
            Type::Identifier("method".into()),
            Type::LParen,
            Type::Symbol("foo".into()),
            Type::RParen,
        ]
    );
}

#[test]
fn test_annotation_bytes_are_paren_symbol_paren() {
    // The lexer is context-free — `(:test)` is just three tokens. The parser
    // reconstructs an annotation at declaration position.
    assert_eq!(
        tokens("(:test)"),
        vec![Type::LParen, Type::Symbol("test".into()), Type::RParen]
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
fn test_annotation() {
    assert_eq!(
        tokens("(:test)\nfunction foo() {}"),
        vec![
            Type::LParen,
            Type::Symbol("test".into()),
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
    assert_eq!(tokens(":mySymbol"), vec![Type::Symbol("mySymbol".into())]);
    assert_eq!(tokens(":_private"), vec![Type::Symbol("_private".into())]);
    // plain colon (e.g. dict separator) is NOT a symbol
    assert_eq!(tokens(":"), vec![Type::Colon]);
}

#[test]
fn test_fat_arrow() {
    assert_eq!(
        tokens(":key => 1"),
        vec![Type::Symbol("key".into()), Type::FatArrow, Type::Long(1)]
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
fn test_compound_operators() {
    assert_eq!(
        tokens("x += 1; y -= 2; z *= 3;"),
        vec![
            Type::Identifier("x".into()),
            Type::AddAssign,
            Type::Long(1),
            Type::Semicolon,
            Type::Identifier("y".into()),
            Type::SubAssign,
            Type::Long(2),
            Type::Semicolon,
            Type::Identifier("z".into()),
            Type::MulAssign,
            Type::Long(3),
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
    assert_eq!(tokens("42"), vec![Type::Long(42)]);
}

#[test]
fn test_float_literal() {
    assert_eq!(tokens("1.5"), vec![Type::Double(1.5)]);
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
