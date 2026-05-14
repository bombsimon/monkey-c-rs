use monkey_c_formatter::Formatter;
use monkey_c_parser::parser::Parser;

fn fmt(src: &str) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).format(&ast)
}

fn fmt_width(src: &str, width: usize) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_line_width(width).format(&ast)
}

fn fmt_aligned(src: &str) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src).with_aligned_dict_pairs().format(&ast)
}

fn fmt_aligned_width(src: &str, width: usize) -> String {
    let ast = Parser::new(src).parse().expect("should parse");
    Formatter::new(src)
        .with_aligned_dict_pairs()
        .with_line_width(width)
        .format(&ast)
}

#[test]
fn test_empty_class() {
    assert_eq!(fmt("class Foo {}"), "class Foo {}");
}

#[test]
fn test_class_with_extends() {
    assert_eq!(fmt("class Foo extends Bar {}"), "class Foo extends Bar {}");
}

#[test]
fn test_var_declaration() {
    assert_eq!(fmt("var x as Number = 1;"), "var x as Number = 1;");
}

#[test]
fn test_const_declaration() {
    assert_eq!(
        fmt("const RATE as Float = 1.5;"),
        "const RATE as Float = 1.5;"
    );
}

#[test]
fn test_type_annotation_before_initializer() {
    // Regression: old formatter emitted `var x = 1 as Float` instead of `var x as Float = 1`
    let out = fmt("class C { var x as Float = 1.0; }");
    assert!(out.contains("var x as Float = 1"), "got: {out}");
}

#[test]
fn test_visibility_modifiers() {
    let out = fmt("class C { private var x; protected var y; public var z; }");
    assert!(out.contains("private var x"));
    assert!(out.contains("protected var y"));
    assert!(out.contains("public var z"));
}

#[test]
fn test_hidden_keyword_preserved() {
    let out = fmt("class C { hidden var x; }");
    assert!(out.contains("hidden var x"), "got: {out}");
}

#[test]
fn test_function_signature() {
    let out = fmt("function foo(a as Number, b as String) as Void {}");
    assert!(out.contains("function foo(a as Number, b as String) as Void"));
}

#[test]
fn test_static_function() {
    let out = fmt("class C { static function foo() {} }");
    assert!(out.contains("static function foo()"));
}

#[test]
fn test_import() {
    assert_eq!(fmt("import Toybox.WatchUi;"), "import Toybox.WatchUi;");
}

#[test]
fn test_import_with_alias() {
    assert_eq!(
        fmt("import Toybox.WatchUi as WatchUi;"),
        "import Toybox.WatchUi as WatchUi;"
    );
}

#[test]
fn test_module() {
    let out = fmt("module MyModule { var x; }");
    assert!(out.starts_with("module MyModule {"));
    assert!(out.contains("var x;"));
}

#[test]
fn test_if_else() {
    let src = "function f() { if (x > 0) { return x; } else { return 0; } }";
    let out = fmt(src);
    assert!(out.contains("if (x > 0)"));
    assert!(out.contains("} else {"));
}

#[test]
fn test_for_loop() {
    let src = "function f() { for (var i = 0; i < 10; i++) { x = x + 1; } }";
    let out = fmt(src);
    assert!(out.contains("for (var i = 0; i < 10; i++)"));
}

#[test]
fn test_while_loop() {
    let src = "function f() { while (x > 0) { x = x - 1; } }";
    let out = fmt(src);
    assert!(out.contains("while (x > 0)"));
}

#[test]
fn test_comment_preserved() {
    let out = fmt("// a comment\nvar x = 1;");
    assert!(out.contains("// a comment"));
}

#[test]
fn test_array_no_trailing_comma_fits_inline() {
    let out = fmt("var a = [1, 2, 3];");
    assert_eq!(out.trim(), "var a = [1, 2, 3];");
}

#[test]
fn test_array_trailing_comma_forces_multiline() {
    let out = fmt("var a = [1, 2, 3,];");
    assert!(
        out.contains('\n'),
        "trailing comma should force multiline: {out}"
    );
    assert!(out.contains("1,\n"), "each element on own line: {out}");
    assert!(
        out.ends_with(",\n];") || out.trim_end().ends_with(",\n];") || out.contains(",\n]"),
        "trailing comma preserved: {out}"
    );
}

#[test]
fn test_array_no_trailing_comma_breaks_when_wide() {
    // Narrow width forces break even without trailing comma
    let out = fmt_width("var a = [\"a_long_string\", \"another_long_string\"];", 30);
    assert!(out.contains('\n'), "should break when too wide: {out}");
}

#[test]
fn test_dict_trailing_comma_forces_multiline() {
    let src = r#"var d = {"key" => "value",};"#;
    let out = fmt(src);
    assert!(
        out.contains('\n'),
        "trailing comma dict should be multiline: {out}"
    );
}

#[test]
fn test_dict_no_trailing_comma_fits_inline() {
    let src = r#"var d = {"k" => 1};"#;
    let out = fmt(src);
    assert!(!out.contains('\n'), "should stay inline: {out}");
}

#[test]
fn test_blank_line_preserved_between_methods() {
    let src = "class C {\n    function a() {}\n\n    function b() {}\n}";
    let out = fmt(src);
    assert!(
        out.contains("\n\n"),
        "blank line between methods should be preserved: {out}"
    );
}

#[test]
fn test_no_blank_line_added_without_original() {
    let src = "class C {\n    function a() {}\n    function b() {}\n}";
    let out = fmt(src);
    // Should not have a blank line where the original didn't
    let between = out.find("function b").map(|i| &out[..i]).unwrap_or("");
    let blank_lines = between.matches("\n\n").count();
    assert_eq!(blank_lines, 0, "no blank line should be added: {out}");
}

#[test]
fn test_return_statement() {
    let out = fmt("function f() { return x + 1; }");
    assert!(out.contains("return x + 1;"));
}

#[test]
fn test_bling_preserved() {
    let out = fmt("function f() { $.globalFn(); }");
    assert!(out.contains("$.globalFn()"));
}

#[test]
fn test_type_cast_expression() {
    let out = fmt("function f() { var x = arr as Array<Number>; }");
    assert!(out.contains("as Array<Number>"));
}

#[test]
fn test_new_expression() {
    let out = fmt("function f() { var x = new MyModule.Foo(); }");
    assert!(out.contains("new MyModule.Foo()"));
}

#[test]
fn test_me_keyword() {
    let out = fmt("class C { function f() { me.x = 1; } }");
    assert!(out.contains("me.x"));
}

#[test]
fn test_operator_spacing() {
    let out = fmt("function f() { var x = a + b * c; }");
    assert!(
        out.contains("a + b * c"),
        "operators should have spaces: {out}"
    );
}

#[test]
fn test_symbol_literal() {
    assert_eq!(fmt("var a = :mySymbol;"), "var a = :mySymbol;");
}

#[test]
fn test_symbol_dict_keys() {
    let out = fmt(r#"var d = {:title => "George", :name => "Taylor"};"#);
    assert!(out.contains(":title =>"), "got: {out}");
    assert!(out.contains(":name =>"), "got: {out}");
}

#[test]
fn test_aligned_dict_fits_stays_inline() {
    // Small dict fits on one line — stays inline, no padding
    let out = fmt_aligned(r#"var d = {:foo => "bar", :not_foo => "baz"};"#);
    assert!(!out.contains('\n'), "small dict should stay inline: {out}");
}

#[test]
fn test_aligned_dict_breaks_when_wide() {
    // Narrow width forces break; aligned mode pads keys to column-align =>
    let out = fmt_aligned_width(r#"var d = {:foo => "bar", :not_foo => "baz"};"#, 30);
    assert!(out.contains('\n'), "wide dict should be multiline: {out}");
    assert!(
        out.contains(":foo     =>"),
        "short key should be padded: {out}"
    );
    assert!(
        out.contains(":not_foo =>"),
        "long key should be unpadded: {out}"
    );
}

#[test]
fn test_aligned_dict_trailing_comma_forces_multiline() {
    // Trailing comma forces multiline with alignment regardless of width
    let src = "var d = {\n    :foo => \"bar\",\n    :not_foo => \"baz\",\n};";
    let out = fmt_aligned(src);
    assert!(
        out.contains('\n'),
        "trailing comma dict should be multiline: {out}"
    );
    assert!(
        out.contains(":foo     =>"),
        "short key should be padded: {out}"
    );
    assert!(
        out.contains(":not_foo =>"),
        "long key should be unpadded: {out}"
    );
}

#[test]
fn test_aligned_dict_round_trip() {
    let src = r#"var d = {:foo => "a_longer_value", :not_foo => "another_longer_value"};"#;
    let out = fmt_aligned(src);
    assert!(
        Parser::new(&out).parse().is_ok(),
        "aligned output should be valid: {out}"
    );
}

#[test]
fn test_aligned_dict_unaffected_without_flag() {
    // Without the flag, normal inline dict stays inline
    let out = fmt(r#"var d = {:foo => "bar", :not_foo => "baz"};"#);
    assert!(!out.contains('\n'), "normal fmt should stay inline: {out}");
}

#[test]
fn test_annotation_is_symbol() {
    let out = fmt("(:glance)\nfunction f() {}");
    assert!(out.contains("(:glance)"), "got: {out}");
}

#[test]
fn test_full_round_trip() {
    // Format, then parse again — should produce equivalent structure
    let src = r#"
        class MyClass extends Base {
            private var mValue as Number = 0;

            public function initialize(value as Number) as Void {
                mValue = value;
            }

            function getValue() as Number {
                return mValue;
            }
        }
    "#;
    let formatted = fmt(src);
    // Formatted output should itself be parseable
    assert!(
        Parser::new(&formatted).parse().is_ok(),
        "formatted output should be valid: {formatted}"
    );
}
