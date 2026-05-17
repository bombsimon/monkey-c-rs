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
fn test_using() {
    assert_eq!(fmt("using Toybox.Lang;"), "using Toybox.Lang;");
}

#[test]
fn test_enum_auto_incremented() {
    let src = "enum { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday }";
    let out = fmt(src);
    assert!(out.contains("enum {\n"));
    assert!(out.contains("    Sunday,"));
    assert!(out.contains("    Saturday\n}"));
}

#[test]
fn test_enum_explicit_values() {
    let src = "enum { x = 1337, y, z, a = 0, b, c }";
    let out = fmt(src);
    assert!(out.contains("    x = 1337,"));
    assert!(out.contains("    a = 0,"));
    assert!(out.contains("    c\n}"));
}

#[test]
fn test_enum_with_trailing_comments() {
    let src = "enum {\n    Sunday,     // 0\n    Monday,     // 1\n    Saturday    // 6\n}";
    let out = fmt(src);
    assert!(out.contains("Sunday, // 0"));
    assert!(out.contains("Saturday // 6"));
}

#[test]
fn test_enum_inside_class() {
    let out = fmt("class C { enum { A, B } }");
    assert!(out.contains("class C {"));
    assert!(out.contains("    enum {"));
    assert!(out.contains("        A,"));
    assert!(out.contains("        B\n    }"));
}

#[test]
fn test_typedef() {
    assert_eq!(
        fmt("typedef Numeric as Number or Float or Long or Double;"),
        "typedef Numeric as Number or Float or Long or Double;"
    );
}

#[test]
fn test_typedef_simple() {
    assert_eq!(fmt("typedef MyInt as Number;"), "typedef MyInt as Number;");
}

#[test]
fn test_ternary_simple() {
    let out = fmt("function f() { var x = true ? 1 : 2; }");
    assert!(out.contains("var x = true ? 1 : 2;"));
}

#[test]
fn test_ternary_nested() {
    let out = fmt("function f() { var x = a ? b : c ? d : e; }");
    assert!(out.contains("var x = a ? b : c ? d : e;"));
}

#[test]
fn test_ternary_inside_dict() {
    let src = r#"function f() { var x = { "k" => (a == null) ? "default" : a }; }"#;
    let out = fmt(src);
    assert!(out.contains("(a == null) ? \"default\" : a"));
}

#[test]
fn test_ternary_wraps_when_too_long() {
    let src = "var x = superLongCondition ? caseIfTrue : caseIfFalse;";
    let out = fmt_width(src, 40);
    // `?` and `:` lead the next lines at extra indent.
    assert!(out.contains("\n    ? caseIfTrue"));
    assert!(out.contains("\n    : caseIfFalse"));
}

#[test]
fn test_new_array_untyped() {
    let out = fmt("function f() { var a = new [size]; }");
    assert!(out.contains("var a = new [size];"));
}

#[test]
fn test_new_array_typed() {
    let out = fmt("function f() { var a = new Array<Number>[size]; }");
    assert!(out.contains("var a = new Array<Number>[size];"));
}

#[test]
fn test_new_array_size_expression() {
    let out = fmt("function f() { var a = new [20 + 30]; }");
    assert!(out.contains("var a = new [20 + 30];"));
}

#[test]
fn test_inline_dict_type_symbol_keys() {
    let out = fmt("function f(opts as { :flag as Boolean, :n as Number }) {}");
    assert!(out.contains(":flag as Boolean"));
    assert!(out.contains(":n as Number"));
}

#[test]
fn test_inline_dict_type_string_keys() {
    let out = fmt(r#"function f(o as { "name" as String, "value" as Number }) {}"#);
    assert!(out.contains("\"name\" as String"));
    assert!(out.contains("\"value\" as Number"));
}

#[test]
fn test_inline_dict_type_trailing_comma_forces_multiline() {
    // Magic-trailing-comma rule: same as dict/array literals — a source
    // trailing comma forces multi-line even when the type would otherwise fit.
    let src = "function f(o as { :a as Number, :b as String, }) {}";
    let out = fmt(src);
    assert!(out.contains(":a as Number,\n"));
    assert!(out.contains(":b as String,\n"));
}

#[test]
fn test_inline_dict_type_no_trailing_comma_fits_inline() {
    let out = fmt("function f(o as { :a as Number, :b as String }) {}");
    // No trailing comma + fits → inline.
    assert!(out.contains(":a as Number, :b as String"));
    assert!(!out.contains(":a as Number,\n"));
}

#[test]
fn test_inline_dict_type_nested() {
    let out = fmt("function f(o as { :outer as { :inner as Number } }) {}");
    assert!(out.contains(":inner as Number"));
    assert!(out.contains(":outer as"));
}

#[test]
fn test_generic_params_distinguish_comma_and_or() {
    // Comma-separated multi-param generic stays comma-separated.
    let comma = fmt("function f() { var d as Dictionary<String, Number> = 1; }");
    assert!(comma.contains("Dictionary<String, Number>"));

    // `or` inside a single param stays a union, not a second param.
    let union = fmt("function f() { var a as Array<Number or Null> = 1; }");
    assert!(union.contains("Array<Number or Null>"));

    // Nested: dict with union-typed value.
    let nested = fmt("function f() { var w as Dictionary<String, Array<Number or Null>> = 1; }");
    assert!(nested.contains("Dictionary<String, Array<Number or Null>>"));
}

#[test]
fn test_and_or_keywords_preserve_source_form() {
    // Source form is preserved: `and`/`or` stay as keywords, `&&`/`||` stay
    // as symbols.
    let kw = fmt("function f() { if (a and b or c) { x; } }");
    assert!(kw.contains("a and b or c"));

    let sym = fmt("function f() { if (a && b || c) { x; } }");
    assert!(sym.contains("a && b || c"));
}

#[test]
fn test_has_operator() {
    let out = fmt("function f() { if (WatchUI has :WatchFaceDelegate) { foo(); } }");
    assert!(out.contains("if (WatchUI has :WatchFaceDelegate)"));
}

#[test]
fn test_switch_basic() {
    let src = "function f() { switch (x) { case 1: foo(); break; case 2: bar(); break; default: baz(); } }";
    let out = fmt(src);
    assert!(out.contains("switch (x) {"));
    assert!(out.contains("        case 1:"));
    assert!(out.contains("            foo();"));
    assert!(out.contains("        default:"));
    assert!(out.contains("            baz();"));
}

#[test]
fn test_switch_instanceof_case() {
    let src =
        "function f() { switch (p) { case instanceof Number: ok(); break; default: bad(); } }";
    let out = fmt(src);
    assert!(out.contains("case instanceof Number:"));
}

#[test]
fn test_switch_leading_comment_on_case() {
    let src = "function f() {\n\
                  switch (x) {\n\
                      // first\n\
                      case 1:\n\
                          a();\n\
                          break;\n\
                      // fallback\n\
                      default:\n\
                          b();\n\
                  }\n\
              }";
    let out = fmt(src);
    // Leading comments live at the case-header indent, not nested inside
    // the previous case's body.
    assert!(out.contains("        // first\n        case 1:"));
    assert!(out.contains("        // fallback\n        default:"));
}

#[test]
fn test_switch_fall_through() {
    let src = "function f() { switch (x) { case 1: foo(); case 2: bar(); break; } }";
    let out = fmt(src);
    // First case has no break — falls through. No `break;` should appear
    // between the two `System.println` equivalents.
    assert!(out.contains("        case 1:\n            foo();\n        case 2:"));
}

#[test]
fn test_try_catch_basic() {
    let out = fmt("function f() { try { foo(); } catch (e) { log(e); } }");
    assert!(out.contains("try {"));
    assert!(out.contains("} catch (e) {"));
}

#[test]
fn test_try_catch_typed_and_finally() {
    let src = "function f() { \
                  try { foo(); } \
                  catch (e instanceof MyException) { log(e); } \
                  catch (e) { other(e); } \
                  finally { cleanup(); } \
              }";
    let out = fmt(src);
    assert!(out.contains("} catch (e instanceof MyException) {"));
    assert!(out.contains("} catch (e) {"));
    assert!(out.contains("} finally {"));
}

#[test]
fn test_throw() {
    let out = fmt("function f() { throw new Lang.Exception(); }");
    assert!(out.contains("throw new Lang.Exception();"));
}

#[test]
fn test_method_symbol_call() {
    assert_eq!(
        fmt("function f() { var cb = method(:onReceive); }"),
        "function f() {\n    var cb = method(:onReceive);\n}"
    );
}

#[test]
fn test_member_method_symbol_call() {
    let out = fmt("function f() { obj.method(:onTap); }");
    assert!(out.contains("obj.method(:onTap);"));
}

#[test]
fn test_top_level_annotation_preserved() {
    let out = fmt("(:test)\nfunction foo() {}");
    assert!(out.contains("(:test)"));
    assert!(out.contains("function foo()"));
}

#[test]
fn test_ternary_stays_flat_when_short() {
    let out = fmt("function f() { var x = a ? b : c; }");
    assert!(out.contains("var x = a ? b : c;"));
    assert!(!out.contains("\n    ?"));
}

#[test]
fn test_using_with_alias() {
    assert_eq!(
        fmt("using Toybox.Lang as Lng;"),
        "using Toybox.Lang as Lng;"
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
fn test_else_if_chain() {
    let src = "function f() { if (a) { return 1; } else if (b) { return 2; } else { return 3; } }";
    let out = fmt(src);
    assert!(out.contains("} else if (b) {"));
    assert!(out.contains("} else {"));
}

#[test]
fn test_if_condition_with_member_access() {
    let src = "function f() { if (obj.flag) { return 1; } }";
    let out = fmt(src);
    assert!(out.contains("if (obj.flag)"));
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
fn test_block_comment_preserved() {
    let out = fmt("/* header */\nvar x = 1;");
    assert!(out.contains("/* header */"));
}

#[test]
fn test_block_comment_in_function_body() {
    let out = fmt("function f() { /* note */ var x = 1; }");
    assert!(out.contains("/* note */"));
}

#[test]
fn test_multiline_block_comment_aligns_close() {
    let src = "function f() {\n    /* line 1\n       line 2 */\n    var x = 1;\n}\n";
    let out = fmt(src);
    // `*/` should be on its own line at the function-body indent.
    assert!(out.contains("\n    */\n"));
    // Should not leave `*/` dangling after content.
    assert!(!out.contains("line 2 */"));
}

#[test]
fn test_multiline_block_comment_at_top_level() {
    let src = "/* header\n   text */\nfunction f() {}\n";
    let out = fmt(src);
    assert!(out.contains("\n*/\n"));
}

#[test]
fn test_multiline_block_comment_nested() {
    let src = "function f() {\n    if (x) {\n        /* a\n           b */\n        var y = 1;\n    }\n}\n";
    let out = fmt(src);
    // `*/` at the if-body indent (8 spaces).
    assert!(out.contains("\n        */\n"));
}

#[test]
fn test_hex_literal_preserves_casing() {
    let out = fmt("var a = 0xFFCC00; var b = 0xffcc00; var c = 0xAbCdEf;");
    assert!(out.contains("0xFFCC00"));
    assert!(out.contains("0xffcc00"));
    assert!(out.contains("0xAbCdEf"));
}

#[test]
fn test_if_else_with_trailing_comments() {
    let src = "function f() {\n\
                  if (a) { return 1; } // first\n\
                  else if (b) { return 2; } // second\n\
                  else { return 3; }\n\
              }";
    let out = fmt(src);
    assert!(out.contains("} // first"));
    assert!(out.contains("} // second"));
    assert!(out.contains("else if (b)"));
}

#[test]
fn test_if_trailing_comment_no_else() {
    let out = fmt("function f() { if (a) { return 1; } // tail\n}");
    assert!(out.contains("} // tail"));
}

#[test]
fn test_dict_entry_trailing_comment() {
    let out = fmt("function f() { var x = {:a => 1, :b => 2, // note\n}; }");
    assert!(out.contains(":b => 2, // note"));
}

#[test]
fn test_array_entry_trailing_comment() {
    let out = fmt("function f() { var x = [1, 2, // mid\n3]; }");
    assert!(out.contains("2, // mid"));
}

#[test]
fn test_call_arg_trailing_comment() {
    let out = fmt("function f() { foo(1, // first\n2); }");
    assert!(out.contains("1, // first"));
}

#[test]
fn test_new_arg_trailing_comment() {
    let out = fmt("function f() { var x = new Foo(1, // first\n2); }");
    assert!(out.contains("1, // first"));
}

#[test]
fn test_function_param_trailing_comment() {
    let out = fmt("function foo(a, // first\nb) {}");
    assert!(out.contains("a, // first"));
}

#[test]
fn test_empty_dict_with_tail_comment() {
    let out = fmt("function f() { var x = {/* nothing */}; }");
    assert!(out.contains("/* nothing */"));
}

#[test]
fn test_if_condition_wraps_at_logical_ops() {
    let src = "function fn() { if (somethingVeryLong > 1 \
               || somethingEvenLongerThisTime > 1 \
               || somethingLongSoWeshouldBreak > 1) { var x = 1; } }";
    let out = fmt(src);
    // Operators lead the next line at extra indent.
    assert!(out.contains("|| somethingEvenLongerThisTime > 1"));
    assert!(out.contains("|| somethingLongSoWeshouldBreak > 1)"));
    // `{` migrates to its own line when the condition wraps.
    assert!(out.contains(")\n    {"));
}

#[test]
fn test_if_condition_stays_flat_when_short() {
    let out = fmt("function f() { if (a || b) { return 1; } }");
    assert!(out.contains("if (a || b) {"));
    // No newline before `{`.
    assert!(!out.contains(")\n    {"));
}

#[test]
fn test_parens_preserved_for_required_grouping() {
    // (a + b) * c must keep parens — different value otherwise.
    let out = fmt("function f() { var x = (a + b) * c; }");
    assert!(out.contains("(a + b) * c"));
}

#[test]
fn test_parens_preserved_when_redundant() {
    // We preserve user intent rather than stripping.
    let out = fmt("function f() { var x = (a - b) - c; }");
    assert!(out.contains("(a - b) - c"));
}

#[test]
fn test_nested_parens_preserved() {
    let out = fmt("function f() { var x = (((a.b) * 91 / 360) * c); }");
    assert!(out.contains("(((a.b) * 91 / 360) * c)"));
}

#[test]
fn test_while_condition_no_double_parens() {
    let out = fmt("function f() { while (x > 0) { x = x - 1; } }");
    assert!(out.contains("while (x > 0)"));
    assert!(!out.contains("while ((x > 0))"));
}

#[test]
fn test_if_condition_wraps_on_and_chain() {
    let src = "function f() { if (longConditionOne > 1 \
               && longConditionTwo > 1 \
               && longConditionThree > 1) { return 1; } }";
    let out = fmt(src);
    assert!(out.contains("&& longConditionTwo > 1"));
    assert!(out.contains("&& longConditionThree > 1)"));
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
