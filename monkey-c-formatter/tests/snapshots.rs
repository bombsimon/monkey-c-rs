//! Formatter snapshot tests.
//!
//! The snapshot body is the formatter output. For inline tests the `expression`
//! field in the snapshot header captures the input; for file-driven tests the
//! `input_file` field points at the `.mc` source.
//!
//! Snapshots live under `tests/snapshots/`. See <https://insta.rs/> for the
//! snapshot tooling.
mod common;
use common::{format, format_aligned, format_aligned_width, format_all_enabled, format_width};

#[test]
fn format_inputs() {
    insta::glob!("snapshot_inputs/*.mc", |path| {
        let raw = std::fs::read_to_string(path).expect("read input file");
        let input = raw.trim();
        insta::with_settings!({ omit_expression => true }, {
            insta::assert_snapshot!(format_all_enabled(input));
        });
    });
}

#[test]
fn empty_class() {
    insta::assert_snapshot!(format("class Foo {}"));
}

#[test]
fn class_with_extends() {
    insta::assert_snapshot!(format("class Foo extends Bar {}"));
}

#[test]
fn module_with_var() {
    insta::assert_snapshot!(format("module MyModule { var x; }"));
}

#[test]
fn import_and_using() {
    insta::assert_snapshot!(format(
        r#"
import Toybox.WatchUi;
using Toybox.Lang;
using Toybox.Lang as Lng;
"#
        .trim()
    ));
}

#[test]
fn typedef_variants() {
    insta::assert_snapshot!(format(
        r#"
typedef MyInt as Number;
typedef Numeric as Number or Float or Long or Double;
"#
        .trim()
    ));
}

#[test]
fn enum_variants() {
    insta::assert_snapshot!(format(
        r#"
enum { Sunday, Monday, Tuesday, Wednesday, Thursday, Friday, Saturday }

enum { x = 1337, y, z, a = 0, b, c }
"#
        .trim()
    ));
}

#[test]
fn enum_inside_class() {
    insta::assert_snapshot!(format("class C { enum { A, B } }"));
}

#[test]
fn function_signature_with_types() {
    insta::assert_snapshot!(format("function foo(a as Number, b as String) as Void {}"));
}

#[test]
fn function_static() {
    insta::assert_snapshot!(format("class C { static function foo() {} }"));
}

#[test]
fn var_and_const_with_type() {
    insta::assert_snapshot!(format(
        r#"
var x as Number = 1;
const RATE as Float = 1.5;
"#
        .trim()
    ));
}

#[test]
fn var_type_annotation_before_initializer() {
    // Regression: ensure we emit `var x as Float = 1` not `var x = 1 as Float`.
    insta::assert_snapshot!(format("class C { var x as Float = 1.0; }"));
}

#[test]
fn class_visibility_modifiers() {
    insta::assert_snapshot!(format(
        r#"
class C {
    private var x;
    protected var y;
    public var z;
    hidden var w;
}
"#
        .trim()
    ));
}

#[test]
fn binary_operator_spacing() {
    insta::assert_snapshot!(format("function f() { var x = a + b * c; }"));
}

#[test]
fn return_with_expression() {
    insta::assert_snapshot!(format("function f() { return x + 1; }"));
}

#[test]
fn ternary_variants() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var simple = true ? 1 : 2;
    var nested = a ? b : c ? d : e;
    var short = a ? b : c;
    var in_dict = { "k" => (a == null) ? "default" : a };
}
"#
        .trim()
    ));
}

#[test]
fn ternary_wraps_when_too_long() {
    insta::assert_snapshot!(format_width(
        "var x = superLongCondition ? caseIfTrue : caseIfFalse;",
        40
    ));
}

#[test]
fn parens_preserved() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var required = (a + b) * c;
    var redundant = (a - b) - c;
    var nested = (((a.b) * 91 / 360) * c);
}
"#
        .trim()
    ));
}

#[test]
fn type_cast_expression() {
    insta::assert_snapshot!(format("function f() { var x = arr as Array<Number>; }"));
}

#[test]
fn new_expression_dotted() {
    insta::assert_snapshot!(format("function f() { var x = new MyModule.Foo(); }"));
}

#[test]
fn new_expression_with_self_reference() {
    insta::assert_snapshot!(format("function f() { return new self.classDef_(); }"));
}

#[test]
fn new_expression_without_parens() {
    // `new Foo` (no argument list) is normalised to `new Foo()`.
    insta::assert_snapshot!(format("function f() { var x = new MyModule.Foo; }"));
}

#[test]
fn new_array_variants() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var untyped = new [size];
    var typed = new Array<Number>[size];
    var expr_size = new [20 + 30];
}
"#
        .trim()
    ));
}

#[test]
fn me_keyword() {
    insta::assert_snapshot!(format("class C { function f() { me.x = 1; } }"));
}

#[test]
fn bling_preserved() {
    insta::assert_snapshot!(format("function f() { $.globalFn(); }"));
}

#[test]
fn symbol_literal() {
    insta::assert_snapshot!(format("var a = :mySymbol;"));
}

#[test]
fn method_symbol_calls() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var cb = method(:onReceive);
    obj.method(:onTap);
}
"#
        .trim()
    ));
}

#[test]
fn hex_literals_preserve_casing() {
    insta::assert_snapshot!(format(
        "var a = 0xFFCC00; var b = 0xffcc00; var c = 0xAbCdEf;"
    ));
}

#[test]
fn has_operator() {
    insta::assert_snapshot!(format(
        "function f() { if (WatchUI has :WatchFaceDelegate) { foo(); } }"
    ));
}

#[test]
fn and_or_source_form_preserved() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    if (a and b or c) { x; }
    if (a && b || c) { y; }
}
"#
        .trim()
    ));
}

#[test]
fn dict_symbol_keys_inline() {
    insta::assert_snapshot!(format(
        r#"var d = {:title => "George", :name => "Taylor"};"#
    ));
}

#[test]
fn dict_expression_key() {
    insta::assert_snapshot!(format(
        r#"var d = {Activity.SPORT_GENERIC * 1000 + Activity.SUB_SPORT_GENERIC => "generic"};"#
    ));
}

#[test]
fn dict_no_trailing_comma_fits_inline() {
    insta::assert_snapshot!(format(r#"var d = {"k" => 1};"#));
}

#[test]
fn dict_trailing_comma_forces_multiline() {
    insta::assert_snapshot!(format(r#"var d = {"key" => "value",};"#));
}

#[test]
fn dict_entry_inline_trailing_comment() {
    insta::assert_snapshot!(format(
        "function f() { var x = {:a => 1, :b => 2, // note\n}; }"
    ));
}

#[test]
fn dict_standalone_comment_between_entries() {
    insta::assert_snapshot!(format(
        r#"
var x = {
    :a => 1,
    // Some comment
    :b => 2,
};
"#
        .trim()
    ));
}

#[test]
fn dict_blank_line_between_entries() {
    insta::assert_snapshot!(format(
        r#"
var x = {
    :a => 1,

    :b => 2,
};
"#
        .trim()
    ));
}

#[test]
fn empty_dict_with_only_comment() {
    insta::assert_snapshot!(format("function f() { var x = {/* nothing */}; }"));
}

#[test]
fn aligned_dict_fits_stays_inline() {
    insta::assert_snapshot!(format_aligned(
        r#"var d = {:foo => "bar", :not_foo => "baz"};"#
    ));
}

#[test]
fn aligned_dict_breaks_when_wide() {
    insta::assert_snapshot!(format_aligned_width(
        r#"var d = {:foo => "bar", :not_foo => "baz"};"#,
        30
    ));
}

#[test]
fn aligned_dict_unaffected_when_flag_off() {
    insta::assert_snapshot!(format(r#"var d = {:foo => "bar", :not_foo => "baz"};"#));
}

#[test]
fn icon_dict_with_section_breaks_unaligned() {
    insta::assert_snapshot!(format(
        r#"
var icon = {
    FIELD_TYPE_SUNRISE => ">",
    // FIELD_TYPE_SUNSET => "?",

    FIELD_TYPE_HEART_RATE => "3",
    FIELD_TYPE_HR_LIVE_5S => "3",
    // FIELD_TYPE_BATTERY => "4",
    // FIELD_TYPE_BATTERY_HIDE_PERCENT => "4",
    FIELD_TYPE_NOTIFICATIONS => "5",
    FIELD_TYPE_TEMPERATURE => "<",
    // FIELD_TYPE_WEATHER => "<",
    // LIVE_HR_SPOT => "=",

    FIELD_TYPE_SUNRISE_SUNSET => "?",
    FIELD_TYPE_PULSE_OX => "B", // SG Addition
    FIELD_BODY_BATTERY => "E", // SG Addition
};
"#
        .trim()
    ));
}

#[test]
fn array_inline_and_trailing_comma() {
    insta::assert_snapshot!(format(
        r#"
var fits = [1, 2, 3];
var collapses = [1, 2, 3,];
"#
        .trim()
    ));
}

#[test]
fn array_no_trailing_comma_breaks_when_wide() {
    insta::assert_snapshot!(format_width(
        "var a = [\"a_long_string\", \"another_long_string\"];",
        30
    ));
}

#[test]
fn inline_dict_type_symbol_keys() {
    insta::assert_snapshot!(format(
        "function f(opts as { :flag as Boolean, :n as Number }) {}"
    ));
}

#[test]
fn inline_dict_type_string_keys() {
    insta::assert_snapshot!(format(
        r#"function f(o as { "name" as String, "value" as Number }) {}"#
    ));
}

#[test]
fn inline_dict_type_trailing_comma_forces_multiline() {
    insta::assert_snapshot!(format(
        "function f(o as { :a as Number, :b as String, }) {}"
    ));
}

#[test]
fn inline_dict_type_no_trailing_comma_fits_inline() {
    insta::assert_snapshot!(format("function f(o as { :a as Number, :b as String }) {}"));
}

#[test]
fn inline_dict_type_nested() {
    insta::assert_snapshot!(format(
        "function f(o as { :outer as { :inner as Number } }) {}"
    ));
}

#[test]
fn if_else() {
    insta::assert_snapshot!(format(
        "function f() { if (x > 0) { return x; } else { return 0; } }"
    ));
}

#[test]
fn else_if_chain() {
    insta::assert_snapshot!(format(
        "function f() { if (a) { return 1; } else if (b) { return 2; } else { return 3; } }"
    ));
}

#[test]
fn if_with_member_access_condition() {
    insta::assert_snapshot!(format("function f() { if (obj.flag) { return 1; } }"));
}

#[test]
fn if_short_stays_inline() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    if (x > 0) { return x; }
    if (a || b) { return 1; }
}
"#
        .trim()
    ));
}

#[test]
fn if_long_condition_wraps_on_logical_or() {
    insta::assert_snapshot!(format(
        "function fn() { if (somethingVeryLong > 1 \
             || somethingEvenLongerThisTime > 1 \
             || somethingLongSoWeshouldBreak > 1) { var x = 1; } }"
    ));
}

#[test]
fn if_long_condition_wraps_on_logical_and() {
    insta::assert_snapshot!(format(
        "function f() { if (longConditionOne > 1 \
             && longConditionTwo > 1 \
             && longConditionThree > 1) { return 1; } }"
    ));
}

#[test]
fn if_long_condition_wraps_at_comparison_op() {
    insta::assert_snapshot!(format_width(
        "function f() { if (value.length() + unit.length() <= mMaxFieldLength) { foo(); } }",
        40,
    ));
}

#[test]
fn while_short_condition() {
    insta::assert_snapshot!(format("function f() { while (x > 0) { x = x - 1; } }"));
}

#[test]
fn while_long_condition_wraps() {
    insta::assert_snapshot!(format_width(
        "function f() { while (alpha + beta + gamma <= maxFieldThreshold) { x = x - 1; } }",
        40,
    ));
}

#[test]
fn for_loop() {
    insta::assert_snapshot!(format(
        "function f() { for (var i = 0; i < 10; i++) { x = x + 1; } }"
    ));
}

#[test]
fn switch_variants() {
    insta::assert_snapshot!(format(
        r#"
function basic(x) { switch (x) { case 1: foo(); break; case 2: bar(); break; default: baz(); } }
function instance_case(p) { switch (p) { case instanceof Number: ok(); break; default: bad(); } }
function fall_through(x) { switch (x) { case 1: foo(); case 2: bar(); break; } }
"#
        .trim()
    ));
}

#[test]
fn switch_discriminant_wraps_when_long() {
    insta::assert_snapshot!(format_width(
        "function f() { switch (computeLong() + somethingElseLong) { case 1: foo(); } }",
        40,
    ));
}

#[test]
fn try_catch_variants() {
    insta::assert_snapshot!(format(
        r#"
function basic() { try { foo(); } catch (e) { log(e); } }
function typed_and_finally() {
    try { foo(); }
    catch (e instanceof MyException) { log(e); }
    catch (e) { other(e); }
    finally { cleanup(); }
}
"#
        .trim()
    ));
}

#[test]
fn annotation_with_comments() {
    insta::assert_snapshot!(format(
        r#"
(/*bar*/ :background_app /*baz*/)
class Foo {}
"#
        .trim()
    ));
}

#[test]
fn resource_ref() {
    insta::assert_snapshot!(format(
        "function f() { statusLabel.setText(@Rez.Strings.cpr_string); }"
    ));
}

#[test]
fn throw_new_exception() {
    insta::assert_snapshot!(format("function f() { throw new Lang.Exception(); }"));
}

#[test]
fn block_comment_in_function_body() {
    insta::assert_snapshot!(format("function f() { /* note */ var x = 1; }"));
}

#[test]
fn line_comment_after_binary_operand_forces_break() {
    // A `//` comment after an operand runs to the end of the line, so the
    // following operator must move to the next line rather than the usual
    // flat-mode space — otherwise it would be swallowed by the comment.
    insta::assert_snapshot!(format(
        r#"
function f() {
    if (((tm - tmStart) < tmMin()) || // we didn't rest long enough
        (val == valMin))            // we are Slowing down - not the back swing
    {
        resetInternal(true);
    }
}
"#
        .trim()
    ));
}
