//! Formatter snapshot tests.
//!
//! The snapshot body is the formatter output. For inline tests the `expression`
//! field in the snapshot header captures the input; for file-driven tests the
//! `input_file` field points at the `.mc` source.
//!
//! Snapshots live under `tests/snapshots/`. See <https://insta.rs/> for the
//! snapshot tooling.
mod common;
use common::{format, format_aligned, preserving, run};
use monkey_c_formatter::Formatter;
use monkey_c_formatter::options::{
    AnnotationSeparator, Case, ExponentPadding, LeadingZero, ModifierOrder, Nullable, Numbers,
    Parens, ResourceRef, TrailingZeros, UnionStyle,
};
use monkey_c_parser::parser::Parser;

// Configurations only the snapshot tests need. Shared helpers live in `common`, which every test
// binary compiles, so anything there must be used by all of them.

fn format_width(src: &str, width: usize) -> String {
    run(src, Formatter::new(src).with_line_width(width))
}

fn format_short_array_element_width(src: &str, element_width: usize) -> String {
    run(
        src,
        Formatter::new(src)
            .with_line_width(40)
            .with_short_array_element_width(element_width),
    )
}

/// Formats with the settings `configure` picks, and checks the result is already formatted, since a
/// rewrite that's undone or redone by the next run would make the output flip between two forms.
fn format_with(src: &str, configure: impl Fn(Formatter) -> Formatter) -> String {
    let formatted = run(src, configure(Formatter::new(src)));
    let again = run(&formatted, configure(Formatter::new(&formatted)));
    assert_eq!(formatted, again, "formatting again changed the output");

    formatted
}

fn format_aligned_width(src: &str, width: usize) -> String {
    run(
        src,
        Formatter::new(src)
            .with_alignment(true)
            .with_line_width(width),
    )
}

#[test]
fn format_inputs() {
    insta::glob!("snapshot_inputs/*.mc", |path| {
        let raw = std::fs::read_to_string(path).expect("read input file");
        let input = raw.trim();
        insta::with_settings!({ omit_expression => true }, {
            insta::assert_snapshot!(format_aligned(input));
        });
    });
}

#[test]
fn crlf_comments_do_not_keep_carriage_returns() {
    let formatted = format(
        "class A {\r\n    // line\r\n    var x = 1; // trailing\r\n    /* multi\r\n       line */\r\n}\r\n",
    );

    assert!(!formatted.contains('\r'), "{formatted:?}");
    insta::assert_snapshot!(formatted);
}

#[test]
fn comments_are_trimmed_at_end_of_line() {
    insta::assert_snapshot!(format(
        "class A {\n    // line   \n    var x = 1; // trailing \t\n    /* block   \n       inner   \n    */\n    /* kept   */\n}\n",
    ));
}

const CONSECUTIVE_TRAILING_COMMENTS: &str = r#"
function f() {
    matchplans = MatchPlan.MATCHPLANS2018; // plans
    teams = Teams.TEAMS; // teams
}
"#;

#[test]
fn trailing_comments_stay_put_without_alignment() {
    insta::assert_snapshot!(format(CONSECUTIVE_TRAILING_COMMENTS.trim()));
}

#[test]
fn trailing_comments_align_with_alignment() {
    insta::assert_snapshot!(format_aligned(CONSECUTIVE_TRAILING_COMMENTS.trim()));
}

#[test]
fn call_breaks_when_its_terminator_overflows() {
    insta::assert_snapshot!(format_width(
        r#"
function f() {
    if (true) {
        var monthformat = WatchUi.loadResource(Rez.Strings.Default_Format_Month);
        var day = monthString.substring(monthpos + 1, monthString.length()).toNumber();
        var fits = WatchUi.loadResource(Rez.Strings.Month); // trailing comments are not measured
    }
}
"#
        .trim(),
        80
    ));
}

#[test]
fn wide_characters_are_measured_by_display_width() {
    // Each line is 43 columns but 47 or more bytes, so it only fits when measured by display
    // width.
    insta::assert_snapshot!(format_width(
        r#"
var teams = {:name => "上海申花", :x => 1};
var plans = ["中超", "中超", "中超", "中"];
"#
        .trim(),
        43
    ));
}

#[test]
fn wide_characters_align_by_display_width() {
    insta::assert_snapshot!(format_aligned(
        r#"
var teams = {
    "上海" => 1,
    "SHANGHAI" => 2,
};
var name = "上海申花"; // name
var id = 1; // id
"#
        .trim()
    ));
}

#[test]
fn member_chain_breaks_one_call_per_line() {
    insta::assert_snapshot!(format_width(
        r#"
function f() {
    var min = timeString.substring(hourpos + 1, timeString.length()).toNumber();
    days = currentmoment.subtract(matchdatemoment).value().toLong().format("%d");
    var fits = a.b().c().d();
}
"#
        .trim(),
        60
    ));
}

#[test]
fn member_chain_with_one_call_breaks_its_arguments() {
    insta::assert_snapshot!(format_width(
        "function f() { var monthString = dateString.substring(yearpos + 1, dateString.length()); }",
        60
    ));
}

#[test]
fn member_chain_keeps_first_call_on_module_head() {
    insta::assert_snapshot!(format_width(
        r#"
function f() {
    View.findDrawableById("LBL_MATCH_DAY").setText(Lang.format("$1$", [year]));
    return Toybox.ActivityMonitor.getHeartRateHistory(1, true).next().heartRate.toString();
    me.getItems(filter).first().name.toUpperCase();
}
"#
        .trim(),
        60
    ));
}

#[test]
fn member_chain_keeps_comments_between_calls() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var a = foo // after head
        .bar()
        .baz();
    var b = foo
        // own line
        .bar()
        .baz();
    var c = foo.bar() // after call
        .baz();
    var d = foo /* block */ .bar().baz();
}
"#
        .trim()
    ));
}

#[test]
fn short_chain_keeps_comments_between_links() {
    insta::assert_snapshot!(format(
        r#"
function f() {
    var a = foo // one call
        .bar();
    var b = foo // no call
        .bar;
    View // module
        .findDrawableById("id")
        .setText("x");
}
"#
        .trim()
    ));
}

#[test]
fn member_chain_with_comment_in_arguments_breaks() {
    insta::assert_snapshot!(format_width(
        r#"
function f() {
    var min = timeString.substring(
        hourpos + 1, // start
        timeString.length()
    ).toNumber();
}
"#
        .trim(),
        60
    ));
}

#[test]
fn inline_block_comment_is_not_aligned_as_trailing() {
    insta::assert_snapshot!(format_aligned(
        r#"
function f() {
    var a = foo(/* inline */ 1);
    var b = 2; // trailing
    var c = foo(/* inline */ 1); // trailing
    var d = 3; // trailing
}
"#
        .trim()
    ));
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
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var required = (a + b) * c;
    var redundant = (a - b) - c;
    var nested = (((a.b) * 91 / 360) * c);
}
"#
        .trim(),
        |formatter| formatter.with_parens(Parens::Preserve),
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
fn fill_short_byte_array() {
    insta::assert_snapshot!(format_width(
        "var parameters = [0x56, 0x34, 0x12, 0x63, 0x96, 0x47, 0x71, 0x73, 0x4f, 0xbd, 0x76, 0xe3, 0xb4, 0x05, 0x19, 0xd1, 0xd9, 0x4a, 0x48]b;",
        80
    ));
}

#[test]
fn fill_trailing_comma_breaks_and_fills() {
    insta::assert_snapshot!(format("var small = [1, 2, 3,];"));
}

#[test]
fn fill_right_aligns_numbers() {
    insta::assert_snapshot!(format_width(
        "var offsets = [0, 12, 250, -3, 1024, 7, 88, -512, 16, 4096, 3, 65535, -1, 42, 900, 5, 100, -20000, 64, 8];",
        80
    ));
}

#[test]
fn fill_left_aligns_symbols() {
    insta::assert_snapshot!(format_width(
        "var keys = [:alpha, :beta, :gamma, :delta, :epsilon, :zeta, :eta, :theta, :iota, :kappa, :lambda, :mu];",
        80
    ));
}

#[test]
fn fill_left_aligns_mixed_numbers_and_symbols() {
    insta::assert_snapshot!(format_width(
        "var mixed = [1, :two, 300, :four, 5, :six, 7000, :eight, 9, :ten, 11];",
        40
    ));
}

#[test]
fn fill_measures_from_the_indent_it_is_nested_in() {
    insta::assert_snapshot!(format_width(
        r#"
class A {
    function f() {
        var bytes = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a];
    }
}
"#
        .trim(),
        40
    ));
}

#[test]
fn fill_as_a_sole_call_argument() {
    insta::assert_snapshot!(format_width(
        "function f() { send([0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a, 0x0b, 0x0c]); }",
        40
    ));
}

#[test]
fn fill_skips_arrays_with_a_long_item() {
    insta::assert_snapshot!(format_width(
        "var names = [:a, :b, :c, :d, :e, :thisOneIsFarTooLong, :f, :g, :h];",
        40
    ));
}

#[test]
fn fill_skips_arrays_with_a_block_comment() {
    insta::assert_snapshot!(format_width(
        "var bytes = [0x01, 0x02, /* magic */ 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09];",
        40
    ));
}

#[test]
fn fill_skips_arrays_with_a_line_comment() {
    insta::assert_snapshot!(format(
        r#"
var table = [
    0x00, 0x01, 0x02, // header
    0x10, 0x11
];
"#
        .trim()
    ));
}

#[test]
fn fill_turned_off_keeps_one_item_per_line() {
    insta::assert_snapshot!(format_short_array_element_width(
        "var bytes = [0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0a];",
        0
    ));
}

#[test]
fn fill_follows_the_configured_element_width() {
    insta::assert_snapshot!(format_short_array_element_width(
        r#"
var short = [:alpha, :beta, :gamma, :delta, :theta, :zeta, :eta, :iota];
var long = [:alpha, :beta, :gamma, :delta, :theta, :zeta, :eta, :epsilon];
"#
        .trim(),
        7
    ));
}

#[test]
fn fill_right_aligns_hex_numbers() {
    insta::assert_snapshot!(format_width(
        "var masks = [0x1, 0xff, 0x10, 0xfff, 0x2, 0xf0, 0x100, 0x3, 0x4, 0xabc];",
        30
    ));
}

#[test]
fn fill_right_aligns_with_a_trailing_comma() {
    insta::assert_snapshot!(format_width("var z = [1, 22, 333, 4,];", 12));
}

#[test]
fn fill_right_aligns_a_wide_last_number() {
    insta::assert_snapshot!(format_width("var w = [1, 2, 3, 100000];", 14));
}

#[test]
fn fill_never_exceeds_the_line_width() {
    insta::assert_snapshot!(format_width("var w = [1, 2, 3, 100000];", 13));
}

#[test]
fn fill_left_aligns_a_last_symbol_without_extra_padding() {
    insta::assert_snapshot!(format_width("var s = [:a, :b, :cccc];", 15));
}

#[test]
fn fill_breaks_a_line_that_only_fits_without_spaces() {
    insta::assert_snapshot!(format_width(
        "var z = [10, 2, 30, 4, 50, 6, 70, 8, 90, 1, 11, 2, 13, 4, 15, 6, 17, 8, 19, 2, 21, 2, 23, 4, 25, 6, 27, 8, 29, 3];",
        111
    ));
}

#[test]
fn wrapped_declaration_stays_flat_when_it_fits() {
    insta::assert_snapshot!(format_width("var a = 1, b = 2;", 17));
}

#[test]
fn wrapped_declaration_breaks_when_its_semicolon_overflows() {
    insta::assert_snapshot!(format_width("var a = 1, b = 2;", 16));
}

#[test]
fn wrapped_declaration_measures_from_its_indentation() {
    insta::assert_snapshot!(format_width(
        "class Foo { const A = 1, B = 2; function f() { var i, j, k = 0; } }",
        23
    ));
}

#[test]
fn wrapped_declaration_keeps_comment_before_semicolon() {
    insta::assert_snapshot!(format_width(
        r#"
var a = 1, b = 2 /* fits */;
var firstLongBindingName = "some value", secondLongBindingName = "another value" /* breaks */;
"#
        .trim(),
        60
    ));
}

const SOLE_ARRAY_ARGUMENT: &str = r#"
function fn() {
    someFn(["foo", "bar", "baz"]);
    var x = new Foo(["foo", "bar", "baz"]);
}
"#;

#[test]
fn sole_array_argument_hugs_brackets() {
    insta::assert_snapshot!(format_width(SOLE_ARRAY_ARGUMENT.trim(), 20));
}

#[test]
fn hugged_array_stays_flat_when_it_fits() {
    insta::assert_snapshot!(format_width("function fn() { someFn([1, 2, 3]); }", 111));
}

#[test]
fn sole_dict_argument_hugs_brackets() {
    insta::assert_snapshot!(format_width(
        r#"
function fn() {
    someFn({ "foo" => "bar", "baz" => "qux" });
    someFn({
        "foo" => "bar",
    });
    var x = new Foo({ :foo => "bar", :baz => "qux" });
}
"#
        .trim(),
        20
    ));
}

#[test]
fn hug_brackets_only_applies_to_a_sole_collection_argument() {
    insta::assert_snapshot!(format_width(
        r#"
function fn() {
    someFn(["foo", "bar", "baz"], 1);
    someFn({ :foo => "bar", :baz => "qux" }, 1);
}
"#
        .trim(),
        20
    ));
}

#[test]
fn hugged_array_keeps_its_own_trailing_comma_and_comments() {
    insta::assert_snapshot!(format_width(
        r#"
function fn() {
    someFn([
        "foo", // first
        "bar",
    ]);
}
"#
        .trim(),
        111
    ));
}

#[test]
fn hug_brackets_yields_to_comments_around_the_array() {
    insta::assert_snapshot!(format_width(
        r#"
function fn() {
    someFn( // leading
        ["foo", "bar", "baz"]
    );
    someFn(
        ["foo", "bar", "baz"] // trailing
    );
    someFn(["foo", "bar", "baz"],);
}
"#
        .trim(),
        111
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
fn empty_annotation() {
    insta::assert_snapshot!(format(
        r#"
()
function fn1() {}

( /* Nothing */ )
function fn2() {}
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
fn resource_ref_preserved() {
    insta::assert_snapshot!(format_with(
        "function f() { statusLabel.setText(@Rez.Strings.cpr_string); }",
        |formatter| formatter.with_resource_ref(ResourceRef::Preserve),
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
    // A `//` comment after an operand runs to the end of the line. The
    // operator that follows it in source (`|| // comment`) stays glued to
    // this line, before the comment, and the next operand starts a fresh
    // line without a leading operator — so the comment isn't repositioned
    // relative to `||`, matching rustfmt's behavior.
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

#[test]
fn line_comment_trailing_whole_condition_moves_paren() {
    // A `//` comment trailing the whole condition runs to the end of the
    // line, so `)` must render before it. A standalone comment between that
    // and `{` must not be dropped either.
    insta::assert_snapshot!(format(
        r#"
function f() {
    if (a || // c1
        (b > 0 || c < -1) // c2
        // c3
        ) {
        foo();
    }
}
"#
        .trim()
    ));
}

#[test]
fn double_literal_with_extreme_negative_exponent() {
    // Re-emitting via `value / 10_f64.powi(-311)` underflowed to `inf`, since
    // `powi` rounds such an extreme power of ten to `0`. `digits` now
    // preserves the source digits verbatim, so this round-trips exactly.
    insta::assert_snapshot!(format("var x = 1.16183573193898954669148418496e-311d;"));
}

#[test]
fn line_comment_after_middle_operand_in_chain() {
    // A `//` comment after the middle operand of a 3+ operand chain attaches
    // to the flattened-away `(a || b)` node — same end offset as `b`, so
    // `attach_comments` prefers it over `b`'s own span. Regression test for
    // that comment being silently dropped.
    insta::assert_snapshot!(format(
        r#"
function f() {
    if (a || b || // c1
        c) {
        foo();
    }
}
"#
        .trim()
    ));
}

const UNIONS: &str = r#"
typedef Pipe as Number | Float;
typedef Mixed as String or Number | Float;
typedef Tight as Number|Float;
var nested as Array<Number | String> | Boolean;
var shapes as { :a as Number | Float } | [Number | String] | Method(b as Long | Float);

function f(value as Number or String) {
    var cast = value as Number | String;
    var grouped = value as Number or (Method() as Boolean);
    var bitwise = 0x00 as Number | 0xFF;
}
"#;

#[test]
fn union_style_or() {
    insta::assert_snapshot!(format_with(UNIONS.trim(), |formatter| formatter));
}

#[test]
fn union_style_pipe() {
    // A cast only reads `|` as a union when a type name follows, so the grouped member keeps `or`.
    insta::assert_snapshot!(format_with(UNIONS.trim(), |formatter| formatter
        .with_union_style(UnionStyle::Pipe),));
}

#[test]
fn union_style_preserved() {
    insta::assert_snapshot!(format_with(UNIONS.trim(), |formatter| formatter
        .with_union_style(UnionStyle::Preserve),));
}

const NULLABLES: &str = r#"
typedef A as Number or Null;
typedef B as Null or Number;
typedef C as Number | Toybox.Lang.Null;
typedef D as Number? or Null;
typedef E as Dictionary<String, Number> or Null;
typedef F as Array<Number or Null>;
typedef G as { :flag as Boolean } or Null;
typedef H as [Number, String] or Null;
typedef I as Method(x as Number) or Null;
typedef J as (Method() as Boolean) or Null;
typedef K as Null or Method() as Boolean;
typedef L as String or Number or Null;
typedef M as Number? or String;
typedef N as Number? | Null;
typedef O as Null;
typedef P as Number or /* why */ Null;
typedef Q as (Method() as Boolean)?;
typedef R as Method() as Boolean?;

function f(x as Number?) as String? {
    var a = y as Number or Null;
    var b = c ? y as Number or Null : z;
    var d = y as Number or Null == null;
    var e = y as Number?;
}
"#;

#[test]
fn nullable_shorthand() {
    // `K` has no shorthand without parentheses and `P` keeps its comment where it was written.
    insta::assert_snapshot!(format_with(NULLABLES.trim(), |formatter| formatter));
}

#[test]
fn nullable_union() {
    insta::assert_snapshot!(format_with(NULLABLES.trim(), |formatter| formatter
        .with_nullable(Nullable::Union),));
}

#[test]
fn nullable_union_with_pipes() {
    insta::assert_snapshot!(format_with(
        "function f(x as Number?) { var y = x as String?; }",
        |formatter| formatter
            .with_nullable(Nullable::Union)
            .with_union_style(UnionStyle::Pipe),
    ));
}

#[test]
fn nullable_preserved() {
    insta::assert_snapshot!(format_with(NULLABLES.trim(), |formatter| formatter
        .with_nullable(Nullable::Preserve),));
}

#[test]
fn nullable_shorthand_in_cast_before_an_expression() {
    // `+` could start an expression after a `?`, which would make it a ternary.
    insta::assert_snapshot!(format_with(
        "function f() { var x = y as Number or Null + 1; }",
        |formatter| formatter,
    ));
}

const ANNOTATIONS: &str = r#"
(:first, :second :third,   :fourth)
function a() {}

(:foo(1, 2), :bar(:sym))
function b() {}

(:first, /* second */ :second)
function c() {}

(:first, // second
    :second)
function d() {}
"#;

#[test]
fn annotation_separator_space() {
    insta::assert_snapshot!(format_with(ANNOTATIONS.trim(), |formatter| formatter));
}

#[test]
fn annotation_separator_comma() {
    insta::assert_snapshot!(format_with(ANNOTATIONS.trim(), |formatter| formatter
        .with_annotation_separator(AnnotationSeparator::Comma),));
}

#[test]
fn annotation_separator_preserved() {
    insta::assert_snapshot!(format_with(ANNOTATIONS.trim(), |formatter| formatter
        .with_annotation_separator(AnnotationSeparator::Preserve),));
}

const MODIFIERS: &str = r#"
static public module M {
    static private const MAX = 10;
    static public function create() as Void {}
    static
        hidden var count = 0;
    static // why
        protected var other = 0;
    private static var kept = 0;
}
"#;

#[test]
fn modifier_order_visibility_first() {
    insta::assert_snapshot!(format_with(MODIFIERS.trim(), |formatter| formatter));
}

#[test]
fn modifier_order_preserved() {
    insta::assert_snapshot!(format_with(MODIFIERS.trim(), |formatter| formatter
        .with_modifier_order(ModifierOrder::Preserve),));
}

#[test]
fn parens_dropped() {
    insta::assert_snapshot!(format_with(
        r#"
const X = (42);
var initializer = (((1 + 2)));

function f() {
    x = (1 + 2);
    x = (y = 1);
    var required = (a + b) * c;
    var leftAssociative = ((1 + 2) - 3) + 4;
    var rightAssociative = a - (b + c);
    var tighter = a + (b * c);
    var negated = a-(-b*c);
    var member = (x + 1).foo;
    var doubled = a - ((b + c));
    var commented = (1 + 2 /* sum */);
    var leading = (/* width */ 2 + margin);
    var castBefore = a as Number | (b & c);
    var castInside = (a | b as Number) | c;
    if (failed) {
        throw (error);
    }
    return (foo());
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// Commas, brackets, `?` and `:` already end the expression in each of these positions, so the
/// parens around it can't change how it reads.
#[test]
fn parens_dropped_in_delimited_positions() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var argument = Math.floor((no / 2), (y));
    var index = values[(i + 1)];
    var element = [(a + b), (c)];
    var entries = {(key) => (a + b)};
    var size = new [(count + 1)];
    var created = new Foo((a + b));
    var condition = (i == 1) ? 4 : 4 + no;
    var branches = ready ? (a + 1) : (b - 1);
    var chained = ready ? a : (other ? b : c);
    var simple = xx + (width) / (foo()) * (values[0]);
    var negated = !(ready) && -(count) < 0;
    var receiver = (Math.floor(x)).toNumber() + (foo).bar[(i)] + (values[0]).abs();
    if ((ready)) {
        x = 1;
    }

    while (((busy))) {
        x = 2;
    }
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// Regrouping the same associative operator on the right gives the same result, while `-`, `+`
/// and `*` can't regroup, since `"x" + (1 + 2)` is `"x3"` and floats round differently.
#[test]
fn parens_dropped_for_associative_operators() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var orChain = a || (b || c);
    var andChain = a && (b and c);
    var bits = (a | (b | c)) & (d & e) ^ (g ^ h);
    var mixedLevel = a + (b | c);
    var andOfOr = a && (b || c);
    var minus = a - (b - c);
    var plus = "x" + (1 + 2);
    var times = a * (b * c);
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// Parens stay where dropping them would read differently or let a cast's type run on.
#[test]
fn parens_kept_where_they_matter() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var doubleNegation = -(-x);
    var nestedThen = ready ? (a ? b : c) : d;
    var nestedCondition = (a ? b : c) ? d : e;
    var castCondition = (x as Boolean) ? 1 : 2;
    var castElse = ready ? 1 : (x as Number);
    var castOperand = (x as Number) + 1;
    var member = (360).toFloat() + (a + b).abs() + (new Foo()).bar();
    var commented = values[(i /* next */ + 1)];
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// `&&` used to group from the right, so dropping the first parens here read as a different tree.
#[test]
fn parens_dropped_in_an_and_chain() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    if ((a <= 7.0 && a >= 4.5) && (b >= 0.0 && b <= 1.5)) {
        x = true;
    }
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// A `(` right after a cast and `|` keeps the cast's type from reading on, however deep in the
/// operand it is.
#[test]
fn parens_kept_after_a_cast_at_any_depth() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var masked = b as Number | (hi << 8) & mask;
    var scaled = x as Number | (a * b) / c;
    var nested = x as Number | (a * b) * c & d;
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

/// Monkey C groups the bitwise and shift operators with the arithmetic ones, unlike C, so parens
/// that would be redundant in C can be needed here and the other way around.
#[test]
fn parens_follow_monkey_c_precedence() {
    insta::assert_snapshot!(format_with(
        r#"
function f() {
    var shiftSum = (a + b) << c;
    var sumShift = a + (b << c);
    var maskEquals = (a & b) == c;
    var maskOfEquals = a & (b == c);
    var orThenAdd = (a | b) + c;
    var addOr = a + (b | c);
    var equalsLess = (a == b) < c;
    var lessOfEquals = a == (b < c);
    var instanceAnd = (a instanceof B) & c;
    var sumInstance = (a + b) instanceof C;
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

#[test]
fn type_parens_dropped() {
    insta::assert_snapshot!(format_with(
        r#"
var a as (Method(x as Number))?;
var b as (Method() as Boolean)?;
var c as (Method() as Boolean) or Number;
var d as Number or (Method() as Boolean) or String;
var e as Array<(Method() as Boolean)>;
var f as (Method(x as Number)) or Number;

function g() as (Method() as Boolean) {
    var h = x as (Method() as Boolean);
}
"#
        .trim(),
        |formatter| formatter,
    ));
}

const IMPORTS: &str = r#"
using Toybox.WatchUi;
import ModuleB;

using ModuleA;
import Toybox.Lang;
using Toybox.Graphics as Gfx;

var x;

// leading
using Toybox.B;
using Toybox.A; // trailing

module M {
    using Toybox.D;
    using Toybox.C;
    // between
    using Toybox.B;
    using Toybox.A;
}

module N {
    using Toybox.B; // after B
    using Toybox.A;
}
"#;

#[test]
fn imports_sorted() {
    // A comment between two imports could be about either, so that run stays as written.
    insta::assert_snapshot!(format_with(IMPORTS.trim(), |formatter| formatter));
}

#[test]
fn imports_kept_in_order() {
    insta::assert_snapshot!(format_with(IMPORTS.trim(), |formatter| formatter
        .with_sort_imports(false),));
}

#[test]
fn every_rewrite_preserved() {
    insta::assert_snapshot!(format_with(
        r#"
using Toybox.B;
using Toybox.A;

(:first, :second)
static private var x as Number | Null = (1);

function f() as (Method() as Boolean) {
    return @Rez.Strings.name;
}
"#
        .trim(),
        preserving,
    ));
}

#[test]
fn imports_with_a_trailing_comment_stay_in_order() {
    // The comment is about `Toybox.Lang`, so sorting would leave it on `Toybox.WatchUi`.
    insta::assert_snapshot!(format_with(
        "import Toybox.WatchUi;\nimport Toybox.Lang; // needed for Number\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_with_a_comment_right_above_stay_in_order() {
    insta::assert_snapshot!(format_with(
        "// for the menu\nimport Toybox.WatchUi;\nimport Toybox.Lang;\n\nvar x;\n\n// the header\n\nimport Bee;\nimport Ant;\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_sorted_below_a_commented_declaration() {
    insta::assert_snapshot!(format_with(
        "// about x\nvar x = 1;\nimport Bee;\nimport Ant;\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_with_a_directive_between_stay_in_order() {
    // Sorting would leave the directive inside an import.
    insta::assert_snapshot!(format_with(
        "import Toybox.WatchUi;\n// rafiki: fmt on\nimport Toybox.Lang;\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_sorted_after_an_annotated_import() {
    insta::assert_snapshot!(format_with(
        "(:test)\nimport Bee;\nimport Cat;\nimport Ant;\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_after_a_fmt_off_region_are_sorted() {
    insta::assert_snapshot!(format_with(
        "// rafiki: fmt off\nimport  Zed;\n// rafiki: fmt on\nimport Bee;\nimport Ant;\n",
        |formatter| formatter,
    ));
}

#[test]
fn modifiers_with_a_comment_between_stay_in_order() {
    insta::assert_snapshot!(format_with(
        "class C {\n    static /* why */ private var x = 1;\n}\n",
        |formatter| formatter,
    ));
}

#[test]
fn annotated_imports_stay_in_order() {
    // The annotation is written for `Z`, so sorting would move it onto `A`.
    insta::assert_snapshot!(format_with(
        "(:debug)\nimport Z;\nimport A;\n",
        |formatter| formatter,
    ));
}

#[test]
fn imports_around_a_fmt_off_region_are_sorted_apart() {
    insta::assert_snapshot!(format_with(
        "import Zed;\nimport Yak;\n// rafiki: fmt off\nimport  Kept;\n// rafiki: fmt on\nimport Bee;\nimport Ant;\n",
        |formatter| formatter,
    ));
}

#[test]
fn nullable_union_writes_one_null() {
    insta::assert_snapshot!(format_with(
        "var x as Number? or String?;\nvar y as Number? or String? or Null;\n",
        |formatter| formatter.with_nullable(Nullable::Union),
    ));
}

#[test]
fn parens_keep_a_comment_an_earlier_statement_takes() {
    // The first statement's trailing drain takes the comment inside the `return`'s parens, which
    // moves it even with every rewrite off, so this can't go through `run`. The parens still count
    // the comment as theirs and stay.
    let src = "function f() { x = (a) /* c */; return (/* c */ a); }";
    let output = Parser::new(src).parse().expect("should parse");
    let formatted = Formatter::new(src).format(&output);

    assert!(formatted.contains("return (a);"), "{formatted}");
}

#[test]
fn nullable_shorthand_in_cast_looks_past_whitespace_and_comments() {
    insta::assert_snapshot!(format_with(
        "function f() {\n    var a = y as Number or Null == null;\n    var b = y as Number or Null /* c */ - 1;\n}\n",
        |formatter| formatter,
    ));
}

const NUMBERS: &str = r#"
var number = 007;
var hex = [0xff, 0XAbC, 0xffl, 0X10L];
var long = [10l, 10L, .50l, 1E+05l];
var float = [.5, 1.50, 1.0, 2F, 1E5, 2.5e+07, 1e-007, .50E+00f];
var double = [.5d, 1.50D, 6371e03d, 10.000d];
"#;

#[test]
fn numbers_normalized() {
    insta::assert_snapshot!(format_with(NUMBERS.trim(), |formatter| formatter));
}

#[test]
fn numbers_preserved() {
    insta::assert_snapshot!(format_with(NUMBERS.trim(), |formatter| formatter
        .with_numbers(Numbers::preserve())));
}

#[test]
fn numbers_in_the_other_case_with_trailing_zeros_trimmed() {
    insta::assert_snapshot!(format_with(NUMBERS.trim(), |formatter| formatter
        .with_numbers(Numbers {
            hex_prefix_case: Case::Upper,
            hex_digit_case: Case::Lower,
            exponent_case: Case::Upper,
            exponent_padding: ExponentPadding::Preserve,
            float_suffix_case: Case::Upper,
            double_suffix_case: Case::Upper,
            long_suffix_case: Case::Lower,
            leading_zero: LeadingZero::Preserve,
            trailing_zeros: TrailingZeros::Trim,
        })));
}

#[test]
fn numbers_align_by_their_rewritten_width() {
    // `.5` grows to `0.5`, so the arrows are aligned to the key as it's written out.
    insta::assert_snapshot!(format_aligned(
        "var d = {\n    .5 => :half,\n    1.0 => :one,\n    0xffl => :mask,\n};"
    ));
}

#[test]
fn leading_dot_long() {
    // The lexer used to panic on a `.` followed by digits that didn't end up a float or double.
    insta::assert_snapshot!(format("var c = .5l;"));
}
