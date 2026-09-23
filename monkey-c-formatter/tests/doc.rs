use monkey_c_formatter::doc::{Doc, render};

#[test]
fn test_text() {
    assert_eq!(render(&Doc::text("hello"), 80), "hello");
}

#[test]
fn test_concat() {
    let doc = Doc::concat(vec![Doc::text("foo"), Doc::text("bar")]);
    assert_eq!(render(&doc, 80), "foobar");
}

#[test]
fn test_hard_line() {
    let doc = Doc::concat(vec![Doc::text("a"), Doc::hard_line(), Doc::text("b")]);
    assert_eq!(render(&doc, 80), "a\nb");
}

#[test]
fn test_hard_line_with_indent() {
    let doc = Doc::concat(vec![
        Doc::text("{"),
        Doc::Indent(vec![Doc::hard_line(), Doc::text("body")]),
        Doc::hard_line(),
        Doc::text("}"),
    ]);
    assert_eq!(render(&doc, 80), "{\n    body\n}");
}

#[test]
fn test_group_fits_flat() {
    let doc = Doc::group(vec![
        Doc::text("{"),
        Doc::Indent(vec![Doc::line(), Doc::text("x: 1")]),
        Doc::line(),
        Doc::text("}"),
    ]);
    assert_eq!(render(&doc, 80), "{ x: 1 }");
}

#[test]
fn test_group_breaks_when_too_wide() {
    let doc = Doc::group(vec![
        Doc::text("{"),
        Doc::Indent(vec![
            Doc::line(),
            Doc::text("a_very_long_key: a_very_long_value"),
        ]),
        Doc::line(),
        Doc::text("}"),
    ]);
    let result = render(&doc, 20);
    assert!(result.contains('\n'));
}

#[test]
fn test_soft_line_empty_when_flat() {
    let doc = Doc::group(vec![Doc::text("a"), Doc::soft_line(), Doc::text("b")]);
    assert_eq!(render(&doc, 80), "ab");
}

#[test]
fn test_soft_line_breaks_when_broken() {
    // Force break by making the content too wide
    let wide = "a".repeat(60);
    let doc = Doc::group(vec![Doc::text(&wide), Doc::soft_line(), Doc::text(&wide)]);
    let result = render(&doc, 80);
    assert!(result.contains('\n'));
}

#[test]
fn test_blank_line() {
    let doc = Doc::concat(vec![Doc::text("a"), Doc::blank_line(), Doc::text("b")]);
    assert_eq!(render(&doc, 80), "a\n\nb");
}

#[test]
fn test_nested_indent() {
    let doc = Doc::concat(vec![
        Doc::text("outer"),
        Doc::Indent(vec![
            Doc::hard_line(),
            Doc::text("inner"),
            Doc::Indent(vec![Doc::hard_line(), Doc::text("deeper")]),
        ]),
        Doc::hard_line(),
        Doc::text("end"),
    ]);
    assert_eq!(render(&doc, 80), "outer\n    inner\n        deeper\nend");
}

#[test]
fn test_empty() {
    assert_eq!(render(&Doc::Empty, 80), "");
}

#[test]
fn test_group_width_boundary_fits() {
    // Content is exactly 10 chars, width is 10 — should fit flat
    let doc = Doc::group(vec![
        Doc::text("["),
        Doc::Indent(vec![Doc::soft_line(), Doc::text("12345678")]),
        Doc::soft_line(),
        Doc::text("]"),
    ]);
    assert_eq!(render(&doc, 10), "[12345678]");
}

#[test]
fn test_group_width_boundary_breaks() {
    // Content would be 11 chars, width is 10 — should break
    let doc = Doc::group(vec![
        Doc::text("["),
        Doc::Indent(vec![Doc::soft_line(), Doc::text("123456789")]),
        Doc::soft_line(),
        Doc::text("]"),
    ]);
    let result = render(&doc, 10);
    assert!(result.contains('\n'));
}

fn call_args(args: &str) -> Doc {
    Doc::group(vec![
        Doc::text("f("),
        Doc::Indent(vec![Doc::soft_line(), Doc::text(args)]),
        Doc::soft_line(),
        Doc::text(")"),
    ])
}

#[test]
fn test_group_breaks_when_following_text_overflows() {
    // The group alone is 10 columns, but the `;` after it pushes the line to 11.
    let doc = Doc::concat(vec![call_args("12345678"), Doc::text(";")]);
    assert_eq!(render(&doc, 10), "f(\n    12345678\n);");
}

#[test]
fn test_group_measures_only_up_to_the_next_line_break() {
    let doc = Doc::concat(vec![
        call_args("12345678"),
        Doc::text(";"),
        Doc::hard_line(),
        Doc::text("a line after the break that is far too long to fit"),
    ]);
    assert_eq!(
        render(&doc, 12),
        "f(12345678);\na line after the break that is far too long to fit"
    );
}

#[test]
fn test_line_comment_does_not_count_towards_fit() {
    let doc = Doc::concat(vec![
        call_args("12345678"),
        Doc::text(";"),
        Doc::line_comment(" // a trailing comment"),
    ]);
    assert_eq!(render(&doc, 12), "f(12345678); // a trailing comment");
}

#[test]
fn test_wide_characters_count_as_two_columns() {
    // "上海" is 6 bytes but 4 columns, so `f("上海")` is 9 columns wide rather than 11.
    let doc = call_args("\"上海\"");
    assert_eq!(render(&doc, 9), "f(\"上海\")");
    assert_eq!(render(&doc, 8), "f(\n    \"上海\"\n)");
}
