use monkey_c_formatter::doc::{render, Doc};

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
