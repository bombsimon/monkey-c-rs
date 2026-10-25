use monkey_c_formatter::doc::{Alignment, Doc, render};

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
fn test_soft_line_empty_when_flat() {
    let doc = Doc::group(vec![Doc::text("a"), Doc::soft_line(), Doc::text("b")]);
    assert_eq!(render(&doc, 80), "ab");
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
    assert_eq!(
        render(
            &Doc::concat(vec![Doc::text("a"), Doc::blank_line(), Doc::text("b")]),
            80
        ),
        "a\n\nb"
    );
}

#[test]
fn test_group_breaks_past_the_width() {
    let doc = Doc::group(vec![
        Doc::text("["),
        Doc::Indent(vec![Doc::soft_line(), Doc::text("12345678")]),
        Doc::soft_line(),
        Doc::text("]"),
    ]);
    assert_eq!(render(&doc, 10), "[12345678]");
    assert_eq!(render(&doc, 9), "[\n    12345678\n]");
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

#[test]
fn test_concat_of_nothing_is_empty() {
    assert!(matches!(Doc::concat(vec![]), Doc::Empty));
    assert!(matches!(
        Doc::concat(vec![Doc::Empty, Doc::concat(vec![Doc::Empty])]),
        Doc::Empty
    ));
    assert!(matches!(
        Doc::concat(vec![Doc::Empty, Doc::text("a")]),
        Doc::Concat(_)
    ));
}

/// Cells as the array builder makes them, each item with its comma already attached.
fn cells(items: &[&str]) -> Vec<Doc> {
    items.iter().map(|item| Doc::text(*item)).collect()
}

/// A fill that must break, like an array with a trailing comma.
fn broken_fill(items: &[&str], alignment: Alignment) -> Doc {
    Doc::concat(vec![
        Doc::text("["),
        Doc::Indent(vec![Doc::hard_line(), Doc::Fill(cells(items), alignment)]),
        Doc::hard_line(),
        Doc::text("]"),
    ])
}

/// A fill that stays on one line when it fits, like an array without a trailing comma.
fn grouped_fill(items: &[&str], alignment: Alignment) -> Doc {
    Doc::group(vec![
        Doc::text("["),
        Doc::Indent(vec![Doc::soft_line(), Doc::Fill(cells(items), alignment)]),
        Doc::soft_line(),
        Doc::text("]"),
    ])
}

#[test]
fn fill_left_aligns_cells_into_columns() {
    let items = [
        ":fo1,", ":fooo2,", ":foo3,", ":foo4,", ":foo5,", ":fo6,", ":fo7",
    ];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 30),
        "[\n    :fo1,  :fooo2, :foo3,\n    :foo4, :foo5,  :fo6,\n    :fo7\n]",
    );
}

#[test]
fn fill_with_a_full_last_row_adds_no_blank_line() {
    let items = [":a,", ":b,", ":c,", ":d,", ":e,", ":f"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 15),
        "[\n    :a, :b, :c,\n    :d, :e, :f\n]",
    );
}

#[test]
fn fill_uses_the_whole_width_including_the_last_column() {
    // Three columns need exactly 22 columns after the indent.
    let items = [":fo1,", ":foooo2,", ":foo3,", ":foo4,", ":foo5,", ":fo6"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 26),
        "[\n    :fo1,  :foooo2, :foo3,\n    :foo4, :foo5,   :fo6\n]",
    );
    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 25),
        "[\n    :fo1,  :foooo2,\n    :foo3, :foo4,\n    :foo5, :fo6\n]",
    );
}

#[test]
fn fill_keeps_a_trailing_comma_on_the_last_cell() {
    let items = ["1,", "2,", "3,"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 80),
        "[\n    1, 2, 3,\n]",
    );
}

#[test]
fn fill_right_aligns_numbers_by_their_last_digit() {
    // The last cell has no comma, so it lines up with the digits above it and not the commas.
    let items = ["1,", "22,", "333,", "4"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 12),
        "[\n      1, 22,\n    333,  4\n]",
    );
}

#[test]
fn fill_right_aligns_negative_numbers() {
    let items = ["-1,", "200,", "-3000,", "4,", "50,", "-6"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 20),
        "[\n    -1, 200, -3000,\n     4,  50,    -6\n]",
    );
}

#[test]
fn fill_that_cannot_fit_two_columns_is_one_cell_per_line() {
    let items = [":abcdef,", ":ghijkl,", ":mn"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 10),
        "[\n    :abcdef,\n    :ghijkl,\n    :mn\n]",
    );
}

#[test]
fn fill_that_cannot_fit_two_columns_does_not_pad_numbers() {
    let items = ["1,", "100000"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 8),
        "[\n    1,\n    100000\n]",
    );
}

#[test]
fn fill_stays_on_one_line_when_it_fits() {
    let items = [":a,", ":b,", ":c"];

    assert_eq!(
        render(&grouped_fill(&items, Alignment::Left), 80),
        "[:a, :b, :c]",
    );
}

#[test]
fn fill_breaks_when_one_line_is_too_wide() {
    let items = [":aaaa,", ":bbbb,", ":cccc,", ":dddd"];

    assert_eq!(
        render(&grouped_fill(&items, Alignment::Left), 20),
        "[\n    :aaaa, :bbbb,\n    :cccc, :dddd\n]",
    );
}

#[test]
fn fill_on_one_line_counts_towards_what_follows() {
    // `[:a, :b] = abc` is 14 wide, so the second group has to break at width 12.
    let doc = Doc::concat(vec![
        grouped_fill(&[":a,", ":b"], Alignment::Left),
        Doc::group(vec![Doc::text(" ="), Doc::line(), Doc::text("abc")]),
    ]);

    assert_eq!(render(&doc, 12), "[:a, :b] =\nabc");
}

#[test]
fn fill_pads_wide_characters_by_display_width() {
    // `"日本",` is 5 characters but 7 columns on screen, so it needs 2 spaces of padding, not 4.
    let items = ["\"日本\",", "\"ab\",", "\"abcdef\",", "\"cd\""];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 19),
        "[\n    \"日本\",   \"ab\",\n    \"abcdef\", \"cd\"\n]",
    );
}

#[test]
fn fill_right_aligns_a_last_cell_with_a_trailing_comma() {
    let items = ["1,", "22,", "333,", "4,"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 12),
        "[\n      1, 22,\n    333,  4,\n]",
    );
}

#[test]
fn fill_widens_a_column_for_a_last_cell_without_a_comma() {
    let items = ["1,", "2,", "3,", "100000"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 14),
        "[\n    1,      2,\n    3, 100000\n]",
    );
}

#[test]
fn fill_counts_the_widened_column_when_choosing_columns() {
    // Two columns need 10 once `100000` lines up with the commas, one more than is left.
    let items = ["1,", "2,", "3,", "100000"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Right), 13),
        "[\n    1,\n    2,\n    3,\n    100000\n]",
    );
}

#[test]
fn fill_left_aligned_does_not_widen_for_a_missing_comma() {
    let items = [":a,", ":b,", ":cccc"];

    assert_eq!(
        render(&broken_fill(&items, Alignment::Left), 15),
        "[\n    :a,   :b,\n    :cccc\n]",
    );
}

#[test]
fn fill_counts_the_spaces_between_cells_when_deciding_to_stay_flat() {
    // `var z = [:a, :b];` is 17 wide, but only 16 when the space between the cells is forgotten.
    let doc = Doc::concat(vec![
        Doc::text("var z = "),
        grouped_fill(&[":a,", ":b"], Alignment::Left),
        Doc::text(";"),
    ]);

    assert_eq!(render(&doc, 17), "var z = [:a, :b];");
    assert_eq!(render(&doc, 16), "var z = [\n    :a, :b\n];");
}
