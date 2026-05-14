use monkey_c_parser::line_index::{LineCol, LineIndex};

#[test]
fn test_single_line() {
    let idx = LineIndex::new("hello world");
    assert_eq!(idx.line_col(0), LineCol { line: 0, col: 0 });
    assert_eq!(idx.line_col(6), LineCol { line: 0, col: 6 });
}

#[test]
fn test_multi_line() {
    let idx = LineIndex::new("hello\nworld\n");
    assert_eq!(idx.line_col(0), LineCol { line: 0, col: 0 });
    assert_eq!(idx.line_col(5), LineCol { line: 0, col: 5 }); // the \n itself
    assert_eq!(idx.line_col(6), LineCol { line: 1, col: 0 }); // 'w' of "world"
    assert_eq!(idx.line_col(8), LineCol { line: 1, col: 2 });
}

#[test]
fn test_blank_lines_between() {
    // "a\n\nb" — one blank line between a and b
    let idx = LineIndex::new("a\n\nb");
    assert_eq!(idx.blank_lines_between(1, 3), 1);
}

#[test]
fn test_no_blank_lines() {
    let idx = LineIndex::new("a\nb");
    assert_eq!(idx.blank_lines_between(1, 2), 0);
}

#[test]
fn test_two_blank_lines() {
    let idx = LineIndex::new("a\n\n\nb");
    assert_eq!(idx.blank_lines_between(1, 4), 2);
}

#[test]
fn test_line_helper() {
    let idx = LineIndex::new("abc\ndef\nghi");
    assert_eq!(idx.line(0), 0);
    assert_eq!(idx.line(4), 1);
    assert_eq!(idx.line(8), 2);
}

#[test]
fn test_1indexed_columns() {
    // LineIndex is 0-indexed; parser errors add 1 when building ParserError
    let idx = LineIndex::new("var x;");
    let lc = idx.line_col(4); // 'x'
    assert_eq!(lc.line, 0);
    assert_eq!(lc.col, 4);
}

#[test]
fn test_empty_source() {
    let idx = LineIndex::new("");
    assert_eq!(idx.line_col(0), LineCol { line: 0, col: 0 });
}
