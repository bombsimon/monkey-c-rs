use monkey_c_parser::line_index::{LineCol, LineIndex};

#[test]
fn test_line_col() {
    let idx = LineIndex::new("hello\nworld\n");
    assert_eq!(idx.line_col(0), LineCol { line: 0, col: 0 });
    assert_eq!(idx.line_col(5), LineCol { line: 0, col: 5 }); // the \n itself
    assert_eq!(idx.line_col(8), LineCol { line: 1, col: 2 });
    assert_eq!(idx.line(12), 2);

    assert_eq!(LineIndex::new("").line_col(0), LineCol { line: 0, col: 0 });
}

#[test]
fn test_blank_lines_between() {
    assert_eq!(LineIndex::new("a\nb").blank_lines_between(1, 2), 0);
    assert_eq!(LineIndex::new("a\n\nb").blank_lines_between(1, 3), 1);
    assert_eq!(LineIndex::new("a\n\n  \nb").blank_lines_between(1, 6), 2);
    assert_eq!(LineIndex::new("a\n// c\n\nb").blank_lines_between(1, 8), 1);
}

#[test]
fn test_indent() {
    let idx = LineIndex::new("a\n    b\n\tc\n\n  ");
    assert_eq!(idx.indent(0), 0);
    assert_eq!(idx.indent(1), 4);
    assert_eq!(idx.indent(2), 1);
    assert_eq!(idx.indent(3), 0);
    assert_eq!(idx.indent(4), 2);
}

#[test]
fn test_starts_line() {
    let idx = LineIndex::new("a; // c\n\t  // own\n");
    assert!(idx.starts_line(0));
    assert!(!idx.starts_line(3), "comment after code");
    assert!(idx.starts_line(11), "comment after indentation");
}
