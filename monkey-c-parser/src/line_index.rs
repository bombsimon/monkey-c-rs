/// Maps byte offsets to line/column positions within a source file.
///
/// Build once per file with `LineIndex::new(source)`, then call `line_col` as
/// needed. Spans in the AST remain raw byte offsets; this type is only needed
/// when you want a human-readable location (error messages, blank-line logic).
pub struct LineIndex {
    /// Byte offset of the first character of each line (line_starts[0] == 0).
    line_starts: Vec<u32>,
}

/// A 0-indexed line and column position.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LineCol {
    pub line: u32,
    pub col: u32,
}

impl LineIndex {
    pub fn new(source: &str) -> Self {
        let mut starts = vec![0u32];

        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                starts.push(i as u32 + 1);
            }
        }

        Self {
            line_starts: starts,
        }
    }

    /// Convert a byte offset into a `LineCol`.
    ///
    /// Panics if `offset` is beyond the end of the source.
    pub fn line_col(&self, offset: u32) -> LineCol {
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;

        LineCol {
            line: line as u32,
            col: offset - self.line_starts[line],
        }
    }

    /// Return the line number (0-indexed) for a byte offset.
    pub fn line(&self, offset: u32) -> u32 {
        self.line_col(offset).line
    }

    /// Return how many blank lines separate two offsets.
    ///
    /// "Blank lines" means lines that contain nothing between `end` and
    /// `start` of the next token — i.e. `line(next_start) - line(prev_end) - 1`.
    pub fn blank_lines_between(&self, prev_end: u32, next_start: u32) -> u32 {
        let end_line = self.line(prev_end);
        let start_line = self.line(next_start);
        start_line.saturating_sub(end_line + 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        assert_eq!(idx.line_col(5), LineCol { line: 0, col: 5 });
        assert_eq!(idx.line_col(6), LineCol { line: 1, col: 0 });
        assert_eq!(idx.line_col(8), LineCol { line: 1, col: 2 });
    }

    #[test]
    fn test_blank_lines_between() {
        // "a\n\nb" — one blank line between a and b
        let src = "a\n\nb";
        let idx = LineIndex::new(src);
        // 'a' ends at offset 1, 'b' starts at offset 3
        assert_eq!(idx.blank_lines_between(1, 3), 1);
    }

    #[test]
    fn test_no_blank_lines() {
        let src = "a\nb";
        let idx = LineIndex::new(src);
        assert_eq!(idx.blank_lines_between(1, 2), 0);
    }
}
