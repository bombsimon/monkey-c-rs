/// Maps byte offsets to line/column positions within a source file.
///
/// Build once per file with `LineIndex::new(source)`, then call `line_col` as needed. Spans in the
/// AST remain raw byte offsets; this type is only needed when you want a human-readable location
/// (error messages, blank-line logic).
pub struct LineIndex {
    /// Byte offset of the first character of each line (line_starts[0] == 0).
    line_starts: Vec<u32>,
    /// `is_blank[i]` is `true` when line `i`'s body is empty or whitespace-only in the source. Used
    /// by [`Self::blank_lines_between`] so a line that contains a comment is *not* counted as
    /// blank.
    is_blank: Vec<bool>,
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

        let mut is_blank = Vec::with_capacity(starts.len());
        for i in 0..starts.len() {
            let line_start = starts[i] as usize;
            let line_end = starts
                .get(i + 1)
                .map(|&s| s as usize - 1)
                .unwrap_or(source.len());
            let content = source.get(line_start..line_end).unwrap_or("");
            is_blank.push(content.trim().is_empty());
        }

        Self {
            line_starts: starts,
            is_blank,
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
    /// A "blank line" is a line whose body is empty or whitespace-only in the source. Lines that
    /// contain comments are *not* counted as blank, so `a;\n// comment\nb;` counts as zero blank
    /// lines even though the line number of `b` is two more than that of `a`.
    pub fn blank_lines_between(&self, prev_end: u32, next_start: u32) -> u32 {
        let end_line = self.line(prev_end);
        let start_line = self.line(next_start);
        if start_line <= end_line + 1 {
            return 0;
        }

        let mut count = 0;
        for line in (end_line + 1)..start_line {
            if self.is_blank[line as usize] {
                count += 1;
            }
        }
        count
    }
}
