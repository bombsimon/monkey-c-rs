/// Maps the byte offsets used by AST spans to lines and columns.
pub struct LineIndex {
    line_starts: Vec<u32>,
    is_blank: Vec<bool>,
    indents: Vec<u32>,
}

/// A line and a column in bytes, both counted from 0.
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
        let mut indents = Vec::with_capacity(starts.len());
        for i in 0..starts.len() {
            let line_start = starts[i] as usize;
            let line_end = starts
                .get(i + 1)
                .map(|&s| s as usize - 1)
                .unwrap_or(source.len());
            let content = source.get(line_start..line_end).unwrap_or("");
            is_blank.push(content.trim().is_empty());
            indents.push((content.len() - content.trim_start_matches([' ', '\t']).len()) as u32);
        }

        Self {
            line_starts: starts,
            is_blank,
            indents,
        }
    }

    /// The line and column of `offset`. Panics if `offset` is beyond the end of the source.
    pub fn line_col(&self, offset: u32) -> LineCol {
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;

        LineCol {
            line: line as u32,
            col: offset - self.line_starts[line],
        }
    }

    /// The line of `offset`, counted from 0.
    pub fn line(&self, offset: u32) -> u32 {
        self.line_col(offset).line
    }

    /// How many spaces and tabs `line` starts with.
    pub fn indent(&self, line: u32) -> u32 {
        self.indents[line as usize]
    }

    /// Whether only whitespace comes before `offset` on its line, such as for a comment written
    /// on a line of its own.
    pub fn starts_line(&self, offset: u32) -> bool {
        let LineCol { line, col } = self.line_col(offset);

        col == self.indent(line)
    }

    /// How many whitespace-only lines separate two offsets. A line holding a comment doesn't count.
    pub fn blank_lines_between(&self, prev_end: u32, next_start: u32) -> u32 {
        let lines = (self.line(prev_end) + 1)..self.line(next_start);

        lines.filter(|&line| self.is_blank[line as usize]).count() as u32
    }
}
