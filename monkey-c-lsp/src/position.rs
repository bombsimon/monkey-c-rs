//! Conversion between the parser's byte offsets and LSP positions.
//!
//! The parser and linter work in UTF-8 byte offsets ([`monkey_c_parser::ast::Span`]). LSP positions
//! are `(line, character)` where `character` is a **UTF-16** code-unit offset into the line, not a
//! byte or `char` offset — so a line containing multibyte UTF-8 (accents, emoji) shifts every
//! column unless the conversion counts UTF-16 units. [`PositionMapper`] does that conversion in
//! both directions off a single precomputed line table.

use lsp_types::{Position, Range};
use monkey_c_parser::ast::Span;

/// Maps byte offsets to/from LSP `(line, utf-16 character)` positions for one source document.
pub struct PositionMapper<'a> {
    source: &'a str,
    /// Byte offset of the start of each line; `line_starts[0] == 0`.
    line_starts: Vec<usize>,
}

impl<'a> PositionMapper<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut line_starts = vec![0usize];
        for (i, b) in source.bytes().enumerate() {
            if b == b'\n' {
                line_starts.push(i + 1);
            }
        }

        Self {
            source,
            line_starts,
        }
    }

    /// Convert a byte offset into an LSP [`Position`]. Offsets past the end of the source clamp to
    /// the final position rather than panicking.
    pub fn position(&self, offset: usize) -> Position {
        let offset = offset.min(self.source.len());
        let line = self.line_starts.partition_point(|&s| s <= offset) - 1;
        let line_start = self.line_starts[line];

        // Count UTF-16 code units from the line start up to `offset`.
        let character = self.source[line_start..offset]
            .chars()
            .map(char::len_utf16)
            .sum::<usize>();

        Position {
            line: line as u32,
            character: character as u32,
        }
    }

    /// Convert a byte [`Span`] into an LSP [`Range`].
    pub fn range(&self, span: Span) -> Range {
        Range {
            start: self.position(span.start),
            end: self.position(span.end),
        }
    }

    /// Convert an LSP [`Position`] back into a byte offset. A `character` past the end of its line
    /// clamps to the line's end; a `line` past the end of the document clamps to the document's
    /// end.
    ///
    /// Not yet used by a request handler — kept for the client-supplied ranges that code actions
    /// and range formatting will need.
    #[allow(dead_code)]
    pub fn offset(&self, position: Position) -> usize {
        let Some(&line_start) = self.line_starts.get(position.line as usize) else {
            return self.source.len();
        };

        let line_end = self
            .line_starts
            .get(position.line as usize + 1)
            .map(|&s| s - 1) // drop the '\n'
            .unwrap_or(self.source.len());

        let target = position.character as usize;
        let mut utf16 = 0;
        for (byte_offset, ch) in self.source[line_start..line_end].char_indices() {
            if utf16 >= target {
                return line_start + byte_offset;
            }
            utf16 += ch.len_utf16();
        }

        line_end
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn ascii_positions() {
        let src = "abc\ndef";
        let m = PositionMapper::new(src);
        assert_eq!(m.position(0), Position::new(0, 0));
        assert_eq!(m.position(2), Position::new(0, 2));
        // The '\n' at byte 3 maps to end of line 0.
        assert_eq!(m.position(3), Position::new(0, 3));
        assert_eq!(m.position(4), Position::new(1, 0));
        assert_eq!(m.position(6), Position::new(1, 2));
    }

    #[test]
    fn multibyte_counts_utf16_units() {
        // "é" is 2 UTF-8 bytes but 1 UTF-16 unit; "x" after it is byte 3.
        let src = "é x";
        let m = PositionMapper::new(src);
        assert_eq!(m.position(0), Position::new(0, 0));
        // byte 3 (the 'x') is UTF-16 character 2 (é=1, space=1).
        assert_eq!(m.position(3), Position::new(0, 2));
    }

    #[test]
    fn astral_char_is_two_utf16_units() {
        // "😀" is 4 UTF-8 bytes and 2 UTF-16 units (a surrogate pair).
        let src = "😀y";
        let m = PositionMapper::new(src);
        // 'y' sits at byte 4, UTF-16 character 2.
        assert_eq!(m.position(4), Position::new(0, 2));
    }

    #[test]
    fn offset_is_inverse_of_position() {
        let src = "é😀\nx = 1;\nlast";
        let m = PositionMapper::new(src);
        for offset in char_boundaries(src) {
            let pos = m.position(offset);
            assert_eq!(m.offset(pos), offset, "round-trip failed at byte {offset}");
        }
    }

    #[test]
    fn offset_clamps_out_of_range() {
        let src = "ab\ncd";
        let m = PositionMapper::new(src);
        // character past line end clamps to line end (byte 2, the '\n').
        assert_eq!(m.offset(Position::new(0, 99)), 2);
        // line past document end clamps to document end.
        assert_eq!(m.offset(Position::new(50, 0)), src.len());
    }

    fn char_boundaries(src: &str) -> Vec<usize> {
        (0..=src.len())
            .filter(|&i| src.is_char_boundary(i))
            .collect()
    }
}
