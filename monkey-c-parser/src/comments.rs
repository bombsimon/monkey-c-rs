//! Positional comment cursor — advances through a sorted [`CommentTable`] as
//! the formatter emits nodes, draining each comment exactly once at the first
//! output position that falls after its source location.
use crate::ast::{CommentStmt, CommentTable};
use crate::line_index::LineIndex;

/// A consuming iterator over source comments, sorted by position.
///
/// The formatter holds one [`CommentCursor`] per format pass. As it builds the
/// `Doc` tree in source order it calls `drain_before` (leading
/// comments) and `drain_on_line` (same-line trailing comments). Every comment
/// is emitted exactly once; none are dropped unless no formatter call covers
/// their position.
#[derive(Debug, Default, Clone)]
pub struct CommentCursor {
    comments: Vec<CommentStmt>,
    /// Index of the next un-drained comment.
    next: usize,
    /// Byte-end of the last drained comment; 0 when no comment has been
    /// drained yet. Used by callers to compute gap widths between items.
    last_end: usize,
}

impl CommentCursor {
    pub fn new(table: &CommentTable) -> Self {
        let mut comments = table.comments.clone();
        comments.sort_by_key(|c| c.span.start);

        Self {
            comments,
            next: 0,
            last_end: 0,
        }
    }

    /// Drain all comments whose `span.start < pos` and return them in source
    /// order. Call this before emitting a node at byte offset `pos` to pick up
    /// any leading or standalone comments that precede it.
    pub fn drain_before(&mut self, pos: usize) -> Vec<CommentStmt> {
        let start = self.next;
        while self.next < self.comments.len() && self.comments[self.next].span.start < pos {
            self.last_end = self.comments[self.next].span.end;
            self.next += 1;
        }

        self.comments[start..self.next].to_vec()
    }

    /// Drain all remaining comments that start on source line `line`. Call
    /// this after emitting a node whose last character is on `line` to pick
    /// up same-line trailing comments (`x; // tail`).
    pub fn drain_on_line(&mut self, line: u32, line_index: &LineIndex) -> Vec<CommentStmt> {
        let start = self.next;
        while self.next < self.comments.len()
            && line_index.line(self.comments[self.next].span.start as u32) == line
        {
            self.last_end = self.comments[self.next].span.end;
            self.next += 1;
        }

        self.comments[start..self.next].to_vec()
    }

    /// Drain same-line trailing comments that start in `[min_pos, max_pos)`.
    ///
    /// `min_pos` should be the end of the emitted node (byte-exclusive) so
    /// comments that precede the node's end are excluded. `max_pos` should be
    /// the start of the next sibling so comments belonging to a later node
    /// are not captured early. Pass `usize::MAX` for `max_pos` when there is
    /// no following sibling on the same line.
    pub fn drain_trailing(
        &mut self,
        min_pos: usize,
        max_pos: usize,
        end_line: u32,
        line_index: &LineIndex,
    ) -> Vec<CommentStmt> {
        let start = self.next;
        while self.next < self.comments.len() {
            let c = &self.comments[self.next];
            if line_index.line(c.span.start as u32) != end_line {
                break;
            }

            if c.span.start < min_pos || c.span.start >= max_pos {
                break;
            }

            self.last_end = c.span.end;
            self.next += 1;
        }

        self.comments[start..self.next].to_vec()
    }

    /// Drain all remaining comments and return them.
    pub fn drain_rest(&mut self) -> Vec<CommentStmt> {
        let rest = self.comments[self.next..].to_vec();
        if let Some(last) = rest.last() {
            self.last_end = last.span.end;
        }

        self.next = self.comments.len();
        rest
    }

    /// Byte-end of the last drained comment. Zero when no comment has been
    /// drained yet. Callers use this with `max(node.span.end)` to compute the
    /// effective end of a node including its trailing comment for gap sizing.
    pub fn last_end(&self) -> usize {
        self.last_end
    }

    /// Return a reference to the next undrained comment if its `span.start < pos`,
    /// without consuming it.
    pub fn peek_before(&self, pos: usize) -> Option<&CommentStmt> {
        if self.next < self.comments.len() && self.comments[self.next].span.start < pos {
            Some(&self.comments[self.next])
        } else {
            None
        }
    }

    /// True if there is any undrained comment with `span.start` in `[start, end)`.
    pub fn has_comment_in(&self, start: usize, end: usize) -> bool {
        self.comments[self.next..]
            .iter()
            .any(|c| c.span.start >= start && c.span.start < end)
    }

    /// True if there is an undrained line comment (`//`) on source line `line`.
    pub fn has_line_comment_on(&self, line: u32, line_index: &LineIndex) -> bool {
        self.has_line_comment_between(0, usize::MAX, line, line_index)
    }

    /// True if there is an undrained `//` comment on `line` with
    /// `span.start` in `[min_pos, max_pos)`.
    pub fn has_line_comment_between(
        &self,
        min_pos: usize,
        max_pos: usize,
        line: u32,
        line_index: &LineIndex,
    ) -> bool {
        self.comments[self.next..]
            .iter()
            .take_while(|c| line_index.line(c.span.start as u32) <= line)
            .any(|c| {
                !c.is_block
                    && line_index.line(c.span.start as u32) == line
                    && c.span.start >= min_pos
                    && c.span.start < max_pos
            })
    }
}
