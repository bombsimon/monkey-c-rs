use crate::ast::{CommentStmt, CommentTable};
use crate::line_index::LineIndex;

/// Walks the comments of a file in source order as the formatter emits nodes, handing each one out
/// exactly once to the first node that asks for its position.
#[derive(Debug, Default, Clone)]
pub struct CommentCursor {
    comments: Vec<CommentStmt>,
    next: usize,
    last_end: usize,
}

impl CommentCursor {
    /// A cursor before the first comment in `table`.
    pub fn new(table: &CommentTable) -> Self {
        let mut comments = table.comments.clone();
        comments.sort_by_key(|c| c.span.start);

        Self {
            comments,
            next: 0,
            last_end: 0,
        }
    }

    /// Drain the comments that start before `pos`.
    pub fn drain_before(&mut self, pos: usize) -> Vec<CommentStmt> {
        let start = self.next;
        while self.next < self.comments.len() && self.comments[self.next].span.start < pos {
            self.last_end = self.comments[self.next].span.end;
            self.next += 1;
        }

        self.comments[start..self.next].to_vec()
    }

    /// Drain the comments that start on `line`, such as the trailing one in `x; // tail`.
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

    /// Drain the comments on `end_line` that start in `[min_pos, max_pos)`, so a comment that
    /// belongs to a later sibling on the same line isn't taken early.
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

    /// Where the last drained comment ends, or zero if none has been drained, so a node's end can
    /// include its trailing comment.
    pub fn last_end(&self) -> usize {
        self.last_end
    }

    /// The next undrained comment, if it starts before `pos`.
    pub fn peek_before(&self, pos: usize) -> Option<&CommentStmt> {
        if self.next < self.comments.len() && self.comments[self.next].span.start < pos {
            Some(&self.comments[self.next])
        } else {
            None
        }
    }

    /// The undrained comments with `span.start` in `[start, end)`, without consuming them.
    pub fn peek_in(&self, start: usize, end: usize) -> impl Iterator<Item = &CommentStmt> {
        self.comments[self.next..]
            .iter()
            .take_while(move |c| c.span.start < end)
            .filter(move |c| c.span.start >= start)
    }

    /// True if there is any undrained comment with `span.start` in `[start, end)`.
    pub fn has_comment_in(&self, start: usize, end: usize) -> bool {
        self.peek_in(start, end).next().is_some()
    }

    /// The comments written with `span.start` in `[start, end)`, drained or not. For questions
    /// about the source rather than about what's left to emit, so the answer doesn't depend on
    /// which comments an earlier node already took.
    pub fn written_in(&self, start: usize, end: usize) -> impl Iterator<Item = &CommentStmt> {
        let first = self.comments.partition_point(|c| c.span.start < start);

        self.comments[first..]
            .iter()
            .take_while(move |c| c.span.start < end)
    }

    /// True if there is any undrained line comment (`//`) with `span.start` in `[start, end)`.
    pub fn has_line_comment_in(&self, start: usize, end: usize) -> bool {
        self.peek_in(start, end).any(|c| !c.is_block)
    }

    /// True if there is an undrained `//` comment on `line` with `span.start` in `[min_pos,
    /// max_pos)`.
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
