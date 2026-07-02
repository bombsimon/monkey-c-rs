use crate::ast::Span;

/// A source comment, either a `// …` line comment or a `/* … */` block
/// comment. `is_block` is the distinction; the stored `text` is the raw
/// content between the comment delimiters.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#comments>
#[derive(Debug, PartialEq, Clone)]
pub struct CommentStmt {
    pub text: String,
    pub is_block: bool,
    pub span: Span,
}

/// All comments in a parsed file, in source order. Comments are not woven
/// into the AST itself; consumers attach them to nodes via a separate pass.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct CommentTable {
    pub comments: Vec<CommentStmt>,
}

impl CommentTable {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, comment: CommentStmt) {
        self.comments.push(comment);
    }
}
