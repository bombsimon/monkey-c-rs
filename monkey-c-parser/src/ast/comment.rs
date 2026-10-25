use crate::ast::Span;

/// A `// …` or `/* … */` comment, with `text` being the raw content between the delimiters.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#comments>
#[derive(Debug, PartialEq, Clone)]
pub struct CommentStmt {
    pub text: String,
    pub is_block: bool,
    pub span: Span,
}

/// All comments in a parsed file, in source order. They are kept out of the AST and attached to
/// nodes by whoever needs them.
#[derive(Debug, Default, Clone, PartialEq)]
pub struct CommentTable {
    pub comments: Vec<CommentStmt>,
}
