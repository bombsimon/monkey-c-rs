mod alignment;
mod comments;
mod declarations;
pub mod doc;
mod expressions;
mod lists;
mod member_chain;
mod operators;
mod statements;
mod types;

use monkey_c_parser::ast::CommentStmt;
use monkey_c_parser::comments::CommentCursor;
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::line_index::LineIndex;
use monkey_c_parser::parser::ParseOutput;
use monkey_c_parser::token;

use crate::alignment::align_trailing_comments;
use doc::render;

use std::cell::RefCell;
use std::collections::HashMap;

/// Formats a Monkey C AST back into source text.
///
/// Construct with [`Formatter::new`], optionally configure with builder
/// methods, then call [`Formatter::format`].
///
/// The formatter uses the Wadler-Lindig algorithm (see [`doc`]) to make
/// line-breaking decisions globally rather than per-node, so a dict or array
/// that fits on one line is kept there automatically.
pub struct Formatter {
    line_index: LineIndex,
    /// Maximum line width before a [`doc::Doc::Group`] is broken.
    line_width: usize,
    /// When `true`, runs of related entries are rendered with their separator
    /// operators column-aligned, as are trailing comments on consecutive lines.
    align_pairs: bool,
    /// Positional drain cursor over all source comments. Advanced forward as
    /// the formatter builds the Doc tree; each comment is emitted exactly once
    /// at the first output position that follows its source location.
    comment_cursor: RefCell<CommentCursor>,
}

impl Formatter {
    /// Create a formatter for `source`.
    pub fn new(source: impl AsRef<str>) -> Self {
        let source = source.as_ref();

        Self {
            line_index: LineIndex::new(source),
            line_width: 100,
            align_pairs: false,
            comment_cursor: RefCell::new(CommentCursor::default()),
        }
    }

    /// Override the target line width (default: 100).
    pub fn with_line_width(mut self, width: usize) -> Self {
        self.line_width = width;
        self
    }

    /// Enable column-aligned separators and trailing comments across related entries (opt-in).
    pub fn with_alignment(mut self, align_pairs: bool) -> Self {
        self.align_pairs = align_pairs;
        self
    }

    /// Format a parsed file and return the result as a `String`.
    pub fn format(&self, output: &ParseOutput) -> String {
        *self.comment_cursor.borrow_mut() = CommentCursor::new(&output.comments);

        let doc = self.ast_to_doc(&output.ast);
        let rendered = render(&doc, self.line_width);
        let mut formatted = if self.align_pairs {
            align_trailing_comments(&rendered)
        } else {
            rendered
        };

        if !formatted.ends_with('\n') {
            formatted.push('\n');
        }

        formatted
    }

    /// "No comment left behind": source comments that did **not** survive into
    /// `rendered`. Re-lexes `rendered` and matches each source comment against
    /// the output comments by kind plus whitespace-normalised content.
    pub fn lost_comments(output: &ParseOutput, rendered: &str) -> Vec<CommentStmt> {
        fn normalize(text: &str) -> String {
            text.split_whitespace().collect::<Vec<_>>().join(" ")
        }

        let mut present: HashMap<(bool, String), usize> = HashMap::new();
        let mut lexer = Lexer::new(rendered);
        loop {
            let (_start, tok, _end) = lexer.next_token();
            match tok {
                token::Type::Eof => break,
                token::Type::Comment(t) => *present.entry((false, normalize(&t))).or_default() += 1,
                token::Type::BlockComment(t) => {
                    *present.entry((true, normalize(&t))).or_default() += 1;
                }
                _ => {}
            }
        }

        let mut lost = Vec::new();
        for c in &output.comments.comments {
            let key = (c.is_block, normalize(&c.text));
            match present.get_mut(&key) {
                Some(n) if *n > 0 => *n -= 1,
                _ => lost.push(c.clone()),
            }
        }

        lost
    }
}
