mod alignment;
mod comments;
mod declarations;
pub mod doc;
mod expressions;
mod lists;
mod member_chain;
mod numbers;
pub mod options;
mod statements;
mod types;
mod verbatim;

use monkey_c_parser::ast::CommentStmt;
use monkey_c_parser::comments::CommentCursor;
use monkey_c_parser::lexer::Lexer;
use monkey_c_parser::line_index::LineIndex;
use monkey_c_parser::parser::ParseOutput;
use monkey_c_parser::token;

use crate::alignment::align_trailing_comments;
use crate::options::{
    AnnotationSeparator, ModifierOrder, Nullable, Numbers, ResourceRef, UnionStyle,
};
use doc::render;

use std::cell::RefCell;
use std::collections::{HashMap, HashSet};

/// Formats a Monkey C AST back into source text, deciding line breaks with the Wadler-Lindig
/// algorithm in [`doc`].
pub struct Formatter {
    source: String,
    line_index: LineIndex,
    line_width: usize,
    align_pairs: bool,
    short_array_element_width: usize,
    union_style: UnionStyle,
    nullable: Nullable,
    annotation_separator: AnnotationSeparator,
    resource_ref: ResourceRef,
    modifier_order: ModifierOrder,
    parens: options::Parens,
    sort_imports: bool,
    numbers: Numbers,
    /// Where operands written right after a cast start. A `(` there has to stay, or the cast's
    /// type would take what follows as one more union member.
    after_cast: RefCell<HashSet<usize>>,
    comment_cursor: RefCell<CommentCursor>,
}

impl Formatter {
    /// Create a formatter for `source`.
    pub fn new(source: impl AsRef<str>) -> Self {
        let source = source.as_ref();

        Self {
            source: source.to_string(),
            line_index: LineIndex::new(source),
            line_width: 100,
            align_pairs: false,
            short_array_element_width: 8,
            union_style: UnionStyle::default(),
            nullable: Nullable::default(),
            annotation_separator: AnnotationSeparator::default(),
            resource_ref: ResourceRef::default(),
            modifier_order: ModifierOrder::default(),
            parens: options::Parens::default(),
            sort_imports: true,
            numbers: Numbers::default(),
            after_cast: RefCell::default(),
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

    /// Fill arrays whose items are all at most `width` columns wide, packing them into aligned
    /// columns instead of one per line. `0` never fills (default: 8).
    pub fn with_short_array_element_width(mut self, width: usize) -> Self {
        self.short_array_element_width = width;
        self
    }

    /// Set the separator between the members of a union type (default: `or`).
    pub fn with_union_style(mut self, union_style: UnionStyle) -> Self {
        self.union_style = union_style;
        self
    }

    /// Set how a type that may be `null` is written (default: `T?`).
    pub fn with_nullable(mut self, nullable: Nullable) -> Self {
        self.nullable = nullable;
        self
    }

    /// Set the separator between the entries of an annotation (default: a space).
    pub fn with_annotation_separator(mut self, separator: AnnotationSeparator) -> Self {
        self.annotation_separator = separator;
        self
    }

    /// Set whether the legacy `@` before a resource reference is dropped (default: dropped).
    pub fn with_resource_ref(mut self, resource_ref: ResourceRef) -> Self {
        self.resource_ref = resource_ref;
        self
    }

    /// Set the order of the visibility and `static` keywords (default: visibility first).
    pub fn with_modifier_order(mut self, modifier_order: ModifierOrder) -> Self {
        self.modifier_order = modifier_order;
        self
    }

    /// Set whether parentheses that can't change how the code reads are dropped (default:
    /// dropped).
    pub fn with_parens(mut self, parens: options::Parens) -> Self {
        self.parens = parens;
        self
    }

    /// Sort each run of `using` and `import` declarations into groups (default: on).
    pub fn with_sort_imports(mut self, sort_imports: bool) -> Self {
        self.sort_imports = sort_imports;
        self
    }

    /// Set how each part of a number literal is written (default: see [`Numbers`]).
    pub fn with_numbers(mut self, numbers: Numbers) -> Self {
        self.numbers = numbers;
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

    /// The source comments missing from `rendered`, matched by kind and text with whitespace
    /// normalized.
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
