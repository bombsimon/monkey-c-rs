pub mod comment;
pub mod decl;
pub mod expr;
pub mod literal;
pub mod ops;
pub mod stmt;
pub mod types;

pub use self::comment::*;
pub use self::decl::*;
pub use self::expr::*;
pub use self::literal::*;
pub use self::ops::*;
pub use self::stmt::*;
pub use self::types::*;

/// An identifier - a plain string name.
pub type Ident = String;

/// Byte offset into the original source string.
pub type Position = usize;

/// A symbol name - the string part of a `:symbolName` literal.
pub type Symbol = String;

/// Visibility modifier on a declaration.
///
/// `Hidden` is synonymous with `Protected` — both restrict access to the
/// declaring class and its subclasses. The distinction is preserved so the
/// formatter can round-trip the original keyword.
///
/// <https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference/#data-hiding>
#[derive(Debug, PartialEq)]
pub enum Visibility {
    Hidden,
    Private,
    Protected,
    Public,
}

/// A half-open byte range `[start, end)` into the original source string.
///
/// All offsets are global and measured from byte 0 of the file.
/// Use [`LineIndex`](crate::line_index::LineIndex) to convert to line/column.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

/// An AST node paired with its source [`Span`]. Used for identifier nodes that
/// need both their value and source position for comment placement.
#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> Spanned<T> {
    pub fn start(&self) -> usize {
        self.span.start
    }
}

/// A parenthesised wrapper around an inner AST piece. Tracks the source positions of the opening
/// `(` and closing `)`. Used wherever the grammar requires parens — `if (cond)`, `function
/// f(args)`, `for (init; cond; update)`, `switch (disc)`.
#[derive(Debug, PartialEq)]
pub struct Parens<T> {
    pub open: Position,
    pub inner: T,
    pub close: Position,
}

impl<T> std::ops::Deref for Parens<T> {
    type Target = T;
    fn deref(&self) -> &T {
        &self.inner
    }
}
