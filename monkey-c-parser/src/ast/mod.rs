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

/// The visibility and `static` keywords written in front of a declaration. Both keep their source
/// position since the compiler accepts them in either order and the formatter keeps it as written.
#[derive(Debug, Default, PartialEq)]
pub struct Modifiers {
    pub visibility: Option<Spanned<Visibility>>,
    pub static_kw_start: Option<Position>,
}

impl Modifiers {
    pub fn visibility(&self) -> Option<&Visibility> {
        self.visibility.as_ref().map(|visibility| &visibility.node)
    }

    pub fn is_static(&self) -> bool {
        self.static_kw_start.is_some()
    }

    pub fn is_empty(&self) -> bool {
        self.visibility.is_none() && !self.is_static()
    }
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

/// A comma-separated list that keeps the position of each `,`, so a comment can stay on the side
/// of the separator it was written on. The comma at index `i` follows item `i`, and a comma after
/// the last item is a trailing comma. Derefs to the items.
#[derive(Debug, PartialEq)]
pub struct Separated<T> {
    pub items: Vec<T>,
    pub commas: Vec<Position>,
}

impl<T> Separated<T> {
    /// The position of the `,` after the item at `index`, if there is one.
    pub fn comma_after(&self, index: usize) -> Option<Position> {
        self.commas.get(index).copied()
    }

    pub fn has_trailing_comma(&self) -> bool {
        !self.items.is_empty() && self.commas.len() == self.items.len()
    }
}

impl<T> Default for Separated<T> {
    fn default() -> Self {
        Self {
            items: Vec::new(),
            commas: Vec::new(),
        }
    }
}

impl<T> std::ops::Deref for Separated<T> {
    type Target = [T];
    fn deref(&self) -> &[T] {
        &self.items
    }
}

impl<T> IntoIterator for Separated<T> {
    type Item = T;
    type IntoIter = std::vec::IntoIter<T>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.into_iter()
    }
}

impl<'a, T> IntoIterator for &'a Separated<T> {
    type Item = &'a T;
    type IntoIter = std::slice::Iter<'a, T>;
    fn into_iter(self) -> Self::IntoIter {
        self.items.iter()
    }
}
