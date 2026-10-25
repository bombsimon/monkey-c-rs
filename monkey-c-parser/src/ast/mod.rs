//! The syntax tree of a Monkey C file.
//!
//! Every node keeps its [`Span`], a byte range into the source. Fields named `*_pos` or
//! `*_kw_start`, like `op_pos` or `as_kw_start`, are the byte offset of that one token, kept so a
//! comment can stay on the side of it where it was written. Lists keep the positions of their
//! commas through [`Separated`].
//!
//! Source spelling that doesn't change meaning is kept too, like `and` against `&&`, number
//! literals as written and redundant parentheses, so the tree can be written back as it was.

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

/// A name, which may be dotted like `Toybox.Lang` where the grammar allows it.
pub type Ident = String;

/// Byte offset into the original source string.
pub type Position = usize;

/// The name of a `:symbolName` literal.
pub type Symbol = String;

/// The visibility keyword of a declaration. `Hidden` means the same as `Protected`, but is kept
/// apart so the keyword written is kept.
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
/// position since the compiler accepts them in either order and the formatter can keep it as
/// written.
#[derive(Debug, Default, PartialEq)]
pub struct Modifiers {
    pub visibility: Option<Spanned<Visibility>>,
    pub static_kw_start: Option<Position>,
}

impl Modifiers {
    /// The visibility keyword, without its position.
    pub fn visibility(&self) -> Option<&Visibility> {
        self.visibility.as_ref().map(|visibility| &visibility.node)
    }

    pub fn is_static(&self) -> bool {
        self.static_kw_start.is_some()
    }

    /// Whether neither keyword is written.
    pub fn is_empty(&self) -> bool {
        self.visibility.is_none() && !self.is_static()
    }
}

/// A half-open byte range `[start, end)` into the source. Use
/// [`LineIndex`](crate::line_index::LineIndex) to convert it to lines and columns.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Span {
    pub start: Position,
    pub end: Position,
}

/// A node paired with its source [`Span`], for nodes that otherwise wouldn't have one.
#[derive(Debug, PartialEq)]
pub struct Spanned<T> {
    pub span: Span,
    pub node: T,
}

impl<T> Spanned<T> {
    /// Where the node starts.
    pub fn start(&self) -> usize {
        self.span.start
    }
}

/// Something the grammar wraps in parentheses, like `if (cond)`, with where the parentheses are.
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

    /// Whether a `,` follows the last item, which the formatter reads as a wish to keep the list
    /// broken over lines.
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
