//! Values for the settings that respell code without changing what it means, like `|` to `or` in
//! a union type.
//!
//! Each one defaults to a canonical form and has a `Preserve` value to keep the code as written,
//! since these are taste rather than layout. A single spot can still be kept as written with
//! `// rafiki: fmt off`.

use serde::Deserialize;

/// The separator between the members of a union type. `or` is what Garmin's documentation and
/// nearly all real code use, and it can't be mistaken for a bitwise or.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum UnionStyle {
    #[default]
    Or,
    Pipe,
    Preserve,
}

/// How a type that may be `null` is written: `String?` or `String or Null`.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Nullable {
    #[default]
    Shorthand,
    Union,
    Preserve,
}

/// The separator between the entries of an annotation, `(:a :b)` or `(:a, :b)`. Spaces are the
/// only documented form.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum AnnotationSeparator {
    #[default]
    Space,
    Comma,
    Preserve,
}

/// Whether to drop the legacy `@` in front of a resource reference like `@Rez.Strings.name`.
///
/// There's no `@`-adding counterpart: a bare `Rez.Strings.name` is an ordinary member access, and
/// telling which of those are resources would take more than the syntax.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ResourceRef {
    #[default]
    Bare,
    Preserve,
}

/// The order of the visibility and `static` keywords. The compiler takes either, and visibility
/// first matches Java and C#.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum ModifierOrder {
    #[default]
    VisibilityFirst,
    Preserve,
}

/// Whether to drop parentheses that can't change how the code is read, like those around a
/// returned value.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, Deserialize)]
#[serde(rename_all = "kebab-case")]
pub enum Parens {
    #[default]
    Drop,
    Preserve,
}
