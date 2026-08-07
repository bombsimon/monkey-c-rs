/// Tokens of the jungle build language.
///
/// Outside a handful of structural characters, jungle is raw text destined for a path or an
/// annotation name, so a whole run of it becomes one [`Type::Word`] rather than being split on
/// `.`, `/` and friends. Quoted strings and `$(…)` dereferences are lexed whole for the same
/// reason. Newlines terminate a build instruction, so they are tokens rather than whitespace.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Word(String),
    /// Contents of a `"…"` value, without the quotes.
    QuotedWord(String),
    /// Contents of a `$(…)` dereference, without the `$(` and `)`.
    Deref(String),
    /// Text after the `#`. The line break belongs to the comment, so no [`Type::Newline`] follows.
    Comment(String),

    Assign,
    Semicolon,
    LBracket,
    RBracket,

    /// A `"…` or `$(…` that runs to the end of its line unclosed, carrying the raw text so the
    /// error can quote it.
    UnclosedString(String),
    UnclosedDeref(String),

    Newline,
    Eof,
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Word(s) => write!(f, "{s}"),
            Type::QuotedWord(s) => write!(f, "\"{s}\""),
            Type::Deref(s) => write!(f, "$({s})"),
            Type::Comment(s) => write!(f, "#{s}"),

            Type::Assign => write!(f, "="),
            Type::Semicolon => write!(f, ";"),
            Type::LBracket => write!(f, "["),
            Type::RBracket => write!(f, "]"),

            Type::UnclosedString(s) | Type::UnclosedDeref(s) => write!(f, "{s}"),

            Type::Newline => write!(f, "newline"),
            Type::Eof => write!(f, "end of file"),
        }
    }
}
