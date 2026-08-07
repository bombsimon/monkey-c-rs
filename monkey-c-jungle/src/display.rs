use crate::ast::{
    Assignment, Comment, Entry, JungleFile, LiteralPart, QualifiedName, Text, Value, ValueKind,
};

/// Indent for the continuation lines of a value list.
const CONTINUATION_INDENT: &str = "  ";

/// Printing emits the line structure the grammar requires and normalises the rest: one space
/// around `=`, none around `;`, one instruction per line, trailing newline. A `\` continuation is
/// folded away; a break a comment forces is not, because nothing else can hold the line open.
impl std::fmt::Display for JungleFile {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for (i, entry) in self.entries.iter().enumerate() {
            writeln!(f, "{entry}")?;

            // A comment on the last value swallows the instruction's terminator, so the entry
            // below would run into it.
            if ends_with_comment(entry) && follows_on(self.entries.get(i + 1)) {
                writeln!(f)?;
            }
        }

        Ok(())
    }
}

impl std::fmt::Display for Entry {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Entry::Assignment(assignment) => write!(f, "{assignment}"),
            Entry::Comment(comment) => write!(f, "{comment}"),
            Entry::BlankLine => Ok(()),
        }
    }
}

impl std::fmt::Display for Assignment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} =", self.target)?;

        if !self.values.is_empty() {
            write!(f, " ")?;
        }

        write_value_list(f, &self.values)
    }
}

/// Write `;`-separated values, breaking the line after any that carries a comment. Shared by an
/// instruction and by a `[…]` group, which lays its contents out the same way.
fn write_value_list(f: &mut std::fmt::Formatter<'_>, values: &[Value]) -> std::fmt::Result {
    for (i, value) in values.iter().enumerate() {
        let last = i + 1 == values.len();

        write!(f, "{value}")?;

        if !last {
            write!(f, ";")?;
        }

        write_comments(f, value, last)?;
    }

    Ok(())
}

impl std::fmt::Display for QualifiedName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.segments.join("."))
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.kind {
            ValueKind::Literal(parts) => {
                for part in parts {
                    write!(f, "{part}")?;
                }

                Ok(())
            }
            ValueKind::Group(values) => {
                write!(f, "[")?;
                write_value_list(f, values)?;

                // A comment on the last value ate the line break, so the `]` has to start a new
                // line — `monkeyc` rejects it sharing one.
                if ends_with_comment_value(values) {
                    writeln!(f)?;
                }

                write!(f, "]")
            }
        }
    }
}

impl std::fmt::Display for LiteralPart {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LiteralPart::Text(text) => write!(f, "{text}"),
            LiteralPart::Reference(name) => write!(f, "$({name})"),
        }
    }
}

impl std::fmt::Display for Text {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.quoted || needs_quotes(&self.text) {
            write!(f, "\"{}\"", self.text)
        } else {
            write!(f, "{}", self.text)
        }
    }
}

impl std::fmt::Display for Comment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "#{}", self.text)
    }
}

/// Write a value's comments — the first on its line, the rest below — and then break the line if
/// another value follows.
///
/// The break is the point: a comment ends its line, so the next value has nowhere to go but a new
/// one. Equally, a value *without* a comment must stay where it is, since a bare line break after
/// its `;` would end the instruction. Hence the mixed shape:
///
/// ```jungle
/// base.sourcePath = source;$(round.sourcePath); # round devices
///   wearable-source # everything else
/// ```
fn write_comments(f: &mut std::fmt::Formatter<'_>, value: &Value, last: bool) -> std::fmt::Result {
    let Some((shared, below)) = value.comments.split_first() else {
        return Ok(());
    };

    write!(f, " {shared}")?;

    for comment in below {
        write!(f, "\n{CONTINUATION_INDENT}{comment}")?;
    }

    if !last {
        write!(f, "\n{CONTINUATION_INDENT}")?;
    }

    Ok(())
}

/// Whether the entry's last line ends in a comment, and so has eaten its own terminator.
fn ends_with_comment(entry: &Entry) -> bool {
    match entry {
        Entry::Comment(_) => false,
        Entry::BlankLine => false,
        Entry::Assignment(assignment) => ends_with_comment_value(&assignment.values),
    }
}

fn ends_with_comment_value(values: &[Value]) -> bool {
    values.last().is_some_and(|v| !v.comments.is_empty())
}

/// Whether the next entry would land on the line directly below.
fn follows_on(next: Option<&Entry>) -> bool {
    matches!(next, Some(Entry::Assignment(_) | Entry::Comment(_)))
}

/// Whether `text` needs quoting to survive a round-trip. Jungle has no escapes, so text holding a
/// `"` or `#` is written raw — no jungle value can express either.
fn needs_quotes(text: &str) -> bool {
    !text.contains(['"', '#'])
        && text.contains(|c: char| c.is_whitespace() || matches!(c, ';' | '=' | '[' | ']'))
}
