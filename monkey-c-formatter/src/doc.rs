/// Formatting intermediate representation.
///
/// Formatters build a `Doc` tree describing layout *intent*, then pass it to
/// `render` which resolves flat-vs-break based on the target line width.
/// This is the Wadler-Lindig pretty-printing algorithm.
///
/// # Typical usage
///
/// ```ignore
/// let doc = Doc::group(vec![
///     Doc::text("{"),
///     Doc::indent(vec![
///         Doc::line(),               // newline in break mode, space in flat
///         Doc::text("\"key\": \"value\""),
///     ]),
///     Doc::line(),
///     Doc::text("}"),
/// ]);
/// let output = render(&doc, 100);
/// ```
#[derive(Debug, Clone)]
pub enum Doc {
    /// Literal text with no embedded newlines.
    Text(String),
    /// A `//` comment. Rendered like [`Doc::Text`] but has no width when deciding whether a
    /// group fits, since it runs to the end of the line and breaking the code before it cannot
    /// make it fit.
    LineComment(String),
    /// Always a newline followed by current indentation. Never flattened.
    HardLine,
    /// Empty in flat mode; newline + indent in break mode.
    SoftLine,
    /// Space in flat mode; newline + indent in break mode.
    Line,
    /// Increase indentation level for all children.
    Indent(Vec<Doc>),
    /// Try to render all children on one line. If they overflow `width`,
    /// switch to break mode and expand `Line`/`SoftLine` nodes.
    Group(Vec<Doc>),
    /// Concatenate documents with no additional spacing logic.
    Concat(Vec<Doc>),
    /// Empty document — renders to nothing.
    Empty,
    /// An empty line: emits `\n\n` followed by current indentation.
    /// Used to preserve a single blank line between declarations or statements.
    BlankLine,
    /// Renders the first child in flat mode, the second in break mode.
    ///
    /// Width is measured from the flat child so enclosing [`Doc::Group`]s make
    /// the correct fit decision.
    FlatOrBreak(Box<Doc>, Box<Doc>),
}

impl Doc {
    pub fn text(s: impl Into<String>) -> Self {
        Doc::Text(s.into())
    }

    pub fn line_comment(s: impl Into<String>) -> Self {
        Doc::LineComment(s.into())
    }

    pub fn group(docs: Vec<Doc>) -> Self {
        Doc::Group(docs)
    }

    pub fn indent(docs: Vec<Doc>) -> Self {
        Doc::Indent(docs)
    }

    pub fn concat(docs: Vec<Doc>) -> Self {
        Doc::Concat(docs)
    }

    pub fn line() -> Self {
        Doc::Line
    }

    pub fn soft_line() -> Self {
        Doc::SoftLine
    }

    pub fn hard_line() -> Self {
        Doc::HardLine
    }

    pub fn blank_line() -> Self {
        Doc::BlankLine
    }

    pub(crate) fn flat_or_break(flat: Doc, break_: Doc) -> Self {
        Doc::FlatOrBreak(Box::new(flat), Box::new(break_))
    }
}

/// Whether a [`Doc::Group`] is being rendered flat (single line) or broken (multi-line).
#[derive(Clone, Copy, PartialEq)]
enum Mode {
    Flat,
    Break,
}

/// A doc waiting to be rendered, with the indentation and mode it inherits from its parent.
#[derive(Clone, Copy)]
struct Command<'a> {
    indent: usize,
    mode: Mode,
    doc: &'a Doc,
}

/// Render a `Doc` to a `String`, breaking groups that would exceed `width`.
///
/// Pending docs live on an explicit stack rather than the call stack so that a group's fit check
/// can see what follows it. A group only fits if everything up to the next line break does, so
/// a call's arguments break when the `);` after them would overflow.
pub fn render(doc: &Doc, width: usize) -> String {
    let mut out = String::new();
    let mut col = 0usize;
    let mut stack = vec![Command {
        indent: 0,
        mode: Mode::Break,
        doc,
    }];

    while let Some(Command { indent, mode, doc }) = stack.pop() {
        match doc {
            Doc::Empty => {}
            Doc::Text(s) | Doc::LineComment(s) => {
                out.push_str(s);
                col += s.len();
            }
            Doc::HardLine => newline(&mut out, &mut col, indent),
            Doc::SoftLine => {
                if mode == Mode::Break {
                    newline(&mut out, &mut col, indent);
                }
            }
            Doc::Line => {
                if mode == Mode::Flat {
                    out.push(' ');
                    col += 1;
                } else {
                    newline(&mut out, &mut col, indent);
                }
            }
            Doc::Indent(docs) => push_all(&mut stack, docs, indent + 4, mode),
            Doc::Group(docs) => {
                let fits = mode == Mode::Flat || fits(docs, &stack, width.saturating_sub(col));
                let child_mode = if fits { Mode::Flat } else { Mode::Break };

                push_all(&mut stack, docs, indent, child_mode);
            }
            Doc::Concat(docs) => push_all(&mut stack, docs, indent, mode),
            Doc::BlankLine => {
                out.push('\n');
                newline(&mut out, &mut col, indent);
            }
            Doc::FlatOrBreak(flat, break_) => {
                let child = if mode == Mode::Flat { flat } else { break_ };
                stack.push(Command {
                    indent,
                    mode,
                    doc: child,
                });
            }
        }
    }

    out
}

fn newline(out: &mut String, col: &mut usize, indent: usize) {
    out.push('\n');
    out.push_str(&" ".repeat(indent));
    *col = indent;
}

/// Push `docs` so that the first one is rendered first.
fn push_all<'a>(stack: &mut Vec<Command<'a>>, docs: &'a [Doc], indent: usize, mode: Mode) {
    for doc in docs.iter().rev() {
        stack.push(Command { indent, mode, doc });
    }
}

/// Whether `group` fits flat in `remaining` columns, together with whatever follows it on the
/// same line. The docs in `rest` keep their own mode, so the first line break an enclosing
/// broken group will take ends the measurement; a line break inside the flat group itself
/// (a [`Doc::HardLine`] or [`Doc::BlankLine`]) means it can never be flat.
fn fits(group: &[Doc], rest: &[Command], mut remaining: usize) -> bool {
    let mut pending: Vec<(Mode, &Doc)> = group.iter().rev().map(|doc| (Mode::Flat, doc)).collect();
    let mut rest = rest.iter().rev();

    loop {
        let (mode, doc) = match pending.pop() {
            Some(next) => next,
            None => match rest.next() {
                Some(command) => (command.mode, command.doc),
                None => return true,
            },
        };

        match doc {
            Doc::Empty | Doc::LineComment(_) => {}
            Doc::Text(s) => match remaining.checked_sub(s.len()) {
                Some(left) => remaining = left,
                None => return false,
            },
            Doc::HardLine | Doc::BlankLine => return mode == Mode::Break,
            Doc::SoftLine => {
                if mode == Mode::Break {
                    return true;
                }
            }
            Doc::Line => {
                if mode == Mode::Break {
                    return true;
                }

                match remaining.checked_sub(1) {
                    Some(left) => remaining = left,
                    None => return false,
                }
            }
            Doc::Indent(docs) | Doc::Group(docs) | Doc::Concat(docs) => {
                pending.extend(docs.iter().rev().map(|doc| (mode, doc)));
            }
            Doc::FlatOrBreak(flat, break_) => {
                let child = if mode == Mode::Flat { flat } else { break_ };
                pending.push((mode, child));
            }
        }
    }
}
