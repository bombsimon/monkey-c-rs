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

/// Render a `Doc` to a `String`, breaking groups that would exceed `width`.
pub fn render(doc: &Doc, width: usize) -> String {
    let mut out = String::new();
    let mut col = 0usize;
    render_doc(doc, width, 0, Mode::Break, &mut out, &mut col);
    out
}

fn render_doc(
    doc: &Doc,
    width: usize,
    indent: usize,
    mode: Mode,
    out: &mut String,
    col: &mut usize,
) {
    match doc {
        Doc::Empty => {}
        Doc::Text(s) => {
            out.push_str(s);
            *col += s.len();
        }
        Doc::HardLine => {
            out.push('\n');
            out.push_str(&" ".repeat(indent));
            *col = indent;
        }
        Doc::SoftLine => {
            if mode == Mode::Break {
                out.push('\n');
                out.push_str(&" ".repeat(indent));
                *col = indent;
            }
        }
        Doc::Line => {
            if mode == Mode::Flat {
                out.push(' ');
                *col += 1;
            } else {
                out.push('\n');
                out.push_str(&" ".repeat(indent));
                *col = indent;
            }
        }
        Doc::Indent(docs) => {
            for d in docs {
                render_doc(d, width, indent + 4, mode, out, col);
            }
        }
        Doc::Group(docs) => {
            let child_mode = if fits_flat(docs, width.saturating_sub(*col)) {
                Mode::Flat
            } else {
                Mode::Break
            };

            for d in docs {
                render_doc(d, width, indent, child_mode, out, col);
            }
        }
        Doc::Concat(docs) => {
            for d in docs {
                render_doc(d, width, indent, mode, out, col);
            }
        }
        Doc::BlankLine => {
            out.push_str("\n\n");
            out.push_str(&" ".repeat(indent));
            *col = indent;
        }
        Doc::FlatOrBreak(flat, break_) => {
            let child = if mode == Mode::Flat { flat } else { break_ };
            render_doc(child, width, indent, mode, out, col);
        }
    }
}

/// Returns the flat (single-line) character width of `doc`, or `None` if it
/// contains a [`Doc::HardLine`] or [`Doc::BlankLine`] and can never be flat.
pub(crate) fn flat_width(doc: &Doc) -> Option<usize> {
    match doc {
        Doc::Empty => Some(0),
        Doc::Text(s) => Some(s.len()),
        Doc::HardLine | Doc::BlankLine => None,
        Doc::SoftLine => Some(0),
        Doc::Line => Some(1),
        Doc::Indent(docs) | Doc::Group(docs) | Doc::Concat(docs) => {
            let mut total = 0usize;
            for d in docs {
                total += flat_width(d)?;
            }

            Some(total)
        }
        Doc::FlatOrBreak(flat, _) => flat_width(flat),
    }
}

/// Returns `true` if rendering `docs` in flat mode would fit within `remaining` columns.
fn fits_flat(docs: &[Doc], remaining: usize) -> bool {
    let mut total = 0usize;

    for d in docs {
        match flat_width(d) {
            None => return false,
            Some(w) => {
                total += w;
                if total > remaining {
                    return false;
                }
            }
        }
    }

    true
}
