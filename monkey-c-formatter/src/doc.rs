use unicode_width::UnicodeWidthStr;

/// Layout intent for the Wadler-Lindig pretty printer: [`render`] decides which groups stay on
/// one line and which break, based on the line width.
#[derive(Debug, Clone)]
pub enum Doc {
    /// Text with no newlines.
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
    Indent(Vec<Doc>),
    /// Children on one line if they fit, otherwise with every `Line`/`SoftLine` broken.
    Group(Vec<Doc>),
    /// Cells kept on one line when they fit, otherwise packed into as many aligned columns as
    /// the width allows, so long lists of short items don't take a line each.
    Fill(Vec<Doc>, Alignment),
    Concat(Vec<Doc>),
    Empty,
    BlankLine,
    /// The first child in flat mode, the second in break mode.
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

    /// Concatenate `docs`, or [`Doc::Empty`] when there is nothing in them, so callers can check
    /// whether anything was produced.
    pub fn concat(docs: Vec<Doc>) -> Self {
        if docs.iter().all(|doc| matches!(doc, Doc::Empty)) {
            return Doc::Empty;
        }

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

    /// A bracketed body: `open` with whatever follows it on its line, `body` indented on the lines
    /// below and `close` on a line of its own. Without a body `open` and `close` share a line as in
    /// `{}`, unless something after `open`, such as a `//` comment, has to end that line.
    pub(crate) fn bracketed(open: Doc, after_open: Doc, body: Doc, close: Doc) -> Self {
        match (&after_open, &body) {
            (Doc::Empty, Doc::Empty) => Doc::concat(vec![open, close]),
            (_, Doc::Empty) => Doc::concat(vec![open, after_open, Doc::HardLine, close]),
            _ => Doc::concat(vec![
                open,
                after_open,
                Doc::Indent(vec![Doc::HardLine, body]),
                Doc::HardLine,
                close,
            ]),
        }
    }

    pub(crate) fn flat_or_break(flat: Doc, break_: Doc) -> Self {
        Doc::FlatOrBreak(Box::new(flat), Box::new(break_))
    }
}

/// Which edge the cells of a [`Doc::Fill`] line up on. Right alignment lines up the last
/// character before a trailing comma, so numbers line up by their last digit.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Alignment {
    Left,
    Right,
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
    render_in(doc, width, Mode::Break)
}

pub(crate) fn render_flat(doc: &Doc) -> String {
    render_in(doc, usize::MAX, Mode::Flat)
}

fn render_in(doc: &Doc, width: usize, mode: Mode) -> String {
    let mut out = String::new();
    let mut col = 0usize;
    let mut stack = vec![Command {
        indent: 0,
        mode,
        doc,
    }];

    while let Some(Command { indent, mode, doc }) = stack.pop() {
        match doc {
            Doc::Empty => {}
            Doc::Text(s) | Doc::LineComment(s) => {
                out.push_str(s);
                col += display_width(s);
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
            Doc::Fill(cells, alignment) => {
                let rendered: Vec<String> = cells.iter().map(render_flat).collect();

                if mode == Mode::Flat {
                    let line = rendered.join(" ");
                    out.push_str(&line);
                    col += display_width(&line);
                } else {
                    let available = width.saturating_sub(indent);

                    write_fill_rows(&mut out, &mut col, &rendered, *alignment, indent, available);
                }
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

/// The number of terminal columns `s` occupies, so wide characters such as CJK count as two
/// columns and combining marks as none, matching how editors lay out the line.
pub(crate) fn display_width(s: &str) -> usize {
    s.width()
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
            Doc::Fill(cells, _) => {
                // A flat fill puts a space between its cells, just like a flat `Line`.
                for (index, cell) in cells.iter().enumerate().rev() {
                    pending.push((mode, cell));

                    if index > 0 {
                        pending.push((mode, &Doc::Line));
                    }
                }
            }
            Doc::Text(s) => match remaining.checked_sub(display_width(s)) {
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

/// Write `cells` in as many aligned columns as fit in `available`, or one per line when not even
/// two do.
fn write_fill_rows(
    out: &mut String,
    col: &mut usize,
    cells: &[String],
    alignment: Alignment,
    indent: usize,
    available: usize,
) {
    let cell_widths: Vec<usize> = cells
        .iter()
        .map(|cell| {
            let width = display_width(cell);

            // Counting the missing comma lines the last number up with the digits above it.
            if alignment == Alignment::Right && !cell.ends_with(',') {
                return width + 1;
            }

            width
        })
        .collect();
    let columns = fill_columns(&cell_widths, available).unwrap_or(1);
    let column_widths = column_widths(&cell_widths, columns);

    for (index, cell) in cells.iter().enumerate() {
        if index > 0 && index % columns == 0 {
            newline(out, col, indent);
        } else if index > 0 {
            out.push(' ');
            *col += 1;
        }

        let ends_row = (index + 1) % columns == 0 || index + 1 == cells.len();
        // Left-aligned padding at the end of a row would only be trailing whitespace.
        let padding = if columns > 1 && (alignment == Alignment::Right || !ends_row) {
            " ".repeat(column_widths[index % columns] - cell_widths[index])
        } else {
            String::new()
        };

        match alignment {
            Alignment::Left => {
                out.push_str(cell);
                out.push_str(&padding);
            }
            Alignment::Right => {
                out.push_str(&padding);
                out.push_str(cell);
            }
        }

        *col += padding.len() + display_width(cell);
    }
}

/// The most columns, at least two, whose padded rows fit in `available_width`.
fn fill_columns(cell_widths: &[usize], available_width: usize) -> Option<usize> {
    let narrowest = cell_widths.iter().min()?;
    // Every cell is at least as wide as the narrowest one, with a space between cells.
    let max_columns = ((available_width + 1) / (narrowest + 1)).min(cell_widths.len());

    // Not monotonic: long cells can share a column at `c` but not at `c - 1`.
    (2..=max_columns)
        .rev()
        .find(|&columns| padded_row_width(cell_widths, columns) <= available_width)
}

fn padded_row_width(cell_widths: &[usize], columns: usize) -> usize {
    let spaces = columns - 1;

    column_widths(cell_widths, columns).iter().sum::<usize>() + spaces
}

/// The widest cell in each column when `cell_widths` are laid out in rows of `columns`.
fn column_widths(cell_widths: &[usize], columns: usize) -> Vec<usize> {
    let mut widths = vec![0; columns];

    for (index, cell_width) in cell_widths.iter().enumerate() {
        let width = &mut widths[index % columns];
        *width = (*width).max(*cell_width);
    }

    widths
}

#[cfg(test)]
mod tests {
    use super::*;

    // Cell widths include the comma, so `:a,` is 3 and the last cell without one is 2.

    #[test]
    fn column_widths_are_the_widest_cell_in_each_column() {
        assert_eq!(column_widths(&[3, 7, 2, 5, 1], 2), vec![3, 7]);
        assert_eq!(column_widths(&[3, 7, 2, 5, 1], 3), vec![5, 7, 2]);
        assert_eq!(padded_row_width(&[10, 3, 3, 3, 10, 3, 3, 3], 3), 25);
        assert_eq!(padded_row_width(&[3, 7, 2], 1), 7);
    }

    #[test]
    fn fill_columns_picks_the_most_columns_that_fit() {
        // Long items sharing a column allow more columns than fewer would.
        assert_eq!(fill_columns(&[10, 3, 3, 3, 10, 3, 3, 3], 24), Some(4));
        assert_eq!(fill_columns(&[2, 2, 1], 111), Some(3));
        assert_eq!(fill_columns(&[2, 2, 2, 2, 2, 2], 8), Some(3));
        assert_eq!(fill_columns(&[2, 2, 2, 2, 2, 2], 10), Some(3));
        assert_eq!(fill_columns(&[2, 2, 2, 2, 2, 2], 11), Some(4));
    }

    #[test]
    fn fill_columns_needs_room_for_two_cells() {
        assert_eq!(fill_columns(&[2, 2, 2], 2), None);
        assert_eq!(fill_columns(&[2], 111), None);
        assert_eq!(fill_columns(&[], 111), None);
    }
}
