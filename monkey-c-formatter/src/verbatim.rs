//! `rafiki: fmt off` and `rafiki: fmt on` comments, which keep a run of statements or declarations
//! exactly as written, for layouts like hand-aligned tables that the formatter would undo.
//!
//! Regions follow ruff's `fmt: off`: they only start and end between items in the same body, and
//! one left open runs to the end of that body. The lines keep their layout relative to each other,
//! but the region as a whole is indented like the code around it, as block comments are.

use monkey_c_parser::ast::{CommentStmt, Span};

use crate::Formatter;
use crate::doc::Doc;

const PREFIX: &str = "rafiki:";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum FmtDirective {
    Off,
    On,
}

/// The `rafiki: fmt` directive in `comment`, if it holds one. Anything after `off` or `on` is a
/// reason, like the text after a `rafiki: ignore` list.
fn fmt_directive(comment: &CommentStmt) -> Option<FmtDirective> {
    let rest = comment.text.trim_start().strip_prefix(PREFIX)?;
    let mut words = rest.split_whitespace();

    if words.next()? != "fmt" {
        return None;
    }

    match words.next()? {
        "off" => Some(FmtDirective::Off),
        "on" => Some(FmtDirective::On),
        _ => None,
    }
}

impl Formatter {
    /// The source to keep as written when a `rafiki: fmt off` comment comes before the first of
    /// `items`, the items left in a body ending at `body_end`. Starts at the comment and ends at the
    /// matching `rafiki: fmt on`, or after everything left in the body when there's none.
    pub(crate) fn fmt_off_region(&self, items: &[Span], body_end: usize) -> Option<Span> {
        let next_start = items.first()?.start;
        let cursor = self.comment_cursor.borrow();

        // A pair closed before the item covers nothing, so its comments are left as they are.
        let mut off = None;

        for comment in cursor.peek_in(0, next_start) {
            match self.own_line_directive(comment) {
                Some(FmtDirective::Off) if off.is_none() => off = Some(comment),
                Some(FmtDirective::On) => off = None,
                _ => {}
            }
        }

        let off = off?;

        // An `on` inside one of the items belongs to a body further in, so it can't end a region
        // started out here.
        let inside_item = |comment: &CommentStmt| {
            items
                .iter()
                .any(|item| item.start < comment.span.start && comment.span.start < item.end)
        };

        let on = cursor.peek_in(next_start, body_end).find(|comment| {
            self.own_line_directive(comment) == Some(FmtDirective::On) && !inside_item(comment)
        });

        let end = match on {
            Some(on) => on.span.end,
            None => {
                let items_end = items.last().map_or(next_start, |item| item.end);
                let comments_end = cursor
                    .peek_in(off.span.end, body_end)
                    .map(|comment| comment.span.end)
                    .max()
                    .unwrap_or(0);

                items_end.max(comments_end)
            }
        };

        Some(Span {
            start: off.span.start,
            end,
        })
    }

    /// The source in `region`, as written apart from trailing whitespace and the indentation of the
    /// line it starts on. A line indented less than that lines up with it instead, since nothing can
    /// go left of the body. Drains the comments inside it, since they're part of the text.
    pub(crate) fn verbatim_doc(&self, region: Span) -> Doc {
        self.comment_cursor.borrow_mut().drain_before(region.end);

        let start_line = self.line_index.line(region.start as u32);
        let source_indent = self.line_index.indent(start_line) as usize;

        let mut lines = self.source[region.start..region.end].split('\n');
        let first = lines.next().unwrap_or_default();
        let mut parts = vec![Doc::text(first.trim_end())];

        for line in lines {
            let line = line.trim_end();
            let indent_width = line.len() - line.trim_start_matches([' ', '\t']).len();
            let line = &line[indent_width.min(source_indent)..];

            // A raw newline keeps blank lines free of the indentation a hard line would add.
            if line.is_empty() {
                parts.push(Doc::text("\n"));
            } else {
                parts.push(Doc::HardLine);
                parts.push(Doc::text(line));
            }
        }

        Doc::concat(parts)
    }

    /// The directive in `comment` when nothing else shares its line, since a region can only start
    /// or end between items.
    fn own_line_directive(&self, comment: &CommentStmt) -> Option<FmtDirective> {
        let rest_of_line = self.source[comment.span.end..].split('\n').next()?;

        if !self.line_index.starts_line(comment.span.start as u32)
            || !rest_of_line.trim().is_empty()
        {
            return None;
        }

        fmt_directive(comment)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn directive(text: &str) -> Option<FmtDirective> {
        fmt_directive(&CommentStmt {
            text: text.to_string(),
            is_block: false,
            span: Span { start: 0, end: 0 },
        })
    }

    #[test]
    fn reads_off_and_on() {
        assert_eq!(directive(" rafiki: fmt off"), Some(FmtDirective::Off));
        assert_eq!(directive(" rafiki: fmt on"), Some(FmtDirective::On));
        assert_eq!(directive("rafiki:fmt   off"), Some(FmtDirective::Off));
    }

    #[test]
    fn allows_a_reason() {
        assert_eq!(
            directive(" rafiki: fmt off keeps the table aligned"),
            Some(FmtDirective::Off)
        );
    }

    #[test]
    fn skips_other_comments() {
        for text in [
            " fmt: off",
            " rafiki: fmt",
            " rafiki: fmt of",
            " rafiki: ignore[fmt]",
            "! rafiki: fmt off",
            " see rafiki: fmt off",
        ] {
            assert_eq!(directive(text), None, "{text:?}");
        }
    }
}
