//! Comma-separated lists: calls, parameters, arrays and dictionaries, with the comments around each
//! comma kept on the side they were written on.

use monkey_c_parser::ast::{
    ArrayExpr, CallArg, CallExpr, DictEntry, DictExpr, Expr, LiteralValue, Parameter, Parens,
    Separated, Span,
};

use crate::Formatter;
use crate::doc::{Doc, display_width};
use crate::expressions::{format_double_lit, format_float_lit};

/// An item in a delimited list (array entry, dict pair, call arg, etc.) along
/// with any comments that trail it inside the bracketed list. Used by
/// [`Formatter::format_list`].
pub(crate) struct ListItem {
    pub(crate) content: Doc,
    pub(crate) before_separator: Doc,
    pub(crate) trailing: Doc,
    pub(crate) ends_line: bool,
}

impl Formatter {
    /// A list entry and the comments around the `,` after it at `comma`, each kept on the side of
    /// the comma it was written on. A comment that ends the line before the comma leaves the comma
    /// to start the next line, as in the source. A block comment in front of the next entry on the
    /// same line is left for that entry.
    pub(crate) fn list_item(
        &self,
        content: Doc,
        item_end: usize,
        comma: Option<usize>,
        next_item_start: Option<usize>,
        list_end: usize,
    ) -> ListItem {
        let next_start = next_item_start.unwrap_or(list_end);
        let ends_line = self.has_trailing_line_comment(item_end, next_start);
        let Some(comma) = comma else {
            return ListItem {
                content,
                before_separator: Doc::Empty,
                trailing: self.drain_trailing_doc(item_end, next_start),
                ends_line,
            };
        };

        let comma_starts_line = self.token_starts_new_line(item_end, comma);
        let mut before_separator = self.drain_before_token(item_end, comma);
        if comma_starts_line {
            before_separator = Doc::concat(vec![before_separator, Doc::HardLine]);
        }

        let comma_line = self.line_index.line(comma as u32);
        let next_on_comma_line =
            next_item_start.is_some_and(|start| self.line_index.line(start as u32) == comma_line);
        let trailing = if next_on_comma_line {
            Doc::Empty
        } else {
            self.drain_trailing_doc(comma + 1, next_start)
        };

        ListItem {
            content,
            before_separator,
            trailing,
            ends_line: ends_line || comma_starts_line,
        }
    }

    /// The `(…)` of a function, method type or interface method.
    pub(crate) fn parameter_list_to_doc(&self, parameters: &Parens<Separated<Parameter>>) -> Doc {
        let items = parameters
            .iter()
            .enumerate()
            .map(|(i, arg)| {
                let mut arg_parts = Vec::new();
                arg_parts.push(self.drain_leading_doc(arg.span.start));
                arg_parts.push(Doc::text(&arg.name.node));
                if let Some(ty) = &arg.type_ {
                    arg_parts.push(Doc::text(" "));
                    arg_parts
                        .push(self.drain_leading_doc(arg.as_kw_start.unwrap_or(ty.span.start)));
                    arg_parts.push(Doc::text("as "));
                    arg_parts.push(self.type_to_doc(ty));
                }

                // arg.span.end points to the start of the following `,` or `)`
                // token (the parser sets it to current_token_start after
                // parse_type, skipping past any same-line comment). Use the
                // type's own span end so trailing comments are captured on the
                // correct source line.
                let parameter_end = arg.type_.as_ref().map_or(arg.span.end, |t| t.span.end);

                self.list_item(
                    Doc::Concat(arg_parts),
                    parameter_end,
                    parameters.comma_after(i),
                    parameters.get(i + 1).map(|next| next.span.start),
                    parameters.close,
                )
            })
            .collect();

        self.format_list(
            "(",
            ")",
            items,
            self.drain_dangling_comments_doc(parameters.close),
            parameters.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }

    /// The `(…)` of a call whose only argument is an array or dict literal, rendered as
    /// `([` … `])` or `({` … `})` so the collection alone decides whether to break. Hugging
    /// saves a level of indentation for the entries. `None` when it does not apply, including when
    /// a comment sits between the parentheses and the brackets, since it would have nowhere to go.
    pub(crate) fn hugged_sole_collection(
        &self,
        args: &[CallArg],
        args_trailing_comma: bool,
        args_open: usize,
        call_end: usize,
    ) -> Option<Doc> {
        if args_trailing_comma {
            return None;
        }

        let [argument] = args else {
            return None;
        };

        if !matches!(argument.value, Expr::Array(_) | Expr::Dict(_)) {
            return None;
        }

        let array_span = *argument.value.span();
        let before_array = Span {
            start: args_open + 1,
            end: array_span.start,
        };
        let after_array = Span {
            start: array_span.end,
            end: call_end,
        };

        if self.has_comments_in(before_array) || self.has_comments_in(after_array) {
            return None;
        }

        Some(Doc::concat(vec![
            Doc::text("("),
            self.expr_inner_to_doc(&argument.value),
            Doc::text(")"),
        ]))
    }

    /// The `(…)` part of a call.
    pub(crate) fn call_arguments_to_doc(&self, e: &CallExpr) -> Doc {
        let before_open = self.drain_before_open_paren(e.args_open);
        if let Some(args) = self.hugged_sole_collection(
            &e.args,
            e.args.has_trailing_comma(),
            e.args_open,
            e.span.end,
        ) {
            return Doc::concat(vec![before_open, args]);
        }

        // Only capture comments between `(` and the first argument as after-open.
        // Comments inside an argument expression (e.g. `x - /* C */ 1`) must not
        // be stolen here — they belong to the sub-expression's drain_leading_doc.
        let first_arg_start = e
            .args
            .first()
            .map(|a| a.value.span().start)
            .unwrap_or(e.span.end);
        let after_open_force_newline =
            self.after_open_has_line_comment(e.args_open, first_arg_start);
        let after_open = self.drain_after_open_paren(e.args_open, first_arg_start);

        let items = self.call_args_to_items(&e.args, e.span.end);
        let before_close = self.drain_dangling_comments_doc(e.span.end);

        Doc::concat(vec![
            before_open,
            self.format_list(
                "(",
                ")",
                items,
                before_close,
                e.args.has_trailing_comma(),
                after_open,
                after_open_force_newline,
            ),
        ])
    }

    pub(crate) fn call_args_to_items(
        &self,
        args: &Separated<CallArg>,
        args_close: usize,
    ) -> Vec<ListItem> {
        args.iter()
            .enumerate()
            .map(|(i, a)| {
                let next_arg_start = args.get(i + 1).map(|next| next.value.span().start);
                let content =
                    self.expr_with_leading_bounded(&a.value, next_arg_start.unwrap_or(args_close));

                self.list_item(
                    content,
                    a.value.span().end,
                    args.comma_after(i),
                    next_arg_start,
                    args_close,
                )
            })
            .collect()
    }

    pub(crate) fn format_array(&self, e: &ArrayExpr) -> Doc {
        let body = self.format_array_body(e);

        if e.is_byte_array {
            Doc::concat(vec![body, Doc::text("b")])
        } else {
            body
        }
    }

    fn format_array_body(&self, e: &ArrayExpr) -> Doc {
        if e.entries.is_empty() {
            return self.empty_collection_to_doc(e.span, "[", "]");
        }

        if self.collection_must_break(e.span, e.entries.has_trailing_comma()) {
            return self.format_array_multiline(e);
        }

        self.format_collection_inline_or_break(
            e.span,
            &e.entries,
            "[",
            "]",
            |entry| entry.value.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                self.expr_with_leading_bounded(&entry.value, next_entry_start)
            },
        )
    }

    /// A trailing comma keeps a collection one entry per line, and so does a `//` comment since it
    /// ends its line. Block comments are left to the line width, like in a call.
    fn collection_must_break(&self, span: Span, trailing_comma: bool) -> bool {
        trailing_comma
            || self
                .comment_cursor
                .borrow()
                .has_line_comment_in(span.start, span.end)
    }

    fn format_array_multiline(&self, e: &ArrayExpr) -> Doc {
        self.format_collection_multiline(
            e.span,
            &e.entries,
            "[",
            "]",
            |entry| entry.value.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                self.expr_with_leading_bounded(&entry.value, next_entry_start)
            },
        )
    }

    #[allow(clippy::too_many_arguments)]
    pub(crate) fn format_list(
        &self,
        open: &str,
        close: &str,
        items: Vec<ListItem>,
        before_close: Doc,
        trailing_comma: bool,
        after_open: Doc,
        after_open_force_newline: bool,
    ) -> Doc {
        let has_after_open = !matches!(&after_open, Doc::Empty);
        let has_before_close = !matches!(&before_close, Doc::Empty);

        if items.is_empty() && !has_after_open {
            return Doc::bracketed(Doc::text(open), Doc::Empty, before_close, Doc::text(close));
        }

        // Line comments (`// …`) consume the rest of the source line, so any
        // list containing one must be forced multi-line regardless of width.
        // Block comments (`/* … */`) can stay inline — let the pretty-printer
        // decide based on the available line width.
        let force_multiline = trailing_comma
            || after_open_force_newline
            || has_before_close
            || items.iter().any(|i| i.ends_line);

        let last_idx = items.len().saturating_sub(1);
        let mut inner = Vec::new();
        for (i, item) in items.into_iter().enumerate() {
            if i > 0 {
                inner.push(if force_multiline {
                    Doc::HardLine
                } else {
                    Doc::Line
                });
            }

            inner.push(item.content);
            inner.push(item.before_separator);

            if i != last_idx || (force_multiline && trailing_comma) {
                inner.push(Doc::text(","));
            }

            inner.push(item.trailing);
        }

        if force_multiline {
            if has_before_close {
                inner.push(Doc::HardLine);
                inner.push(before_close);
            }

            return Doc::bracketed(
                Doc::text(open),
                after_open,
                Doc::concat(inner),
                Doc::text(close),
            );
        }

        // A block comment after the opening delimiter belongs to the first item, so it moves down
        // with it when the list breaks rather than being left behind on the delimiter's line.
        let indent_start = if has_after_open && inner.is_empty() {
            vec![after_open]
        } else if has_after_open {
            vec![Doc::SoftLine, after_open, Doc::text(" ")]
        } else {
            vec![Doc::SoftLine]
        };

        Doc::Group(vec![
            Doc::text(open),
            Doc::Indent([indent_start, vec![Doc::Concat(inner)]].concat()),
            Doc::SoftLine,
            Doc::text(close),
        ])
    }

    pub(crate) fn format_dict(&self, e: &DictExpr) -> Doc {
        if e.entries.is_empty() {
            return self.empty_collection_to_doc(e.span, "{", "}");
        }

        if self.collection_must_break(e.span, e.entries.has_trailing_comma()) {
            return self.format_dict_multiline(e);
        }

        if self.has_comments_in(e.span) {
            return self.format_dict_with_comments(e);
        }

        if self.align_pairs {
            return self.format_dict_aligned_or_inline(e);
        }

        self.format_dict_inline_or_break(e)
    }

    fn format_dict_aligned_or_inline(&self, e: &DictExpr) -> Doc {
        let mut flat_inner = Vec::new();
        for (i, entry) in e.entries.iter().enumerate() {
            if i > 0 {
                flat_inner.push(Doc::text(", "));
            }

            // format_dict guarantees no comments inside dict when this path is
            // taken, so expr_inner_to_doc is safe and avoids accidentally
            // draining trailing comments that belong to the outer context (e.g.
            // a `// comment` after `}` in an enclosing array entry).
            flat_inner.push(Doc::concat(vec![
                self.expr_inner_to_doc(&entry.key),
                Doc::text(" => "),
                self.expr_inner_to_doc(&entry.value),
            ]));
        }
        let flat_doc = Doc::concat(vec![
            Doc::text("{"),
            Doc::Concat(flat_inner),
            Doc::text("}"),
        ]);
        let break_doc = self.format_dict_multiline(e);

        Doc::Group(vec![Doc::flat_or_break(flat_doc, break_doc)])
    }

    fn format_dict_multiline(&self, e: &DictExpr) -> Doc {
        let padding_of = self.dict_key_padding(e);

        self.format_collection_multiline(
            e.span,
            &e.entries,
            "{",
            "}",
            |entry| entry.key.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                let padding = Doc::Text(padding_of(&entry.key));

                self.dict_entry_to_doc(entry, next_entry_start, padding)
            },
        )
    }

    /// A dict with block comments in it, kept on one line when it fits. Keys are only padded for
    /// alignment once it breaks.
    fn format_dict_with_comments(&self, e: &DictExpr) -> Doc {
        let padding_of = self.dict_key_padding(e);

        self.format_collection_inline_or_break(
            e.span,
            &e.entries,
            "{",
            "}",
            |entry| entry.key.span().start,
            |entry| entry.value.span().end,
            |entry, next_entry_start| {
                let padding = Doc::flat_or_break(Doc::Empty, Doc::Text(padding_of(&entry.key)));

                self.dict_entry_to_doc(entry, next_entry_start, padding)
            },
        )
    }

    /// The spaces after each key that line up the `=>` of a broken dict, none unless pairs are
    /// aligned.
    fn dict_key_padding(&self, e: &DictExpr) -> impl Fn(&Expr) -> String {
        let max_key_width = if self.align_pairs {
            e.entries
                .iter()
                .filter_map(|entry| expr_key_width(&entry.key))
                .max()
                .unwrap_or(0)
        } else {
            0
        };

        move |key| {
            expr_key_width(key)
                .map(|width| " ".repeat(max_key_width.saturating_sub(width)))
                .unwrap_or_default()
        }
    }

    /// A `key => value` pair with the comments around it, `padding` going between the key and
    /// `=>`.
    fn dict_entry_to_doc(&self, entry: &DictEntry, next_entry_start: usize, padding: Doc) -> Doc {
        let key = self.expr_with_leading(&entry.key);
        let key_end = entry.key.span().end;
        let arrow_breaks = self.token_starts_new_line(key_end, entry.arrow_pos);
        let before_arrow = self.drain_before_token(key_end, entry.arrow_pos);
        let value = self.after_token(
            entry.arrow_pos + "=>".len(),
            entry.value.span().start,
            || self.expr_with_leading_bounded(&entry.value, next_entry_start),
        );

        if arrow_breaks {
            return Doc::concat(vec![
                key,
                before_arrow,
                Doc::Indent(vec![Doc::HardLine, Doc::text("=>"), value]),
            ]);
        }

        Doc::concat(vec![key, before_arrow, padding, Doc::text(" =>"), value])
    }

    /// Render `entries` on one line when they fit and one per line otherwise, with the comments
    /// around them placed like in a call. `content_of` is as for the multi-line form.
    #[allow(clippy::too_many_arguments)]
    fn format_collection_inline_or_break<E>(
        &self,
        span: Span,
        entries: &Separated<E>,
        open: &str,
        close: &str,
        start_of: impl Fn(&E) -> usize,
        end_of: impl Fn(&E) -> usize,
        content_of: impl Fn(&E, usize) -> Doc,
    ) -> Doc {
        let first_start = entries.first().map(&start_of).unwrap_or(span.end);
        let after_open = self.drain_after_open_paren(span.start, first_start);
        let items = entries
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let next_entry_start = entries.get(i + 1).map(&start_of);

                self.list_item(
                    content_of(entry, next_entry_start.unwrap_or(span.end)),
                    end_of(entry),
                    entries.comma_after(i),
                    next_entry_start,
                    span.end,
                )
            })
            .collect();
        let before_close = self.drain_dangling_comments_doc(span.end);

        self.format_list(open, close, items, before_close, false, after_open, false)
    }

    /// Render `entries` one per line. `content_of` renders an entry given where the next one
    /// starts, which bounds the comments it may take.
    #[allow(clippy::too_many_arguments)]
    fn format_collection_multiline<E>(
        &self,
        span: Span,
        entries: &Separated<E>,
        open: &str,
        close: &str,
        start_of: impl Fn(&E) -> usize,
        end_of: impl Fn(&E) -> usize,
        content_of: impl Fn(&E, usize) -> Doc,
    ) -> Doc {
        let trailing_comma = entries.has_trailing_comma();
        let last_idx = entries.len().saturating_sub(1);

        // A comment on the line of the opening bracket stays there, unless the first entry shares
        // that line too and the comment is in front of it.
        let open_line = self.line_index.line(span.start as u32);
        let after_open = match entries.first().map(&start_of) {
            Some(first_start) if self.line_index.line(first_start as u32) != open_line => {
                self.drain_after_open_brace(span.start, first_start)
            }
            _ => Doc::Empty,
        };

        let mut inner: Vec<Doc> = Vec::new();
        let mut prev_end: Option<usize> = None;

        for (i, entry) in entries.iter().enumerate() {
            let entry_start = start_of(entry);

            // Drain any standalone comments before this entry — but only those on a
            // different line. A block comment on the same line as the entry is an inline
            // prefix (e.g. `/* INFO */ :key => val`) and is handled by `content_of`.
            let entry_line = self.line_index.line(entry_start as u32);
            let comment_start = self
                .comment_cursor
                .borrow()
                .peek_before(entry_start)
                .filter(|c| self.line_index.line(c.span.start as u32) != entry_line)
                .map(|c| c.span.start);

            if let Some(start) = comment_start {
                self.push_gap(&mut inner, prev_end, start, trailing_comma);
                let comment_docs = self.drain_leading_doc(entry_start);
                inner.push(comment_docs);
            } else {
                self.push_gap(&mut inner, prev_end, entry_start, trailing_comma);
            }

            // Only capture trailing comments up to the next entry's start (or
            // the collection close). This prevents stealing a comment from a
            // later entry or from outside the collection.
            let next_entry_start = entries.get(i + 1).map(&start_of);
            let content = content_of(entry, next_entry_start.unwrap_or(span.end));
            let entry_end = end_of(entry);
            let item = self.list_item(
                content,
                entry_end,
                entries.comma_after(i),
                next_entry_start,
                span.end,
            );

            let mut parts = vec![item.content, item.before_separator];
            if i != last_idx || trailing_comma {
                parts.push(Doc::text(","));
            }

            parts.push(item.trailing);
            inner.push(Doc::Concat(parts));
            prev_end = Some(self.comment_cursor.borrow().last_end().max(entry_end));
        }

        // Drain any standalone comments after the last entry.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            self.push_gap(&mut inner, prev_end, start, trailing_comma);
            inner.push(self.drain_dangling_comments_doc(span.end));
        }

        Doc::bracketed(
            Doc::text(open),
            after_open,
            Doc::concat(inner),
            Doc::text(close),
        )
    }

    /// An empty `[]` or `{}`, with any comments written inside it on lines of their own.
    fn empty_collection_to_doc(&self, span: Span, open: &str, close: &str) -> Doc {
        let mut body = Vec::new();
        if self.has_comments_in(Span {
            start: span.start + 1,
            end: span.end,
        }) {
            let comments = self.comment_cursor.borrow_mut().drain_before(span.end);
            for (i, comment) in comments.iter().enumerate() {
                if i > 0 {
                    body.push(Doc::HardLine);
                }

                body.push(self.comment_to_doc(comment));
            }
        }

        Doc::bracketed(
            Doc::text(open),
            Doc::Empty,
            Doc::concat(body),
            Doc::text(close),
        )
    }

    fn push_gap(
        &self,
        inner: &mut Vec<Doc>,
        prev_end: Option<usize>,
        next_start: usize,
        preserve_blanks: bool,
    ) {
        let Some(prev) = prev_end else { return };

        if preserve_blanks {
            inner.push(self.gap_between_positions(prev, next_start));
        } else {
            inner.push(Doc::HardLine);
        }
    }

    fn format_dict_inline_or_break(&self, e: &DictExpr) -> Doc {
        let items: Vec<ListItem> = e
            .entries
            .iter()
            .map(|entry| ListItem {
                // format_dict guarantees no comments inside dict when this
                // path is taken — use expr_inner_to_doc to avoid draining
                // trailing comments that belong to the outer context.
                content: Doc::concat(vec![
                    self.expr_inner_to_doc(&entry.key),
                    Doc::text(" => "),
                    self.expr_inner_to_doc(&entry.value),
                ]),
                before_separator: Doc::Empty,
                trailing: Doc::Empty,
                ends_line: false,
            })
            .collect();

        self.format_list(
            "{",
            "}",
            items,
            Doc::Empty,
            e.entries.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }
}

/// Compute the flat display width of a dict key expression without touching
/// the comment cursor. Used for alignment padding in multiline dicts.
///
/// Returns `None` when the expression cannot be represented as a single flat
/// token sequence (e.g. a call or binary expression as a key).
fn expr_key_width(expr: &Expr) -> Option<usize> {
    match expr {
        Expr::Ident(e) => Some(display_width(&e.name)),
        Expr::Me(_) => Some("me".len()),
        Expr::Self_(_) => Some("self".len()),
        Expr::Bling(_) => Some(1),
        Expr::Member(e) => Some(expr_key_width(&e.object)? + 1 + display_width(&e.property)),
        Expr::Lit(e) => Some(match &e.value {
            LiteralValue::Number(v)
            | LiteralValue::Long(v)
            | LiteralValue::Hex(v)
            | LiteralValue::HexLong(v) => v.len(),
            LiteralValue::Float(lit) => format_float_lit(lit).len(),
            LiteralValue::Double(lit) => format_double_lit(lit).len(),
            LiteralValue::String(v) => 2 + display_width(v),
            LiteralValue::Char(v) => 3 + display_width(v),
            LiteralValue::Boolean(v) => v.to_string().len(),
            LiteralValue::Symbol(v) => 1 + display_width(v),
            LiteralValue::Null => "null".len(),
            LiteralValue::NaN => "NaN".len(),
        }),
        _ => None,
    }
}
