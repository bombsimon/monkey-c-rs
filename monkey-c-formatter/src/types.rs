//! Type annotations, including inline dictionary and interface types.

use monkey_c_parser::ast::{
    DictTypeEntry, DictTypeKey, InterfaceMember, Separated, Span, Type, TypeKind,
};

use crate::Formatter;
use crate::doc::Doc;

impl Formatter {
    pub(crate) fn type_to_doc(&self, ty: &Type) -> Doc {
        let leading = self.drain_leading_doc(ty.span.start);
        let inner = self.type_inner_to_doc(ty);

        Doc::concat(vec![leading, inner])
    }

    fn type_inner_to_doc(&self, ty: &Type) -> Doc {
        let suffix = if ty.optional { "?" } else { "" };
        let base = match &ty.kind {
            TypeKind::Named {
                ident,
                generic_params,
            } => {
                if generic_params.is_empty() {
                    Doc::text(format!("{ident}{suffix}"))
                } else {
                    let params: Vec<Doc> = generic_params
                        .iter()
                        .enumerate()
                        .flat_map(|(i, p)| {
                            if i > 0 {
                                vec![Doc::text(", "), self.type_to_doc(p)]
                            } else {
                                vec![self.type_to_doc(p)]
                            }
                        })
                        .collect();

                    Doc::concat(vec![
                        Doc::text(format!("{ident}<")),
                        Doc::Concat(params),
                        Doc::text(format!(">{suffix}")),
                    ])
                }
            }
            TypeKind::Dict { entries, body_span } => {
                self.inline_dict_type_to_doc(entries, *body_span, suffix)
            }
            TypeKind::Interface { members, body_span } => {
                self.interface_type_to_doc(members, *body_span, suffix)
            }
            TypeKind::Tuple { elements } => {
                let mut parts = vec![Doc::text("[")];
                for (i, el) in elements.iter().enumerate() {
                    if i > 0 {
                        parts.push(Doc::text(", "));
                    }

                    parts.push(self.type_to_doc(el));
                }

                parts.push(Doc::text(format!("]{suffix}")));

                Doc::Concat(parts)
            }
            TypeKind::Method {
                name,
                args,
                returns,
            } => {
                let mut parts = vec![Doc::text(name), self.parameter_list_to_doc(args)];

                if let Some(ret) = returns {
                    parts.push(Doc::text(" as "));
                    parts.push(self.type_to_doc(ret));
                }

                if !suffix.is_empty() {
                    parts.push(Doc::text(suffix.to_string()));
                }

                Doc::Concat(parts)
            }
            TypeKind::Group(group) => Doc::concat(vec![
                Doc::text("("),
                self.type_to_doc(&group.inner),
                Doc::text(format!("){suffix}")),
            ]),
        };

        if ty.alternatives.is_empty() {
            return base;
        }

        let mut parts = vec![base];
        for alternative in &ty.alternatives {
            parts.push(Doc::text(format!(
                " {} ",
                alternative.separator.node.as_str()
            )));
            parts.push(self.type_to_doc(&alternative.type_));
        }

        Doc::Concat(parts)
    }

    fn inline_dict_type_to_doc(
        &self,
        entries: &Separated<DictTypeEntry>,
        body_span: Span,
        suffix: &str,
    ) -> Doc {
        let close = body_span.end - 1;
        let items = entries
            .iter()
            .enumerate()
            .map(|(i, entry)| {
                let leading = self.drain_leading_doc(entry.span.start);
                let key = match &entry.key {
                    DictTypeKey::Symbol(s) => format!(":{s}"),
                    DictTypeKey::String(s) => format!("\"{s}\""),
                };
                let content = Doc::concat(vec![
                    leading,
                    Doc::text(key),
                    Doc::text(" as "),
                    self.type_to_doc(&entry.value_type),
                ]);
                let next_entry_start = entries.get(i + 1).map(|next| next.span.start);

                self.list_item(
                    content,
                    entry.span.end,
                    entries.comma_after(i),
                    next_entry_start,
                    close,
                )
            })
            .collect();

        self.format_list(
            "{",
            &format!("}}{suffix}"),
            items,
            self.drain_dangling_comments_doc(close),
            entries.has_trailing_comma(),
            Doc::Empty,
            false,
        )
    }

    fn interface_type_to_doc(
        &self,
        members: &[InterfaceMember],
        body_span: Span,
        suffix: &str,
    ) -> Doc {
        let has_content = !members.is_empty()
            || self
                .comment_cursor
                .borrow()
                .has_comment_in(body_span.start + 1, body_span.end);

        if !has_content {
            return Doc::text(format!("interface {{}}{suffix}"));
        }

        // Place same-line comment directly after `{`, not inside the indent.
        let after_open = self.drain_after_open_brace(body_span.start, body_span.end);
        let mut inner = Vec::new();

        let mut prev_end: Option<usize> = None;

        for member in members {
            let member_span = match member {
                InterfaceMember::Function(m) => m.span,
                InterfaceMember::Variable(v) => v.span,
            };

            let eff_start = self.effective_start(member_span);
            if let Some(pe) = prev_end {
                inner.push(self.gap_between_positions(pe, eff_start));
            }

            inner.push(self.interface_member_to_doc(member));
            prev_end = Some(self.effective_end(member_span));
        }

        inner.push(self.drain_remaining_comments(prev_end, body_span.end));

        Doc::bracketed(
            Doc::text("interface {"),
            after_open,
            Doc::concat(inner),
            Doc::text(format!("}}{suffix}")),
        )
    }

    fn interface_member_to_doc(&self, member: &InterfaceMember) -> Doc {
        let span = match member {
            InterfaceMember::Function(m) => m.span,
            InterfaceMember::Variable(v) => v.span,
        };

        // Drain leading before building body — body construction calls type_to_doc
        // which also drains, so leading must come first.
        let leading = self.drain_leading_doc(span.start);

        let body = match member {
            InterfaceMember::Function(m) => {
                let mut parts = vec![
                    Doc::text("function "),
                    self.name_to_doc(&m.name),
                    self.parameter_list_to_doc(&m.args),
                ];
                if let Some(ret) = &m.returns {
                    parts.push(Doc::text(" "));
                    parts.push(self.drain_leading_doc(m.as_kw_start.unwrap_or(ret.span.start)));
                    parts.push(Doc::text("as "));
                    parts.push(self.type_to_doc(ret));
                }

                parts.push(Doc::text(";"));
                Doc::Concat(parts)
            }
            InterfaceMember::Variable(v) => {
                let mut parts = vec![Doc::text("var "), self.name_to_doc(&v.name)];
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(v.as_kw_start));
                parts.push(Doc::text("as "));
                parts.push(self.type_to_doc(&v.type_));
                parts.push(Doc::text(";"));
                Doc::Concat(parts)
            }
        };

        let trailing = self.drain_trailing_doc(span.end, usize::MAX);

        match (&leading, &trailing) {
            (Doc::Empty, Doc::Empty) => body,
            _ => Doc::concat(vec![leading, body, trailing]),
        }
    }
}
