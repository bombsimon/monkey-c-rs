//! Type annotations, including inline dictionary and interface types.

use monkey_c_parser::ast::{
    DictTypeEntry, DictTypeKey, InterfaceMember, Parens, Separated, Span, Type, TypeKind,
    UnionSeparator,
};
use monkey_c_parser::lexer::Lexer;

use crate::Formatter;
use crate::doc::Doc;
use crate::options::{self, Nullable, UnionStyle};

/// What can follow a type where it's written, which decides the rewrites that would otherwise make
/// it read differently. Without parentheses, a `?` or a union after `Method() as Return` belongs to
/// `Return`, and the target of a cast is read with a lookahead that can stop before `?` or `|`.
#[derive(Debug, Clone, Copy)]
struct TypeSlot {
    cast: bool,
    question_allowed: bool,
    in_union: bool,
}

impl TypeSlot {
    const ANNOTATION: Self = Self {
        cast: false,
        question_allowed: true,
        in_union: false,
    };
}

impl Formatter {
    pub(crate) fn type_to_doc(&self, ty: &Type) -> Doc {
        self.type_in_slot(ty, TypeSlot::ANNOTATION)
    }

    /// The target of an `as` cast, where `rest` is the source from the token after it.
    pub(crate) fn cast_type_to_doc(&self, ty: &Type, rest: &str) -> Doc {
        let slot = TypeSlot {
            cast: true,
            question_allowed: ends_cast_before_question(rest),
            in_union: false,
        };

        self.type_in_slot(ty, slot)
    }

    fn type_in_slot(&self, ty: &Type, slot: TypeSlot) -> Doc {
        let leading = self.drain_leading_doc(ty.span.start);
        let inner = self.type_inner_to_doc(ty, slot);

        Doc::concat(vec![leading, inner])
    }

    fn type_inner_to_doc(&self, ty: &Type, slot: TypeSlot) -> Doc {
        if let Some(member) = self.nullable_shorthand(ty, slot) {
            return self.kind_to_doc(&member.kind, true, slot);
        }

        // Every `?` in the union becomes one `or Null`, or none when a member already is `Null`.
        let expands_optional = self.nullable == Nullable::Union;
        let members = || std::iter::once(ty).chain(ty.alternatives.iter().map(|a| &a.type_));
        let adds_null =
            expands_optional && members().any(|member| member.optional) && !members().any(is_null);
        let keeps_optional = |member: &Type| member.optional && !expands_optional;

        let in_union = slot.in_union || adds_null || !ty.alternatives.is_empty();
        let base = self.kind_to_doc(&ty.kind, keeps_optional(ty), TypeSlot { in_union, ..slot });

        if !adds_null && ty.alternatives.is_empty() {
            return base;
        }

        let member_slot = TypeSlot {
            in_union: true,
            ..slot
        };
        let mut parts = vec![base];

        if adds_null {
            let separator = self.union_separator(UnionSeparator::Or, None, slot);
            parts.push(Doc::text(format!(" {separator} Null")));
        }

        for alternative in &ty.alternatives {
            let separator = self.union_separator(
                alternative.separator.node,
                Some(&alternative.type_.kind),
                slot,
            );
            parts.push(Doc::text(format!(" {separator} ")));

            // A member is a single type, so this is all `type_in_slot` would do, minus a `Null`
            // of its own.
            let member = &alternative.type_;
            parts.push(self.drain_leading_doc(member.span.start));
            parts.push(self.kind_to_doc(&member.kind, keeps_optional(member), member_slot));
        }

        Doc::Concat(parts)
    }

    /// The member to write with a `?` in place of a union of it and `Null`, when that reads the
    /// same.
    fn nullable_shorthand<'a>(&self, ty: &'a Type, slot: TypeSlot) -> Option<&'a Type> {
        if self.nullable != Nullable::Shorthand || !slot.question_allowed {
            return None;
        }

        let [alternative] = ty.alternatives.as_slice() else {
            return None;
        };

        let member = match (is_null(ty), is_null(&alternative.type_)) {
            (false, true) => ty,
            (true, false) => &alternative.type_,
            (true, true) | (false, false) => return None,
        };

        // `Null or Method() as Boolean` has no shorthand without parentheses, since the `?` would
        // go to `Boolean`. A comment could end up on the other side of the type it was written by.
        if is_method_with_return(&member.kind) || self.has_written_comments_in(ty.span) {
            return None;
        }

        Some(member)
    }

    /// `next` is the member after the separator, when there is one in the source.
    fn union_separator(
        &self,
        written: UnionSeparator,
        next: Option<&TypeKind>,
        slot: TypeSlot,
    ) -> &'static str {
        // A cast only reads `|` as a union when a type name, `{` or `[` follows it.
        let pipe_reads_as_union = !slot.cast
            || next.is_none_or(|kind| {
                matches!(
                    kind,
                    TypeKind::Named { .. }
                        | TypeKind::Method { .. }
                        | TypeKind::Dict { .. }
                        | TypeKind::Tuple { .. }
                )
            });

        match self.union_style {
            UnionStyle::Or => UnionSeparator::Or.as_str(),
            UnionStyle::Pipe if pipe_reads_as_union => UnionSeparator::Pipe.as_str(),
            UnionStyle::Pipe => UnionSeparator::Or.as_str(),
            UnionStyle::Preserve => written.as_str(),
        }
    }

    fn kind_to_doc(&self, kind: &TypeKind, optional: bool, slot: TypeSlot) -> Doc {
        let suffix = if optional { "?" } else { "" };

        match kind {
            TypeKind::Named {
                ident,
                generic_params,
            } => {
                if generic_params.is_empty() {
                    return Doc::text(format!("{ident}{suffix}"));
                }

                Doc::concat(vec![
                    Doc::text(format!("{ident}<")),
                    self.type_list_to_doc(generic_params),
                    Doc::text(format!(">{suffix}")),
                ])
            }
            TypeKind::Dict { entries, body_span } => {
                self.inline_dict_type_to_doc(entries, *body_span, suffix)
            }
            TypeKind::Interface { members, body_span } => {
                self.interface_type_to_doc(members, *body_span, suffix)
            }
            TypeKind::Tuple { elements } => Doc::concat(vec![
                Doc::text("["),
                self.type_list_to_doc(elements),
                Doc::text(format!("]{suffix}")),
            ]),
            TypeKind::Method {
                name,
                args,
                returns,
            } => {
                let mut parts = vec![Doc::text(name), self.parameter_list_to_doc(args)];

                // Whatever follows the method type follows its return type too.
                if let Some(ret) = returns {
                    parts.push(Doc::text(" as "));
                    parts.push(self.type_in_slot(ret, slot));
                }

                if !suffix.is_empty() {
                    parts.push(Doc::text(suffix.to_string()));
                }

                Doc::Concat(parts)
            }
            TypeKind::Group(group) => {
                if self.drops_type_parens(group, optional, slot) {
                    return self.kind_to_doc(&group.inner.kind, optional, slot);
                }

                Doc::concat(vec![
                    Doc::text("("),
                    self.type_to_doc(&group.inner),
                    Doc::text(format!("){suffix}")),
                ])
            }
        }
    }

    fn type_list_to_doc(&self, types: &[Type]) -> Doc {
        let mut parts = Vec::new();
        for (i, ty) in types.iter().enumerate() {
            if i > 0 {
                parts.push(Doc::text(", "));
            }

            parts.push(self.type_to_doc(ty));
        }

        Doc::concat(parts)
    }

    /// The grammar only takes parentheses around a `Method(…)` type, and they're only needed when
    /// something after them would otherwise go to its return type.
    fn drops_type_parens(&self, group: &Parens<Box<Type>>, optional: bool, slot: TypeSlot) -> bool {
        let inner = &group.inner;
        let TypeKind::Method { returns, .. } = &inner.kind else {
            return false;
        };

        if self.parens == options::Parens::Preserve
            || inner.optional
            || !inner.alternatives.is_empty()
            || self.has_written_comments_in(Span {
                start: group.open,
                end: group.close,
            })
        {
            return false;
        }

        returns.is_none() || !(optional || slot.in_union || slot.cast)
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

        let after_open = self.drain_after_open_brace(body_span.start, body_span.end);
        let mut inner = Vec::new();

        let mut prev_end: Option<usize> = None;

        for member in members {
            let member_span = member.span();
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
        let span = member.span();
        let leading = self.drain_leading_doc(span.start);

        let body = match member {
            InterfaceMember::Function(m) => {
                let mut parts = vec![
                    Doc::text("function "),
                    self.name_to_doc(&m.name),
                    self.parameter_list_to_doc(&m.args),
                ];
                if let Some(ret) = &m.returns {
                    parts.push(self.type_annotation_to_doc(m.as_kw_start, ret));
                }

                parts.push(Doc::text(";"));
                Doc::Concat(parts)
            }
            InterfaceMember::Variable(v) => {
                let mut parts = vec![Doc::text("var "), self.name_to_doc(&v.name)];
                parts.push(self.type_annotation_to_doc(Some(v.as_kw_start), &v.type_));
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

/// Whether `ty` is a plain `Null`, including the fully qualified forms.
fn is_null(ty: &Type) -> bool {
    let TypeKind::Named {
        ident,
        generic_params,
    } = &ty.kind
    else {
        return false;
    };

    generic_params.is_empty() && matches!(ident.as_str(), "Null" | "Lang.Null" | "Toybox.Lang.Null")
}

fn is_method_with_return(kind: &TypeKind) -> bool {
    matches!(
        kind,
        TypeKind::Method {
            returns: Some(_),
            ..
        }
    )
}

/// Whether a `?` added to the type of a cast would still be read as nullable, given `rest`, the
/// source after the type. The parser decides by the next token, so comments don't count.
fn ends_cast_before_question(rest: &str) -> bool {
    Lexer::new(rest)
        .peek_token_skip_comments()
        .1
        .follows_nullable_cast()
}
