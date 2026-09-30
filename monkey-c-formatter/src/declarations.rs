//! Top-level and class-level declarations: imports, modules, classes, functions, enums, variables
//! and annotations.

use monkey_c_parser::ast::{
    AnnotationEntry, Ast, Binding, ConstDecl, EnumDecl, EnumVariant, FunctionDecl, Modifiers,
    Separated, Span, VarDecl, Visibility,
};

use crate::Formatter;
use crate::doc::{Doc, display_width};

impl Formatter {
    pub(crate) fn ast_to_doc(&self, ast: &Ast) -> Doc {
        match ast {
            Ast::Document(nodes, span) => self.decls_to_doc(nodes, *span),
            Ast::Import(decl) => {
                let mut parts = vec![Doc::text("import "), self.at(&decl.name)];
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Using(decl) => {
                let mut parts = vec![Doc::text("using "), self.at(&decl.name)];
                if let Some(alias) = &decl.alias {
                    parts.push(Doc::text(" "));
                    parts.push(self.drain_leading_doc(decl.as_kw_start.unwrap_or(decl.span.end)));
                    parts.push(Doc::text("as "));
                    parts.push(self.at(alias));
                }
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Typedef(decl) => {
                let mut parts = vec![Doc::text("typedef ")];
                parts.push(self.at(&decl.name));
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(decl.as_kw_start));
                parts.push(Doc::text("as "));
                parts.push(self.type_to_doc(&decl.type_));
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Module(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "module");
                header_parts.push(self.at(&decl.name));
                header_parts.push(Doc::text(" "));
                let header = Doc::concat(header_parts);
                let before_brace = self.drain_leading_doc(decl.brace_start);
                let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
                let inner = self.decls_to_doc(&decl.body, decl.span);

                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![
                        header,
                        before_brace,
                        Doc::text("{"),
                        after_open,
                        Doc::text("}"),
                    ]);
                }

                Doc::concat(vec![
                    header,
                    before_brace,
                    Doc::text("{"),
                    after_open,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Class(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "class");
                header_parts.push(self.at(&decl.name));
                if let Some(extends) = &decl.extends {
                    header_parts.push(Doc::text(" "));
                    header_parts.push(
                        self.drain_leading_doc(decl.extends_kw_start.unwrap_or(decl.brace_start)),
                    );
                    header_parts.push(Doc::text("extends "));
                    header_parts.push(self.at(extends));
                }
                header_parts.push(Doc::text(" "));
                let header = Doc::Concat(header_parts);
                let before_brace = self.drain_leading_doc(decl.brace_start);
                let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
                let inner = self.decls_to_doc(&decl.body, decl.span);

                if decl.body.is_empty() && matches!(inner, Doc::Empty) {
                    return Doc::concat(vec![
                        header,
                        before_brace,
                        Doc::text("{"),
                        after_open,
                        Doc::text("}"),
                    ]);
                }

                Doc::concat(vec![
                    header,
                    before_brace,
                    Doc::text("{"),
                    after_open,
                    Doc::Indent(vec![Doc::HardLine, inner]),
                    Doc::HardLine,
                    Doc::text("}"),
                ])
            }
            Ast::Function(decl) => self.function_to_doc(decl),
            Ast::Enum(decl) => self.enum_to_doc(decl),
            Ast::Variable(var_stmt) => self.var_stmt_to_doc(var_stmt),
            Ast::Const(decl) => self.const_decl_to_doc(decl),
            Ast::Annotation(entries, span) => self.annotation_to_doc(entries, *span),
            Ast::Eof => Doc::Empty,
        }
    }

    /// A `//` comment runs to the end of the line, so an annotation group holding one is broken
    /// with each entry on its own line. Otherwise the entry after it would be commented out.
    fn annotation_to_doc(&self, entries: &[AnnotationEntry], span: Span) -> Doc {
        let inner_start = span.start + 1;
        let inner_end = span.end.saturating_sub(1);
        let multiline = self
            .comment_cursor
            .borrow()
            .has_line_comment_in(inner_start, inner_end);
        let separator = if multiline {
            Doc::HardLine
        } else {
            Doc::text(" ")
        };

        if entries.is_empty() {
            let comments = self.comment_cursor.borrow_mut().drain_before(inner_end);
            if comments.is_empty() {
                return Doc::text("()");
            }

            let mut inner = Vec::new();
            for (i, comment) in comments.iter().enumerate() {
                if i > 0 {
                    inner.push(separator.clone());
                }

                inner.push(self.comment_to_doc(comment));
            }

            return wrap_annotation(inner, multiline);
        }

        let mut inner = Vec::new();
        for (i, entry) in entries.iter().enumerate() {
            let next_entry = entries.get(i + 1);

            if i > 0 {
                inner.push(separator.clone());
            }

            inner.push(self.drain_leading_doc(entry.span.start));
            inner.push(Doc::text(format!(":{}", entry.name)));

            if !entry.args.is_empty() {
                inner.push(Doc::text("("));
                for (j, arg) in entry.args.iter().enumerate() {
                    if j > 0 {
                        inner.push(Doc::text(", "));
                    }

                    inner.push(self.expr_with_leading(arg));
                }

                inner.push(Doc::text(")"));
            }

            if next_entry.is_some_and(|next| next.preceded_by_comma) {
                inner.push(Doc::text(","));
            }

            // Bounded so comments belonging to the next entry, or to the annotated declaration
            // after the closing `)`, are not pulled in here.
            let max_pos = next_entry.map_or(span.end, |next| next.span.start);
            inner.push(self.drain_trailing_doc_bounded(entry.span.end, max_pos));
        }

        wrap_annotation(inner, multiline)
    }

    /// Render a sequence of declarations interleaved with standalone comments,
    /// preserving blank lines between adjacent items.
    fn decls_to_doc(&self, decls: &[Ast], container: Span) -> Doc {
        let has_content = !decls.is_empty()
            || self
                .comment_cursor
                .borrow()
                .has_comment_in(container.start, container.end);
        if !has_content {
            return Doc::Empty;
        }

        let mut docs = Vec::new();
        let mut prev_end: Option<usize> = None;
        let mut prev_is_block_decl = false;

        for (idx, decl) in decls.iter().enumerate() {
            let Some(decl_span) = decl.span() else {
                continue;
            };
            let decl_span = *decl_span;

            let eff_start = self.effective_start(decl_span);
            let is_block_decl = matches!(decl, Ast::Function(_) | Ast::Class(_) | Ast::Module(_));

            if let Some(pe) = prev_end {
                let gap = self.gap_between_positions(pe, eff_start);
                docs.push(if prev_is_block_decl && matches!(gap, Doc::HardLine) {
                    Doc::BlankLine
                } else {
                    gap
                });
            }

            // Bound the trailing drain at the next decl's start so that same-line
            // comments following a decl (e.g. after an annotation or var with `;`)
            // are not stolen by the current decl's trailing drain.
            let next_decl_start = decls[idx + 1..]
                .iter()
                .find_map(|d| d.span().map(|s| s.start))
                .unwrap_or(container.end);

            docs.push(self.drain_leading_doc(decl_span.start));
            docs.push(self.ast_to_doc(decl));
            docs.push(self.drain_trailing_doc_bounded(decl_span.end, next_decl_start));

            prev_end = Some(self.effective_end(decl_span));
            prev_is_block_decl = is_block_decl;
        }

        // Drain any remaining comments inside the container (after last decl).
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(container.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            if let Some(pe) = prev_end {
                docs.push(self.gap_between_positions(pe, start));
            }

            docs.push(self.drain_dangling_comments_doc(container.end));
        }

        Doc::Concat(docs)
    }

    fn enum_to_doc(&self, decl: &EnumDecl) -> Doc {
        let mut prefix_parts = self.decl_keyword(&decl.modifiers, "enum");
        if let Some(name) = &decl.name {
            prefix_parts.push(self.at(name));
            prefix_parts.push(Doc::text(" "));
        }

        let prefix = Doc::concat(prefix_parts);

        if decl.variants.is_empty() {
            let before_brace = self.drain_leading_doc(decl.brace_start);
            let after_open = self.drain_after_open_brace(decl.brace_start, decl.span.end);
            let remaining_start = self
                .comment_cursor
                .borrow()
                .peek_before(decl.span.end)
                .map(|c| c.span.start);

            if remaining_start.is_none()
                && matches!(before_brace, Doc::Empty)
                && matches!(after_open, Doc::Empty)
            {
                return Doc::concat(vec![prefix.clone(), Doc::text("{"), Doc::text("}")]);
            }

            let mut inner = Vec::new();
            if remaining_start.is_some() {
                inner.push(self.drain_dangling_comments_doc(decl.span.end));
            }

            if inner.is_empty()
                && matches!(before_brace, Doc::Empty)
                && matches!(after_open, Doc::Empty)
            {
                return Doc::concat(vec![prefix.clone(), Doc::text("{"), Doc::text("}")]);
            }

            return Doc::concat(vec![
                prefix.clone(),
                before_brace,
                Doc::text("{"),
                after_open,
                Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
                Doc::HardLine,
                Doc::text("}"),
            ]);
        }

        // Bounded by the first variant so comments after it on a one-line enum stay with it.
        let before_brace = self.drain_leading_doc(decl.brace_start);
        let after_open = self.drain_after_open_brace(decl.brace_start, decl.variants[0].span.start);
        let header = Doc::concat(vec![
            prefix.clone(),
            before_brace,
            Doc::text("{"),
            after_open,
        ]);

        let last_idx = decl.variants.len() - 1;
        let mut inner = Vec::new();
        let mut prev_end: Option<usize> = None;

        let push_gap = |inner: &mut Vec<Doc>, prev_end: Option<usize>, next_start: usize| {
            let Some(pe) = prev_end else { return };
            inner.push(self.gap_between_positions(pe, next_start));
        };

        let name_pads = if self.align_pairs {
            enum_variant_name_pads(&decl.variants)
        } else {
            vec![0; decl.variants.len()]
        };

        for (i, v) in decl.variants.iter().enumerate() {
            let eff_start = self.effective_start(v.span);
            push_gap(&mut inner, prev_end, eff_start);

            let mut parts = Vec::new();
            parts.push(self.drain_leading_doc(v.span.start));
            parts.push(Doc::text(&v.name));

            if let Some(value) = &v.value {
                let pad = name_pads[i].saturating_sub(display_width(&v.name));
                if pad > 0 {
                    parts.push(Doc::text(" ".repeat(pad)));
                }
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(v.assign_kw_start.unwrap_or(value.span().start)));
                parts.push(Doc::text("= "));
                parts.push(self.expr_with_leading(value));
            }

            // Bounded by the closing `}` so a comment after it on a one-line enum stays there.
            let item = self.list_item(
                Doc::Concat(parts),
                v.span.end,
                decl.variants.comma_after(i),
                decl.variants.get(i + 1).map(|next| next.span.start),
                decl.span.end,
            );
            let mut parts = vec![item.content, item.before_separator];
            if i != last_idx || decl.variants.has_trailing_comma() {
                parts.push(Doc::text(","));
            }

            parts.push(item.trailing);
            inner.push(Doc::Concat(parts));
            prev_end = Some(self.effective_end(v.span));
        }

        // Drain comments after last variant before `}`.
        let remaining_start = self
            .comment_cursor
            .borrow()
            .peek_before(decl.span.end)
            .map(|c| c.span.start);
        if let Some(start) = remaining_start {
            push_gap(&mut inner, prev_end, start);
            inner.push(self.drain_dangling_comments_doc(decl.span.end));
        }

        Doc::concat(vec![
            header,
            Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
            Doc::HardLine,
            Doc::text("}"),
        ])
    }

    fn function_to_doc(&self, decl: &FunctionDecl) -> Doc {
        let mut parts = self.decl_keyword(&decl.modifiers, "function");
        parts.push(self.at(&decl.name));

        parts.push(self.parameter_list_to_doc(&decl.parameters));

        if let Some(ret) = &decl.returns {
            parts.push(Doc::text(" "));
            parts.push(self.drain_leading_doc(decl.as_kw_start.unwrap_or(ret.span.start)));
            parts.push(Doc::text("as "));
            parts.push(self.type_to_doc(ret));
        }

        match &decl.body {
            None => parts.push(Doc::text(";")),
            Some(body) => {
                parts.push(Doc::text(" "));
                parts.push(self.block_body_to_doc(body));
            }
        }

        Doc::Concat(parts)
    }

    pub(crate) fn var_stmt_to_doc(&self, var_decl: &VarDecl) -> Doc {
        if var_decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                &var_decl.modifiers,
                "var",
                &var_decl.bindings,
                var_decl.semi_pos,
            );
        }

        let mut parts = vec![self.var_decl_to_doc(var_decl)];
        self.push_before_semi(&mut parts, var_decl.semi_pos);

        Doc::Concat(parts)
    }

    fn const_decl_to_doc(&self, decl: &ConstDecl) -> Doc {
        if decl.bindings.len() >= 2 {
            return self.wrapped_bindings_decl(
                &decl.modifiers,
                "const",
                &decl.bindings,
                decl.semi_pos,
            );
        }

        let mut parts = self.decl_keyword(&decl.modifiers, "const");
        self.push_bindings(&mut parts, &decl.bindings);
        self.push_before_semi(&mut parts, decl.semi_pos);

        Doc::Concat(parts)
    }

    pub(crate) fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts = self.decl_keyword(&var.modifiers, "var");
        self.push_bindings(&mut parts, &var.bindings);

        Doc::Concat(parts)
    }

    fn wrapped_bindings_decl(
        &self,
        modifiers: &Modifiers,
        keyword: &str,
        bindings: &Separated<Binding>,
        semi_pos: usize,
    ) -> Doc {
        let mut parts = self.modifiers_to_doc(modifiers);

        let mut indented = vec![Doc::Line];
        for (i, binding) in bindings.iter().enumerate() {
            let content = self.binding_to_doc(binding);
            let next_start = bindings.get(i + 1).map(|next| next.span.start);
            // The last binding leaves its comments to the `;`.
            let item = self.list_item(
                content,
                binding.span.end,
                bindings.comma_after(i),
                next_start,
                binding.span.end,
            );
            indented.push(item.content);
            indented.push(item.before_separator);

            if next_start.is_none() {
                indented.push(item.trailing);
                continue;
            }

            indented.push(Doc::text(","));
            indented.push(item.trailing);
            indented.push(if item.trailing_is_line_comment {
                Doc::HardLine
            } else {
                Doc::Line
            });
        }

        // The `;` sits inside the group so a declaration that only overflows
        // by its terminator still breaks.
        let mut group = vec![Doc::text(keyword.to_string()), Doc::Indent(indented)];
        self.push_before_semi(&mut group, semi_pos);
        parts.push(Doc::group(group));

        Doc::Concat(parts)
    }

    fn decl_keyword(&self, modifiers: &Modifiers, keyword: &str) -> Vec<Doc> {
        let mut parts = self.modifiers_to_doc(modifiers);
        parts.push(Doc::text(format!("{keyword} ")));

        parts
    }

    fn push_bindings(&self, parts: &mut Vec<Doc>, bindings: &Separated<Binding>) {
        for (i, binding) in bindings.iter().enumerate() {
            let content = self.binding_to_doc(binding);
            let next_start = bindings.get(i + 1).map(|next| next.span.start);

            // The last binding leaves its comments to what follows it, such as the `;`.
            let item = self.list_item(
                content,
                binding.span.end,
                bindings.comma_after(i),
                next_start,
                binding.span.end,
            );
            parts.push(item.content);
            parts.push(item.before_separator);

            if next_start.is_none() {
                continue;
            }

            parts.push(Doc::text(","));
            parts.push(item.trailing);
            parts.push(if item.trailing_is_line_comment {
                Doc::HardLine
            } else {
                Doc::text(" ")
            });
        }
    }

    fn binding_to_doc(&self, b: &Binding) -> Doc {
        let mut parts = vec![
            self.drain_leading_doc(b.name.start()),
            Doc::text(&b.name.node),
        ];

        if let Some(ty) = &b.type_ {
            parts.push(Doc::text(" "));
            parts.push(self.drain_leading_doc(b.as_kw_start.unwrap_or(ty.span.start)));
            parts.push(Doc::text("as "));
            parts.push(self.type_to_doc(ty));
        }

        if let Some(init) = &b.initializer {
            let before_assign = b.type_.as_ref().map_or(b.name.span.end, |ty| ty.span.end);
            let assign_pos = b.assign_kw_start.unwrap_or(init.span().start);
            parts.push(self.token_then_expr(before_assign, "=", assign_pos, init));
        }

        Doc::Concat(parts)
    }

    fn modifiers_to_doc(&self, modifiers: &Modifiers) -> Vec<Doc> {
        let mut keywords = Vec::new();
        if let Some(visibility) = &modifiers.visibility {
            let keyword = match visibility.node {
                Visibility::Private => "private ",
                Visibility::Protected => "protected ",
                Visibility::Hidden => "hidden ",
                Visibility::Public => "public ",
            };
            keywords.push((visibility.start(), keyword));
        }

        if let Some(static_kw_start) = modifiers.static_kw_start {
            keywords.push((static_kw_start, "static "));
        }

        keywords.sort_by_key(|(position, _)| *position);

        keywords
            .into_iter()
            .flat_map(|(position, keyword)| [self.drain_leading_doc(position), Doc::text(keyword)])
            .collect()
    }
}

fn wrap_annotation(inner: Vec<Doc>, multiline: bool) -> Doc {
    if !multiline {
        return Doc::concat(vec![Doc::text("("), Doc::Concat(inner), Doc::text(")")]);
    }

    Doc::concat(vec![
        Doc::text("("),
        Doc::Indent(vec![Doc::HardLine, Doc::Concat(inner)]),
        Doc::HardLine,
        Doc::text(")"),
    ])
}

fn enum_variant_name_pads(variants: &[EnumVariant]) -> Vec<usize> {
    let mut out = vec![0; variants.len()];
    let mut i = 0;
    while i < variants.len() {
        if variants[i].value.is_none() {
            i += 1;
            continue;
        }

        let run_start = i;
        let mut max_name = 0;
        while i < variants.len() && variants[i].value.is_some() {
            max_name = max_name.max(display_width(&variants[i].name));
            i += 1;
        }

        if i - run_start >= 2 {
            for slot in out.iter_mut().take(i).skip(run_start) {
                *slot = max_name;
            }
        }
    }
    out
}
