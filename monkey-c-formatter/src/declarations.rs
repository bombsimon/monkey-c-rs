//! Top-level and class-level declarations: imports, modules, classes, functions, enums, variables
//! and annotations.

use monkey_c_parser::ast::{
    AnnotationEntry, Ast, Binding, ConstDecl, EnumDecl, EnumVariant, FunctionDecl, Modifiers,
    Separated, Span, VarDecl, Visibility,
};

use crate::Formatter;
use crate::doc::{Doc, display_width};
use crate::options::{AnnotationSeparator, ModifierOrder};
use crate::verbatim::FmtDirective;

impl Formatter {
    pub(crate) fn ast_to_doc(&self, ast: &Ast) -> Doc {
        match ast {
            Ast::Document(nodes, span) => self.decls_to_doc(nodes, *span),
            Ast::Import(decl) => {
                let mut parts = vec![Doc::text("import "), self.name_to_doc(&decl.name)];
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Using(decl) => {
                let mut parts = vec![Doc::text("using "), self.name_to_doc(&decl.name)];
                if let Some(alias) = &decl.alias {
                    parts.push(Doc::text(" "));
                    parts.push(self.drain_leading_doc(decl.as_kw_start.unwrap_or(decl.span.end)));
                    parts.push(Doc::text("as "));
                    parts.push(self.name_to_doc(alias));
                }
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Typedef(decl) => {
                let mut parts = vec![Doc::text("typedef ")];
                parts.push(self.name_to_doc(&decl.name));
                parts.push(Doc::text(" "));
                parts.push(self.drain_leading_doc(decl.as_kw_start));
                parts.push(Doc::text("as "));
                parts.push(self.type_to_doc(&decl.type_));
                self.push_before_semi(&mut parts, decl.span.end - 1);
                Doc::Concat(parts)
            }
            Ast::Module(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "module");
                header_parts.push(self.name_to_doc(&decl.name));
                header_parts.push(Doc::text(" "));
                header_parts.push(self.declaration_block_to_doc(
                    decl.brace_start,
                    decl.span,
                    &decl.body,
                ));

                Doc::concat(header_parts)
            }
            Ast::Class(decl) => {
                let mut header_parts = self.decl_keyword(&decl.modifiers, "class");
                header_parts.push(self.name_to_doc(&decl.name));
                if let Some(extends) = &decl.extends {
                    header_parts.push(Doc::text(" "));
                    header_parts.push(
                        self.drain_leading_doc(decl.extends_kw_start.unwrap_or(decl.brace_start)),
                    );
                    header_parts.push(Doc::text("extends "));
                    header_parts.push(self.name_to_doc(extends));
                }
                header_parts.push(Doc::text(" "));
                header_parts.push(self.declaration_block_to_doc(
                    decl.brace_start,
                    decl.span,
                    &decl.body,
                ));

                Doc::concat(header_parts)
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

            if next_entry.is_some_and(|next| self.comma_before_annotation(next)) {
                inner.push(Doc::text(","));
            }

            // Bounded so comments belonging to the next entry, or to the annotated declaration
            // after the closing `)`, are not pulled in here.
            let max_pos = next_entry.map_or(span.end, |next| next.span.start);
            inner.push(self.drain_trailing_doc(entry.span.end, max_pos));
        }

        wrap_annotation(inner, multiline)
    }

    fn comma_before_annotation(&self, entry: &AnnotationEntry) -> bool {
        match self.annotation_separator {
            AnnotationSeparator::Space => false,
            AnnotationSeparator::Comma => true,
            AnnotationSeparator::Preserve => entry.preceding_comma_pos.is_some(),
        }
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
        let spans: Vec<Span> = decls.iter().filter_map(Ast::span).copied().collect();
        // The end of the declarations already rendered ahead of the one at hand, as written or
        // as one sorted block of imports.
        let mut rendered_end = 0;
        // Where the run of imports last looked at ends, so one is only considered from its start.
        // A run cut short by a `rafiki: fmt off` region starts again after it.
        let mut import_run_end = 0;

        for (idx, decl) in decls.iter().enumerate() {
            let Some(decl_span) = decl.span() else {
                continue;
            };
            let decl_span = *decl_span;

            if decl_span.start < rendered_end {
                continue;
            }

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

            // `spans` skips the declarations without one, so `idx` can't index it.
            let remaining = spans.partition_point(|span| span.start < decl_span.start);

            if let Some(region) = self.fmt_off_region(&spans[remaining..], container.end) {
                docs.push(self.drain_leading_doc(region.start));
                docs.push(self.verbatim_doc(region));
                prev_end = Some(region.end);
                prev_is_block_decl = false;
                rendered_end = region.end;
                continue;
            }

            let next_decl_start = |after: usize| {
                decls[after + 1..]
                    .iter()
                    .find_map(|d| d.span().map(|s| s.start))
                    .unwrap_or(container.end)
            };

            // An annotation applies to the declaration after it, so that one stays where it is
            // and the imports after it are a run of their own.
            let previous = idx.checked_sub(1).map(|previous| &decls[previous]);
            let annotated = matches!(previous, Some(Ast::Annotation(..)));

            let run = if self.sort_imports && idx >= import_run_end && !annotated {
                let run = self.import_run(&decls[idx..]);
                import_run_end = idx + run.len();

                run
            } else {
                &[]
            };

            let above_start = previous
                .and_then(Ast::span)
                .map_or(container.start, |span| span.end);

            if self.sorts_imports(run, above_start) {
                let last = run.len() - 1;
                let run_end = run[last].span().map_or(decl_span.end, |span| span.end);

                docs.push(self.drain_leading_doc(decl_span.start));
                docs.push(self.sorted_imports_to_doc(run));
                docs.push(self.drain_trailing_doc(run_end, next_decl_start(idx + last)));

                prev_end = Some(self.effective_end(Span {
                    start: decl_span.start,
                    end: run_end,
                }));
                prev_is_block_decl = false;
                rendered_end = run_end;
                continue;
            }

            // Bound the trailing drain at the next decl's start so that same-line
            // comments following a decl (e.g. after an annotation or var with `;`)
            // are not stolen by the current decl's trailing drain.
            docs.push(self.drain_leading_doc(decl_span.start));
            docs.push(self.ast_to_doc(decl));
            docs.push(self.drain_trailing_doc(decl_span.end, next_decl_start(idx)));

            prev_end = Some(self.effective_end(decl_span));
            prev_is_block_decl = is_block_decl;
        }

        // Drain any remaining comments inside the container (after last decl).
        docs.push(self.drain_remaining_comments(prev_end, container.end));

        Doc::concat(docs)
    }

    /// Whether `run` should be written sorted. A comment between the imports, at the end of the
    /// last one's line or right above the first could be about any one of them, so a run with one
    /// is kept in the order written. A comment a blank line above is about the whole block, and
    /// one above `above_start` is about an earlier declaration.
    fn sorts_imports(&self, run: &[Ast], above_start: usize) -> bool {
        let (Some(first), Some(last)) = (
            run.first().and_then(Ast::span),
            run.last().and_then(Ast::span),
        ) else {
            return false;
        };

        if !self.sort_imports || run.len() < 2 {
            return false;
        }

        let line_end = self.source[last.end..]
            .find('\n')
            .map_or(self.source.len(), |offset| last.end + offset);
        let cursor = self.comment_cursor.borrow();

        // `rafiki: fmt` comments are about formatting, not about an import.
        let comment_above = cursor
            .written_in(above_start, first.start)
            .last()
            .is_some_and(|comment| {
                self.line_index.starts_line(comment.span.start as u32)
                    && self.own_line_directive(comment).is_none()
                    && self
                        .line_index
                        .blank_lines_between(comment.span.end as u32, first.start as u32)
                        == 0
            });

        // A directive between the imports has nowhere to go once they're sorted, so it counts too.
        let comment_inside = cursor.written_in(first.start, line_end).next().is_some();

        !comment_above && !comment_inside
    }

    /// The imports at the start of `decls`, up to a `rafiki: fmt off` comment between two of them.
    /// The region it starts is kept as written, and the imports after it are a run of their own.
    fn import_run<'a>(&self, decls: &'a [Ast]) -> &'a [Ast] {
        let len = decls
            .iter()
            .take_while(|decl| import_name(decl).is_some())
            .count();
        let cursor = self.comment_cursor.borrow();
        let turns_off = |previous: &Ast, next: &Ast| {
            let (Some(previous), Some(next)) = (previous.span(), next.span()) else {
                return false;
            };

            cursor
                .written_in(previous.end, next.start)
                .any(|comment| self.own_line_directive(comment) == Some(FmtDirective::Off))
        };

        let end = decls[..len]
            .windows(2)
            .position(|pair| turns_off(&pair[0], &pair[1]))
            .map_or(len, |index| index + 1);

        &decls[..end]
    }

    /// `run` in groups split by blank lines: `using Toybox`, `import Toybox`, then the other
    /// `using`s and `import`s, each sorted by name.
    fn sorted_imports_to_doc(&self, run: &[Ast]) -> Doc {
        let mut sorted: Vec<(ImportGroup, &str, &Ast)> = run
            .iter()
            .filter_map(|decl| {
                let (group, name) = import_name(decl)?;

                Some((group, name, decl))
            })
            .collect();
        sorted.sort_by_key(|(group, name, _)| (*group, *name));

        let mut parts = Vec::new();
        let mut previous_group = None;

        for (group, _, decl) in sorted {
            match previous_group {
                Some(previous) if previous != group => parts.push(Doc::BlankLine),
                Some(_) => parts.push(Doc::HardLine),
                None => {}
            }

            parts.push(self.ast_to_doc(decl));
            previous_group = Some(group);
        }

        Doc::concat(parts)
    }

    fn enum_to_doc(&self, decl: &EnumDecl) -> Doc {
        let mut prefix_parts = self.decl_keyword(&decl.modifiers, "enum");
        if let Some(name) = &decl.name {
            prefix_parts.push(self.name_to_doc(name));
            prefix_parts.push(Doc::text(" "));
        }

        let prefix = Doc::concat(prefix_parts);
        let before_brace = self.drain_leading_doc(decl.brace_start);

        // Bounded by the first variant so comments after it on a one-line enum stay with it.
        let first_variant_start = decl
            .variants
            .first()
            .map_or(decl.span.end, |v| v.span.start);
        let after_open = self.drain_after_open_brace(decl.brace_start, first_variant_start);

        let last_idx = decl.variants.len().saturating_sub(1);
        let mut inner = Vec::new();
        let mut prev_end: Option<usize> = None;

        let name_pads = if self.align_pairs {
            enum_variant_name_pads(&decl.variants)
        } else {
            vec![0; decl.variants.len()]
        };

        for (i, v) in decl.variants.iter().enumerate() {
            if let Some(prev_end) = prev_end {
                inner.push(self.gap_between_positions(prev_end, self.effective_start(v.span)));
            }

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

        inner.push(self.drain_remaining_comments(prev_end, decl.span.end));

        Doc::concat(vec![
            prefix,
            Doc::bracketed(
                Doc::concat(vec![before_brace, Doc::text("{")]),
                after_open,
                Doc::concat(inner),
                Doc::text("}"),
            ),
        ])
    }

    /// The `{ … }` of a module or class, holding its declarations.
    fn declaration_block_to_doc(&self, brace_start: usize, span: Span, body: &[Ast]) -> Doc {
        let before_brace = self.drain_leading_doc(brace_start);
        let after_open = self.drain_after_open_brace(brace_start, span.end);

        Doc::bracketed(
            Doc::concat(vec![before_brace, Doc::text("{")]),
            after_open,
            self.decls_to_doc(body, span),
            Doc::text("}"),
        )
    }

    fn function_to_doc(&self, decl: &FunctionDecl) -> Doc {
        let mut parts = self.decl_keyword(&decl.modifiers, "function");
        parts.push(self.name_to_doc(&decl.name));

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
                parts.push(self.block_to_doc(body));
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
        parts.push(self.bindings_to_doc(&decl.bindings, Doc::text(" ")));
        self.push_before_semi(&mut parts, decl.semi_pos);

        Doc::Concat(parts)
    }

    pub(crate) fn var_decl_to_doc(&self, var: &VarDecl) -> Doc {
        let mut parts = self.decl_keyword(&var.modifiers, "var");
        parts.push(self.bindings_to_doc(&var.bindings, Doc::text(" ")));

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

        let indented = vec![Doc::text(" "), self.bindings_to_doc(bindings, Doc::Line)];

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

    /// The bindings of a `var` or `const` separated by `,` and `separator`, with comments kept on
    /// their side of each comma. The last binding leaves its comments to what follows it, such as
    /// the `;`.
    fn bindings_to_doc(&self, bindings: &Separated<Binding>, separator: Doc) -> Doc {
        let mut parts = Vec::new();
        for (i, binding) in bindings.iter().enumerate() {
            let content = self.binding_to_doc(binding);
            let next_start = bindings.get(i + 1).map(|next| next.span.start);
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
            parts.push(if item.ends_line {
                Doc::HardLine
            } else {
                separator.clone()
            });
        }

        Doc::concat(parts)
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
            parts.push(self.token_then_expr(
                before_assign,
                "=",
                assign_pos,
                self.without_parens(init),
            ));
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

        // A comment between the keywords could be about either, so they stay as written.
        let comment_between = match keywords.as_slice() {
            [(visibility, _), (static_kw, _)] => self.has_written_comments_in(Span {
                start: *visibility.min(static_kw),
                end: *visibility.max(static_kw),
            }),
            _ => false,
        };

        if self.modifier_order == ModifierOrder::Preserve || comment_between {
            keywords.sort_by_key(|(position, _)| *position);
        }

        keywords
            .into_iter()
            .flat_map(|(position, keyword)| [self.drain_leading_doc(position), Doc::text(keyword)])
            .collect()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum ImportGroup {
    ToyboxUsing,
    ToyboxImport,
    OtherUsing,
    OtherImport,
}

fn import_name(decl: &Ast) -> Option<(ImportGroup, &str)> {
    let (name, toybox_group, other_group) = match decl {
        Ast::Using(decl) => (
            &decl.name.node,
            ImportGroup::ToyboxUsing,
            ImportGroup::OtherUsing,
        ),
        Ast::Import(decl) => (
            &decl.name.node,
            ImportGroup::ToyboxImport,
            ImportGroup::OtherImport,
        ),
        _ => return None,
    };

    let is_toybox = name == "Toybox" || name.starts_with("Toybox.");
    let group = if is_toybox { toybox_group } else { other_group };

    Some((group, name))
}

fn wrap_annotation(inner: Vec<Doc>, multiline: bool) -> Doc {
    if !multiline {
        return Doc::concat(vec![Doc::text("("), Doc::Concat(inner), Doc::text(")")]);
    }

    Doc::bracketed(
        Doc::text("("),
        Doc::Empty,
        Doc::concat(inner),
        Doc::text(")"),
    )
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
