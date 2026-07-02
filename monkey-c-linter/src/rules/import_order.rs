//! `import-order` — flag contiguous runs of `using`/`import` declarations that aren't in canonical
//! order.
//!
//! The canonical order is four groups, separated by blank lines:
//!
//! 1. `using Toybox.*`  (alphabetical)
//! 2. `import Toybox.*` (alphabetical)
//! 3. `using <other>`   (alphabetical)
//! 4. `import <other>`  (alphabetical)
//!
//! Within each group, declarations are sorted alphabetically by their dotted path. When comments
//! interleave the imports we still report unsorted order, but suppress the auto-fix — rearranging
//! would clobber the user's comment placement, so we leave the fix to the user.
use monkey_c_parser::ast::{Ast, Span};

use crate::visit::LintContext;
use crate::{Diagnostic, Fix};

pub const RULE: &str = "import-order";

pub fn check_ast_seq(seq: &[Ast], ctx: &LintContext) -> Vec<Diagnostic> {
    let mut out = Vec::new();
    let mut i = 0;

    while i < seq.len() {
        if !is_import_like(&seq[i]) {
            i += 1;
            continue;
        }

        let run_start = i;
        while i < seq.len() && is_import_like(&seq[i]) {
            i += 1;
        }

        let run = &seq[run_start..i];
        if run.len() < 2 {
            continue;
        }

        let Some(first) = run.first().and_then(|n| n.span()) else {
            continue;
        };

        let Some(last) = run.last().and_then(|n| n.span()) else {
            continue;
        };

        let span = Span {
            start: first.start,
            end: last.end,
        };

        let indent = leading_indent(ctx.source, first.start);
        let canonical = build_canonical(run, ctx.source, indent);
        let current = &ctx.source[span.start..span.end];
        let comments = has_interleaving_comments(run, ctx.source);

        // When comments interleave the imports we only enforce sort *order*, not exact text
        // (blank-line placement around the user's comments is their call). When there are no
        // comments we enforce full canonical form, including blank lines between groups.
        let should_report = if comments {
            !is_order_canonical(run)
        } else {
            current != canonical
        };

        if !should_report {
            continue;
        }

        // Suppress the auto-fix when comments interleave — rebuilding would lose the user's comment
        // placement.
        let fix = if comments {
            None
        } else {
            Some(Fix::single(span, canonical))
        };

        out.push(Diagnostic {
            rule: RULE,
            message: "imports should be sorted and grouped".into(),
            span,
            fix,
        });
    }

    out
}

fn is_import_like(ast: &Ast) -> bool {
    matches!(ast, Ast::Using(_) | Ast::Import(_))
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Group {
    ToyboxUsing,
    ToyboxImport,
    OtherUsing,
    OtherImport,
}

fn classify(ast: &Ast) -> (Group, &str) {
    match ast {
        Ast::Using(d) => {
            let group = if is_toybox(&d.name.node) {
                Group::ToyboxUsing
            } else {
                Group::OtherUsing
            };

            (group, &d.name.node)
        }
        Ast::Import(d) => {
            let group = if is_toybox(&d.name.node) {
                Group::ToyboxImport
            } else {
                Group::OtherImport
            };

            (group, &d.name.node)
        }
        _ => unreachable!("is_import_like already filtered"),
    }
}

fn is_toybox(name: &str) -> bool {
    name == "Toybox" || name.starts_with("Toybox.")
}

/// True if the run's statements appear in canonical `(Group, name)` order. Doesn't care about
/// whitespace/blank lines between them — that's a separate concern handled only when no comments
/// interleave.
fn is_order_canonical(run: &[Ast]) -> bool {
    let current: Vec<(Group, &str)> = run.iter().map(classify).collect();
    let mut sorted = current.clone();
    sorted.sort();
    current == sorted
}

/// Walk the bytes between consecutive statements in the run. If we see `//` or `/*` (outside any
/// string, but imports can't contain strings so any occurrence counts), the run is comment-mixed
/// and we don't touch it.
fn has_interleaving_comments(run: &[Ast], source: &str) -> bool {
    for w in run.windows(2) {
        let (Some(prev), Some(next)) = (w[0].span(), w[1].span()) else {
            continue;
        };

        let between = &source[prev.end..next.start];
        if between.contains("//") || between.contains("/*") {
            return true;
        }
    }

    false
}

/// Whitespace immediately before `start` up to the preceding newline (or
/// start-of-file). Used so the rebuilt block keeps the same indentation as
/// the original first statement.
fn leading_indent(source: &str, start: usize) -> &str {
    let prefix = &source[..start];
    let line_start = prefix.rfind('\n').map(|i| i + 1).unwrap_or(0);
    let indent = &prefix[line_start..];

    // Only treat it as indent if it's pure whitespace; otherwise something
    // non-trivial precedes the statement on the same line — leave indent
    // empty so we don't accidentally include code in our prepend.
    if indent.bytes().all(|b| b == b' ' || b == b'\t') {
        indent
    } else {
        ""
    }
}

fn build_canonical(run: &[Ast], source: &str, indent: &str) -> String {
    let mut groups: [Vec<(&str, &str)>; 4] = Default::default();
    for node in run {
        let Some(span) = node.span() else { continue };
        let (group, name) = classify(node);
        let text = &source[span.start..span.end];
        let idx = match group {
            Group::ToyboxUsing => 0,
            Group::ToyboxImport => 1,
            Group::OtherUsing => 2,
            Group::OtherImport => 3,
        };
        groups[idx].push((name, text));
    }

    for group in &mut groups {
        group.sort_by(|a, b| a.0.cmp(b.0));
    }

    let mut out = String::new();
    let mut first_group = true;
    for group in &groups {
        if group.is_empty() {
            continue;
        }

        if !first_group {
            out.push_str("\n\n");
            out.push_str(indent);
        }

        for (i, (_, text)) in group.iter().enumerate() {
            if i > 0 {
                out.push('\n');
                out.push_str(indent);
            }
            out.push_str(text);
        }

        first_group = false;
    }

    out
}

#[cfg(test)]
mod tests {
    use crate::{Diagnostic, apply_fixes, lint};
    use monkey_c_parser::parser::Parser;

    fn lints(src: &str) -> Vec<Diagnostic> {
        let output = Parser::new(src).parse().expect("parse");
        lint(&output, src)
    }

    fn fixed(src: &str) -> String {
        let diags = lints(src);
        let fixes = diags.into_iter().filter_map(|d| d.fix).collect();
        apply_fixes(src, fixes)
    }

    #[test]
    fn already_canonical_emits_no_diagnostic() {
        let src = r#"
using Toybox.A;
using Toybox.B;

import Toybox.C;

using ModuleA;

import ModuleB;
"#
        .trim();
        assert!(lints(src).is_empty());
    }

    #[test]
    fn single_import_no_diagnostic() {
        // A single statement is trivially sorted.
        let src = "using Toybox.A;";
        assert!(lints(src).is_empty());
    }

    #[test]
    fn unsorted_within_group_is_flagged() {
        let src = r#"
using Toybox.B;
using Toybox.A;
"#
        .trim();
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "import-order");
    }

    #[test]
    fn fix_sorts_alphabetically_within_group() {
        let src = r#"
using Toybox.B;
using Toybox.A;
"#
        .trim();
        let expected = r#"
using Toybox.A;
using Toybox.B;
"#
        .trim();
        assert_eq!(fixed(src), expected);
    }

    #[test]
    fn fix_separates_groups_with_blank_line() {
        let src = r#"
using Toybox.A;
import Toybox.B;
using ModuleA;
import ModuleB;
"#
        .trim();
        let expected = r#"
using Toybox.A;

import Toybox.B;

using ModuleA;

import ModuleB;
"#
        .trim();
        assert_eq!(fixed(src), expected);
    }

    #[test]
    fn fix_groups_toybox_separately_and_sorts() {
        let src = r#"
using Toybox.A;
using Toybox.B as D;
using Toybox.F.G;
import Toybox.C;
import Toybox.D;
using ModuleA;
using ModuleD;
import ModuleB;
import ModuleC;
"#
        .trim();
        let expected = r#"
using Toybox.A;
using Toybox.B as D;
using Toybox.F.G;

import Toybox.C;
import Toybox.D;

using ModuleA;
using ModuleD;

import ModuleB;
import ModuleC;
"#
        .trim();
        assert_eq!(fixed(src), expected);
    }

    #[test]
    fn comments_between_imports_still_report_but_skip_fix() {
        // Comments between statements mean we shouldn't rewrite (we'd lose
        // the user's comment placement), but the unsorted-ness is still
        // worth reporting — just without an auto-fix.
        let src = r#"
using Toybox.B;
// section
using Toybox.A;
"#
        .trim();
        let diags = lints(src);
        assert_eq!(diags.len(), 1);
        assert_eq!(diags[0].rule, "import-order");
        assert!(
            diags[0].fix.is_none(),
            "fix should be suppressed when comments interleave"
        );
    }

    #[test]
    fn comments_between_already_sorted_imports_no_diagnostic() {
        // Same-as-canonical order: the comment is fine, nothing to report.
        let src = r#"
using Toybox.A;
// section
using Toybox.B;
"#
        .trim();
        assert!(lints(src).is_empty());
    }

    #[test]
    fn preserves_indent_inside_module() {
        let src = r#"
module M {
    using Toybox.B;
    using Toybox.A;
}
"#
        .trim();
        let expected = r#"
module M {
    using Toybox.A;
    using Toybox.B;
}
"#
        .trim();
        assert_eq!(fixed(src), expected);
    }
}
