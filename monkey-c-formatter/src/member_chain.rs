//! Layout of method chains such as `a.b(1).c()`, one `.name(…)` group per line, following Prettier.
//! See <https://github.com/prettier/prettier/blob/main/src/language-js/print/member-chain.js>.

use monkey_c_parser::ast::{CallExpr, Expr, IndexExpr, MemberExpr, Span};

use crate::Formatter;
use crate::doc::Doc;

impl Formatter {
    /// Lay out a method chain one `.name(…)` group per line, or `None` to leave it to the regular
    /// layout. [`MemberChain`] decides the layout; this handles the comments in the chain and
    /// renders its links.
    pub(crate) fn member_chain_to_doc(&self, expr: &Expr) -> Option<Doc> {
        let chain = MemberChain::new(expr)?;

        if chain.inner_gaps().any(|gap| self.has_comments_in(gap)) {
            return None;
        }

        // Decide from a peek before taking any comments, since falling back to the regular layout
        // after taking them would leave them nowhere to go.
        let group_gaps: Vec<Span> = chain.groups.iter().map(|group| group[0].gap()).collect();
        let commented_groups: Vec<bool> = group_gaps
            .iter()
            .map(|gap| self.has_comments_in(*gap))
            .collect();
        let layout = chain.layout(&commented_groups)?;

        let mut head = vec![self.expr_with_leading(chain.head)];
        head.extend(
            chain
                .head_links
                .iter()
                .map(|link| self.chain_link_to_doc(link)),
        );

        let groups: Vec<GroupDoc> = chain
            .groups
            .iter()
            .zip(&group_gaps)
            .map(|(links, gap)| GroupDoc {
                same_line_comments: self.drain_trailing_doc_bounded(gap.start, gap.end),
                own_line_comments: self.drain_leading_doc(gap.end),
                doc: Doc::Concat(
                    links
                        .iter()
                        .map(|link| self.chain_link_to_doc(link))
                        .collect(),
                ),
            })
            .collect();

        Some(chain_doc(Doc::Concat(head), &groups, layout))
    }

    fn chain_link_to_doc(&self, link: &ChainLink) -> Doc {
        match link {
            ChainLink::Member(e) => Doc::text(format!(".{}", e.property)),
            ChainLink::Call(e) => self.call_arguments_to_doc(e),
            ChainLink::Index(e) => Doc::concat(vec![
                Doc::text("["),
                self.expr_with_leading(&e.index),
                Doc::text("]"),
            ]),
        }
    }
}

/// One postfix step of a chain, applied to everything before it.
enum ChainLink<'a> {
    Member(&'a MemberExpr),
    Call(&'a CallExpr),
    Index(&'a IndexExpr),
}

impl ChainLink<'_> {
    /// The source between this link and whatever it applies to, where comments can sit outside of
    /// any node. An index has none, since a comment after `[` is part of the index expression.
    fn gap(&self) -> Span {
        match self {
            ChainLink::Member(e) => Span {
                start: e.object.span().end,
                end: e.span.end - e.property.len(),
            },
            ChainLink::Call(e) => Span {
                start: e.callee.span().end,
                end: e.args_open,
            },
            ChainLink::Index(e) => Span {
                start: e.object.span().end,
                end: e.object.span().end,
            },
        }
    }
}

/// A chain flattened into its head and the `.name(…)` groups after it. The parser nests a chain
/// with its last call outermost, but the layout depends on the chain as a whole.
struct MemberChain<'a> {
    head: &'a Expr,
    head_links: Vec<ChainLink<'a>>,
    groups: Vec<Vec<ChainLink<'a>>>,
    ends_in_call: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct ChainLayout {
    first_call_on_head_line: bool,
    always_broken: bool,
}

/// A rendered group. Comments on the line before it stay there, while comments on their own lines
/// move down with it.
struct GroupDoc {
    same_line_comments: Doc,
    own_line_comments: Doc,
    doc: Doc,
}

impl<'a> MemberChain<'a> {
    /// Split `expr` into a chain, or `None` when nothing after its head forms a group.
    fn new(expr: &'a Expr) -> Option<Self> {
        let (head, mut links) = flatten(expr);
        let is_member = |link: &ChainLink| matches!(link, ChainLink::Member(_));
        let is_call = |link: &ChainLink| matches!(link, ChainLink::Call(_));

        // The head keeps calls and indexes applied directly to it (`f()[0]`) and the property
        // accesses leading up to the first called member (`Toybox.ActivityMonitor`).
        let mut head_len = links.iter().take_while(|link| !is_member(link)).count();
        while head_len + 1 < links.len()
            && is_member(&links[head_len])
            && is_member(&links[head_len + 1])
        {
            head_len += 1;
        }

        if head_len == links.len() {
            return None;
        }

        // After the head, a new group starts at every member access that follows a call.
        let mut groups: Vec<Vec<ChainLink>> = vec![Vec::new()];
        for link in links.split_off(head_len) {
            let current = groups.last_mut().expect("groups start non-empty");
            if is_member(&link) && current.iter().any(is_call) {
                groups.push(vec![link]);
            } else {
                current.push(link);
            }
        }

        Some(Self {
            head,
            head_links: links,
            groups,
            ends_in_call: matches!(expr, Expr::Call(_)),
        })
    }

    /// The gaps between links inside the head or a group. A comment in one of these, such as
    /// between `.name` and `(`, has no place to go in either layout.
    fn inner_gaps(&self) -> impl Iterator<Item = Span> {
        let inside_groups = self.groups.iter().flat_map(|group| &group[1..]);

        self.head_links
            .iter()
            .chain(inside_groups)
            .map(ChainLink::gap)
    }

    /// How to lay out the chain given which groups have a comment in front of them, or `None` to
    /// leave it to the regular layout. A single call after the head reads fine broken inside its
    /// arguments, while a comment between groups forces the broken layout for any chain.
    fn layout(&self, commented_groups: &[bool]) -> Option<ChainLayout> {
        let always_broken = commented_groups.contains(&true);
        let first_call_on_head_line =
            !commented_groups[0] && self.groups.len() >= 2 && self.head_is_subject();

        let max_unbroken_groups = if first_call_on_head_line { 2 } else { 1 };
        if !always_broken && (!self.ends_in_call || self.groups.len() <= max_unbroken_groups) {
            return None;
        }

        Some(ChainLayout {
            first_call_on_head_line,
            always_broken,
        })
    }

    /// Whether the head names a module, class or `me`, which reads as the subject of the chain
    /// and keeps its first call on its line, e.g. `View.findDrawableById("id")`.
    fn head_is_subject(&self) -> bool {
        let is_subject_name =
            |name: &str| name.starts_with(char::is_uppercase) || name.chars().all(|c| c == '_');

        match self.head_links.last() {
            None => match self.head {
                Expr::Me(_) | Expr::Self_(_) | Expr::Bling(_) => true,
                Expr::Ident(e) => is_subject_name(&e.name),
                _ => false,
            },
            Some(ChainLink::Member(e)) => is_subject_name(&e.property),
            Some(_) => false,
        }
    }
}

/// Assemble a chain with every group on its own indented line, and unless it must break, try the
/// whole chain on one line first.
fn chain_doc(head: Doc, groups: &[GroupDoc], layout: ChainLayout) -> Doc {
    let broken = if layout.first_call_on_head_line {
        // Keep the first call on the head's line only when it fits there whole. Otherwise its
        // arguments would break and leave the rest of the chain dangling after `)`.
        let (first, rest) = groups.split_at(1);
        let head_with_first_call = Doc::group(vec![Doc::flat_or_break(
            Doc::concat(vec![head.clone(), first[0].doc.clone()]),
            Doc::concat(vec![head.clone(), broken_lines(first)]),
        )]);

        Doc::concat(vec![head_with_first_call, broken_lines(rest)])
    } else {
        Doc::concat(vec![head.clone(), broken_lines(groups)])
    };

    if layout.always_broken {
        return broken;
    }

    let mut one_line = vec![head];
    one_line.extend(groups.iter().map(|group| group.doc.clone()));

    Doc::group(vec![Doc::flat_or_break(Doc::Concat(one_line), broken)])
}

fn broken_lines(groups: &[GroupDoc]) -> Doc {
    let lines = groups.iter().flat_map(|group| {
        [
            group.same_line_comments.clone(),
            Doc::Indent(vec![
                Doc::HardLine,
                group.own_line_comments.clone(),
                group.doc.clone(),
            ]),
        ]
    });

    Doc::Concat(lines.collect())
}

/// Split `expr` into the innermost expression that is not a member access, call or index, and the
/// links applied to it in source order.
fn flatten(expr: &Expr) -> (&Expr, Vec<ChainLink<'_>>) {
    let mut links = Vec::new();
    let mut current = expr;

    loop {
        match current {
            Expr::Call(e) => {
                links.push(ChainLink::Call(e));
                current = &e.callee;
            }
            Expr::Member(e) => {
                links.push(ChainLink::Member(e));
                current = &e.object;
            }
            Expr::Index(e) => {
                links.push(ChainLink::Index(e));
                current = &e.object;
            }
            _ => break,
        }
    }

    links.reverse();

    (current, links)
}

#[cfg(test)]
mod tests {
    use super::*;

    use monkey_c_parser::ast::{Ast, Stmt};
    use monkey_c_parser::parser::Parser;

    fn parse(src: &str) -> Expr {
        let wrapped = format!("function f() {{ return {src}; }}");
        let ast = Parser::new(&wrapped).parse().expect("should parse").ast;

        if let Ast::Document(nodes, _) = ast
            && let Some(Ast::Function(function)) = nodes.into_iter().next()
            && let Some(Stmt::Return(ret)) = function.body.unwrap().stmts.into_iter().next()
        {
            return ret.value.expect("return value");
        }

        panic!("could not extract expression");
    }

    /// The head's links and each group, written as `.name()` shapes.
    fn shape(src: &str) -> (String, Vec<String>) {
        let expr = parse(src);
        let chain = MemberChain::new(&expr).expect("a chain");
        let text = |links: &[ChainLink]| {
            links
                .iter()
                .map(|link| match link {
                    ChainLink::Member(e) => format!(".{}", e.property),
                    ChainLink::Call(_) => "()".to_string(),
                    ChainLink::Index(_) => "[]".to_string(),
                })
                .collect::<String>()
        };

        (
            text(&chain.head_links),
            chain.groups.iter().map(|group| text(group)).collect(),
        )
    }

    fn layout(src: &str, commented_groups: &[bool]) -> Option<ChainLayout> {
        let expr = parse(src);

        MemberChain::new(&expr)
            .expect("a chain")
            .layout(commented_groups)
    }

    fn broken(first_call_on_head_line: bool, always_broken: bool) -> Option<ChainLayout> {
        Some(ChainLayout {
            first_call_on_head_line,
            always_broken,
        })
    }

    #[test]
    fn a_new_group_starts_at_each_member_after_a_call() {
        let groups = vec![".b()".to_string(), ".c()".to_string(), ".d.e()".to_string()];
        assert_eq!(shape("a.b(1).c().d.e(2)"), (String::new(), groups));
    }

    #[test]
    fn the_head_keeps_direct_calls_and_leading_properties() {
        assert_eq!(shape("f()[0].b()"), ("()[]".into(), vec![".b()".into()]));
        assert_eq!(
            shape("Toybox.ActivityMonitor.getInfo().steps"),
            (
                ".ActivityMonitor".into(),
                vec![".getInfo()".into(), ".steps".into()]
            )
        );
    }

    #[test]
    fn a_head_without_groups_is_not_a_chain() {
        assert!(MemberChain::new(&parse("f()")).is_none());
        assert!(MemberChain::new(&parse("a")).is_none());
    }

    #[test]
    fn short_chains_are_left_to_the_regular_layout() {
        assert_eq!(layout("a.b(1)", &[false]), None);
        assert_eq!(layout("View.find(1).set(2)", &[false; 2]), None);
        assert_eq!(layout("a.b(1).c(2).d", &[false; 3]), None);
    }

    #[test]
    fn longer_chains_break_one_group_per_line() {
        assert_eq!(layout("a.b(1).c()", &[false; 2]), broken(false, false));
        assert_eq!(
            layout("View.find(1).set(2).get()", &[false; 3]),
            broken(true, false)
        );
        assert_eq!(
            layout("me.find(1).set(2).get()", &[false; 3]),
            broken(true, false)
        );
        assert_eq!(
            layout("Toybox.Sensor.find(1).set().get()", &[false; 3]),
            broken(true, false)
        );
    }

    #[test]
    fn a_comment_between_groups_forces_the_broken_layout() {
        assert_eq!(layout("a.b(1)", &[true]), broken(false, true));
        assert_eq!(layout("a.b", &[true]), broken(false, true));
        assert_eq!(
            layout("View.find(1).set(2)", &[true, false]),
            broken(false, true)
        );
    }
}
