//! Expressions, including binary chains that break before their operators.

use monkey_c_parser::ast::{BinaryOperator, Expr, LiteralValue, ParenExpr, UnaryOperator};

use crate::Formatter;
use crate::doc::Doc;
use crate::operators;
use crate::options::{self, Numbers, ResourceRef};

/// Which side of a binary operator an operand is on.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperandSide {
    Left,
    Right,
}

/// One operand of a binary chain, and whether it gets parentheses the source didn't have.
struct ChainOperand<'a> {
    expr: &'a Expr,
    parenthesized: bool,
}

/// The groups of operators readers tell apart, to see which mixes need parentheses to read right.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum OperatorFamily {
    Additive,
    Multiplicative,
    Shift,
    Bitwise,
    Comparison,
    TypeCheck,
    Logical,
}

impl Formatter {
    fn binary_chain_parts(
        &self,
        operands: &[ChainOperand],
        ops: &[(&BinaryOperator, usize)],
        outer_max_pos: usize,
    ) -> Vec<Doc> {
        let mut parts: Vec<Doc> = Vec::new();

        for (i, operand) in operands.iter().enumerate() {
            let span = *operand.expr.span();
            let leading = self.drain_leading_doc(span.start);

            // Bound the inner expression at the operator position so it cannot
            // steal comments that sit between the operand and the operator.
            // For the last operand fall back to `outer_max_pos`.
            let inner_max = ops
                .get(i)
                .map(|(_, op_pos)| *op_pos)
                .unwrap_or(outer_max_pos);

            let inner = match operand.expr {
                Expr::Binary(e) if operand.parenthesized => Doc::concat(vec![
                    Doc::text("("),
                    self.binary_chain_to_doc(
                        operand.expr,
                        &e.operator,
                        true,
                        span.end.min(inner_max),
                    ),
                    Doc::text(")"),
                ]),
                Expr::Binary(e) => {
                    self.binary_chain_to_doc(operand.expr, &e.operator, false, inner_max)
                }
                _ => self.expr_inner_to_doc_ctx(operand.expr, inner_max),
            };

            parts.push(leading);
            parts.push(inner);

            if i + 1 < operands.len() {
                let (op, op_pos) = ops[i];
                let op_text = operators::binary_op(op);
                let op_end = op_pos + op_text.len();
                let op_line = self.line_index.line(op_pos as u32);
                let next_start = operands[i + 1].expr.span().start;

                // A comment after the operator that ends the line keeps the operator at the end of
                // the line in front of it, as in `left || // c`, rather than moving past it.
                let comment_after_op = self.node_starts_new_line(op_end, next_start);
                let breaks = self.token_starts_new_line(span.end, op_pos);
                parts.push(self.drain_before_token(span.end, op_pos));

                if comment_after_op {
                    let op_on_operand_line = op_line == self.line_index.line(span.end as u32 - 1);
                    parts.push(if breaks {
                        Doc::HardLine
                    } else if op_on_operand_line {
                        Doc::text(" ")
                    } else {
                        Doc::Line
                    });
                    parts.push(Doc::text(op_text));
                    parts.push(self.drain_trailing_doc(op_end, next_start));
                    parts.push(Doc::HardLine);
                } else {
                    parts.push(if breaks { Doc::HardLine } else { Doc::Line });
                    parts.push(Doc::text(format!("{op_text} ")));
                }
            } else {
                parts.push(self.drain_trailing_doc(span.end, outer_max_pos));
            }
        }

        parts
    }

    fn binary_chain_to_doc(
        &self,
        expr: &Expr,
        op: &BinaryOperator,
        outermost: bool,
        outer_max_pos: usize,
    ) -> Doc {
        let mut operands: Vec<ChainOperand> = Vec::new();
        let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
        self.collect_binary_chain(expr, op, &mut operands, &mut ops);

        let inner = self.binary_chain_parts(&operands, &ops, outer_max_pos);

        if outermost {
            Doc::Group(vec![Doc::Indent(inner)])
        } else {
            Doc::Group(inner)
        }
    }

    /// Operators that bind as tight share one chain, so `a + b - c + d` wraps at every `+` and
    /// `-`, while `b * c` in `a + b * c` stays a tight group of its own and keeps the grouping
    /// visible after wrapping.
    fn collect_binary_chain<'a>(
        &self,
        expr: &'a Expr,
        chain_op: &BinaryOperator,
        operands: &mut Vec<ChainOperand<'a>>,
        ops: &mut Vec<(&'a BinaryOperator, usize)>,
    ) {
        if let Expr::Binary(be) = expr
            && be.operator.precedence() == chain_op.precedence()
        {
            let left = self.operand_without_parens(&be.left, be.operator, OperandSide::Left);
            self.collect_chain_operand(left, be.operator, chain_op, operands, ops);
            ops.push((&be.operator, be.op_pos));

            if ends_with_cast(&be.left) {
                self.after_cast.borrow_mut().insert(be.right.span().start);
            }

            let right = self.operand_without_parens(&be.right, be.operator, OperandSide::Right);
            self.collect_chain_operand(right, be.operator, chain_op, operands, ops);

            return;
        }

        operands.push(ChainOperand {
            expr,
            parenthesized: false,
        });
    }

    /// An operand that reads ambiguously under `parent` becomes its own parenthesized operand
    /// instead of joining the chain.
    fn collect_chain_operand<'a>(
        &self,
        operand: &'a Expr,
        parent: BinaryOperator,
        chain_op: &BinaryOperator,
        operands: &mut Vec<ChainOperand<'a>>,
        ops: &mut Vec<(&'a BinaryOperator, usize)>,
    ) {
        if self.parens == options::Parens::Normalize
            && let Expr::Binary(inner) = operand
            && needs_clarifying_parens(inner.operator, parent)
        {
            // These parentheses already open the operand after a cast, so the ones inside are free
            // to go.
            self.after_cast.borrow_mut().remove(&operand.span().start);
            operands.push(ChainOperand {
                expr: operand,
                parenthesized: true,
            });

            return;
        }

        self.collect_binary_chain(operand, chain_op, operands, ops);
    }

    /// `expr` without the parentheses around it, in a place where any expression reads the same.
    /// An assignment keeps them, since `x = y = 1` isn't one.
    pub(crate) fn without_parens<'a>(&self, expr: &'a Expr) -> &'a Expr {
        let mut current = expr;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
            && !matches!(*p.inner, Expr::Assign(_))
        {
            current = &p.inner;
        }

        current
    }

    /// A ternary's condition without parentheses, unless it's a ternary itself, which would read
    /// as a chain, or ends in a cast, whose type would take the `?` as nullable.
    fn condition_without_parens<'a>(&self, expr: &'a Expr) -> &'a Expr {
        let mut current = expr;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
            && !matches!(*p.inner, Expr::Assign(_) | Expr::Ternary(_))
            && !ends_with_cast(&p.inner)
        {
            current = &p.inner;
        }

        current
    }

    /// A ternary branch without parentheses. A nested ternary keeps them in the `then` branch,
    /// where it's hard to tell which `:` belongs to which `?`, and a cast keeps them in the
    /// `else` branch, where whatever follows the ternary could extend its type.
    fn branch_without_parens<'a>(&self, expr: &'a Expr, branch: Branch) -> &'a Expr {
        let mut current = expr;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
            && !matches!(*p.inner, Expr::Assign(_))
        {
            let keeps = match branch {
                Branch::Then => matches!(*p.inner, Expr::Ternary(_)),
                Branch::Else => ends_with_cast(&p.inner),
            };

            if keeps {
                break;
            }

            current = &p.inner;
        }

        current
    }

    /// The operand of a prefix operator like `!` or `-` without parentheses around a name,
    /// literal, call or other operand that precedence can't split. Another prefix operator keeps
    /// them, since `-(-x)` would otherwise become the decrement `--x`.
    fn simple_without_parens<'a>(&self, expr: &'a Expr) -> &'a Expr {
        let mut current = expr;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
            && is_simple(&p.inner)
            && !is_prefix_unary(&p.inner)
        {
            current = &p.inner;
        }

        current
    }

    /// What a member access, index or call is applied to, without parentheses around a name, call,
    /// member access or index. A literal keeps them, since `(360).toFloat()` reads more clearly
    /// than `360.toFloat()`, and so does `new`, where `new Foo().bar()` would hide which part
    /// `new` applies to.
    pub(crate) fn receiver_without_parens<'a>(&self, expr: &'a Expr) -> &'a Expr {
        let mut current = expr;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
            && matches!(
                *p.inner,
                Expr::Paren(_)
                    | Expr::Call(_)
                    | Expr::Member(_)
                    | Expr::Index(_)
                    | Expr::Ident(_)
                    | Expr::Me(_)
                    | Expr::Self_(_)
                    | Expr::Bling(_)
            )
        {
            current = &p.inner;
        }

        current
    }

    /// `operand` of a `parent` operator without parentheses that can't change how it groups:
    /// around an operator that binds tighter, or one that binds as tight on the left, since every
    /// operator is left-associative. On the right, the same associative operator regroups freely
    /// too, so `a || (b || c)` is `a || b || c`.
    fn operand_without_parens<'a>(
        &self,
        operand: &'a Expr,
        parent: BinaryOperator,
        side: OperandSide,
    ) -> &'a Expr {
        let mut current = operand;
        while let Expr::Paren(p) = current
            && self.drops_parens(p)
        {
            let redundant = match p.inner.as_ref() {
                inner if is_simple(inner) => true,
                Expr::Binary(inner) => {
                    let inner_precedence = inner.operator.precedence();
                    let parent_precedence = parent.precedence();
                    let groups = inner_precedence > parent_precedence
                        || (inner_precedence == parent_precedence && side == OperandSide::Left)
                        || (side == OperandSide::Right
                            && associative(inner.operator).is_some()
                            && associative(inner.operator) == associative(parent));

                    groups && !ends_with_cast(&p.inner)
                }
                _ => false,
            };

            if !redundant {
                break;
            }

            current = &p.inner;
        }

        current
    }

    fn drops_parens(&self, paren: &ParenExpr) -> bool {
        self.parens == options::Parens::Normalize
            && !self.has_written_comments_in(paren.span)
            && !self.after_cast.borrow().contains(&paren.span.start)
    }

    pub(crate) fn condition_to_doc(&self, expr: &Expr) -> Doc {
        if let Expr::Binary(e) = expr {
            let mut operands: Vec<ChainOperand> = Vec::new();
            let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
            self.collect_binary_chain(expr, &e.operator, &mut operands, &mut ops);

            // Cap at cond.span.end so the last operand cannot drain trailing
            // comments that sit outside the condition (i.e. after the `)`
            // in `if (…)`). paren_condition_header drains those via has_line.
            let cond_end = expr.span().end;

            return Doc::Concat(self.binary_chain_parts(&operands, &ops, cond_end));
        }

        self.expr_with_leading(expr)
    }

    pub(crate) fn expr_list_to_doc(&self, exprs: &[Expr]) -> Doc {
        Doc::concat(
            exprs
                .iter()
                .enumerate()
                .flat_map(|(i, e)| {
                    let sep = if i > 0 { vec![Doc::text(", ")] } else { vec![] };
                    sep.into_iter()
                        .chain(std::iter::once(self.expr_with_leading(e)))
                })
                .collect(),
        )
    }

    pub(crate) fn expr_with_leading(&self, expr: &Expr) -> Doc {
        let span = *expr.span();
        let leading = self.drain_leading_doc(span.start);
        let inner = self.expr_inner_to_doc(expr);

        Doc::concat(vec![leading, inner])
    }

    /// Like [`Self::expr_with_leading`] but caps the trailing drain of any nested
    /// binary chain at `outer_max_pos`. Used when the expression sits inside a
    /// delimiter (e.g. paren) whose closing token must not be swallowed by a
    /// `//` comment that belongs to an outer chain.
    pub(crate) fn expr_with_leading_bounded(&self, expr: &Expr, outer_max_pos: usize) -> Doc {
        let leading = self.drain_leading_doc(expr.span().start);
        let inner = self.expr_inner_to_doc_ctx(expr, outer_max_pos);

        Doc::concat(vec![leading, inner])
    }

    fn expr_inner_to_doc_ctx(&self, expr: &Expr, outer_max_pos: usize) -> Doc {
        match expr {
            // Bound binary chains at the expression's own span so they cannot
            // drain trailing comments that belong to the surrounding delimiter
            // (e.g. a trailing `//` after the last operand of an array entry
            // must stay after the `,`, not inside the binary expression doc).
            Expr::Binary(e) => self.binary_chain_to_doc(
                expr,
                &e.operator,
                true,
                expr.span().end.min(outer_max_pos),
            ),
            Expr::Paren(e) => {
                let inner_end = e.inner.span().end;
                let paren_end = e.span.end.min(outer_max_pos);

                Doc::concat(vec![
                    Doc::text("("),
                    self.expr_with_leading_bounded(&e.inner, paren_end),
                    self.drain_trailing_doc(inner_end, paren_end),
                    Doc::text(")"),
                ])
            }
            _ => self.expr_inner_to_doc(expr),
        }
    }

    pub(crate) fn expr_inner_to_doc(&self, expr: &Expr) -> Doc {
        match expr {
            // Bound binary chains at the expression's own span so they cannot
            // steal trailing comments that appear after a following `;` or `,`.
            Expr::Binary(e) => self.binary_chain_to_doc(expr, &e.operator, true, expr.span().end),
            Expr::Unary(e) => match e.operator {
                UnaryOperator::PostInc => {
                    Doc::concat(vec![self.expr_with_leading(&e.operand), Doc::text("++")])
                }
                UnaryOperator::PostDec => {
                    Doc::concat(vec![self.expr_with_leading(&e.operand), Doc::text("--")])
                }
                UnaryOperator::ResourceRef if self.resource_ref == ResourceRef::Bare => {
                    self.expr_with_leading(&e.operand)
                }
                _ => Doc::concat(vec![
                    Doc::text(operators::unary_prefix_op(&e.operator)),
                    self.expr_with_leading(self.simple_without_parens(&e.operand)),
                ]),
            },
            Expr::Ternary(e) => {
                let condition = self.expr_with_leading(self.condition_without_parens(&e.condition));
                let condition_end = e.condition.span().end;
                let question_break = self.line_before_token(condition_end, e.question_pos);
                let before_question = self.drain_before_token(condition_end, e.question_pos);
                let then_expr =
                    self.after_token(e.question_pos + 1, e.then_expr.span().start, || {
                        self.expr_with_leading(
                            self.branch_without_parens(&e.then_expr, Branch::Then),
                        )
                    });
                let then_end = e.then_expr.span().end;
                let colon_break = self.line_before_token(then_end, e.colon_pos);
                let before_colon = self.drain_before_token(then_end, e.colon_pos);
                let else_expr = self.after_token(e.colon_pos + 1, e.else_expr.span().start, || {
                    self.expr_with_leading(self.branch_without_parens(&e.else_expr, Branch::Else))
                });

                Doc::Group(vec![
                    condition,
                    before_question,
                    Doc::Indent(vec![
                        question_break,
                        Doc::text("?"),
                        then_expr,
                        before_colon,
                        colon_break,
                        Doc::text(":"),
                        else_expr,
                    ]),
                ])
            }
            Expr::Assign(e) => Doc::concat(vec![
                self.expr_with_leading(&e.target),
                self.token_then_expr(
                    e.target.span().end,
                    operators::assign_op(&e.operator),
                    e.op_pos,
                    self.without_parens(&e.value),
                ),
            ]),
            Expr::Call(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                let callee = self.expr_with_leading(self.receiver_without_parens(&e.callee));

                Doc::concat(vec![callee, self.call_arguments_to_doc(e)])
            }
            Expr::Member(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                Doc::concat(vec![
                    self.expr_with_leading(self.receiver_without_parens(&e.object)),
                    Doc::text(format!(".{}", e.property)),
                ])
            }
            Expr::Index(e) => {
                let mut parts = vec![
                    self.expr_with_leading(self.receiver_without_parens(&e.object)),
                    Doc::text("["),
                    self.expr_with_leading(self.without_parens(&e.index)),
                ];
                parts.push(self.drain_before_close_doc(e.span.end - 1));
                parts.push(Doc::text("]"));
                Doc::Concat(parts)
            }
            Expr::New(e) => {
                // Drained here or the first argument would claim them and move them inside `(`.
                let class = Doc::concat(vec![
                    Doc::text("new "),
                    Doc::Indent(vec![
                        self.drain_leading_doc(e.class_span.start),
                        Doc::text(&e.class),
                    ]),
                ]);

                let Some(args_open) = e.args_open else {
                    return Doc::concat(vec![class, Doc::text("()")]);
                };

                let before_open = self.drain_before_open_paren(args_open);
                let first_arg_start = e
                    .args
                    .first()
                    .map(|a| a.value.span().start)
                    .unwrap_or(e.span.end);
                let hugged = self.hugged_sole_collection(
                    &e.args,
                    e.args.has_trailing_comma(),
                    args_open,
                    e.span.end,
                );

                if let Some(args) = hugged {
                    return Doc::concat(vec![class, before_open, args]);
                }

                let after_open_force_newline =
                    self.after_open_has_line_comment(args_open, first_arg_start);
                let after_open = self.drain_after_open_paren(args_open, first_arg_start);

                let items = self.call_args_to_items(&e.args, e.span.end);
                let before_close = self.drain_dangling_comments_doc(e.span.end);

                Doc::concat(vec![
                    class,
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
            Expr::NewArray(e) => {
                let mut parts = vec![Doc::text("new")];
                if let Some(ty) = &e.element_type {
                    parts.push(Doc::text(" "));
                    parts.push(self.type_to_doc(ty));
                } else {
                    parts.push(Doc::text(" "));
                }
                parts.push(Doc::text("["));
                parts.push(self.expr_with_leading(self.without_parens(&e.size)));
                let close_pos = e.span.end - if e.is_byte_array { 2 } else { 1 };
                parts.push(self.drain_before_close_doc(close_pos));
                parts.push(Doc::text(if e.is_byte_array { "]b" } else { "]" }));

                Doc::Concat(parts)
            }
            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_with_leading(&e.expr),
                Doc::text(" as "),
                self.cast_type_to_doc(&e.target_type, &self.source[e.target_type.span.end..]),
            ]),
            Expr::Array(e) => self.format_array(e),
            Expr::Dict(e) => self.format_dict(e),
            Expr::Lit(e) => Doc::text(literal_text(&e.value, &self.numbers)),
            Expr::Ident(e) => Doc::text(&e.name),
            Expr::Paren(e) => {
                let inner_end = e.inner.span().end;

                Doc::concat(vec![
                    Doc::text("("),
                    self.expr_with_leading_bounded(&e.inner, e.span.end),
                    self.drain_trailing_doc(inner_end, e.span.end),
                    Doc::text(")"),
                ])
            }
            Expr::Me(_) => Doc::text("me"),
            Expr::Self_(_) => Doc::text("self"),
            Expr::Bling(_) => Doc::text("$"),
        }
    }
}

/// A literal as the formatter writes it.
pub(crate) fn literal_text(value: &LiteralValue, numbers: &Numbers) -> String {
    match value {
        LiteralValue::Number(v) => v.clone(),
        LiteralValue::Long(v) => numbers.long(v),
        LiteralValue::Hex(v) | LiteralValue::HexLong(v) => numbers.hex(v),
        LiteralValue::Float(lit) => numbers.float(lit),
        LiteralValue::Double(lit) => numbers.double(lit),
        LiteralValue::String(v) => format!("\"{v}\""),
        LiteralValue::Char(v) => format!("'{v}'"),
        LiteralValue::Boolean(v) => v.to_string(),
        LiteralValue::Symbol(v) => format!(":{v}"),
        LiteralValue::Null => "null".to_string(),
        LiteralValue::NaN => "NaN".to_string(),
    }
}

#[derive(Clone, Copy)]
enum Branch {
    Then,
    Else,
}

/// Whether `expr` holds together as one operand whatever operator is around it.
fn is_simple(expr: &Expr) -> bool {
    match expr {
        Expr::Unary(_)
        | Expr::Paren(_)
        | Expr::Call(_)
        | Expr::Member(_)
        | Expr::Index(_)
        | Expr::New(_)
        | Expr::NewArray(_)
        | Expr::Array(_)
        | Expr::Dict(_)
        | Expr::Lit(_)
        | Expr::Ident(_)
        | Expr::Me(_)
        | Expr::Self_(_)
        | Expr::Bling(_) => true,
        Expr::Binary(_) | Expr::Ternary(_) | Expr::Assign(_) | Expr::TypeCast(_) => false,
    }
}

/// The operator `op` is the same as for regrouping, for those where `a op (b op c)` always equals
/// `a op b op c`. `+` and `*` aren't among them, since `"x" + (1 + 2)` is `"x3"` and floats round
/// differently when regrouped.
fn associative(op: BinaryOperator) -> Option<BinaryOperator> {
    match op {
        BinaryOperator::And | BinaryOperator::AndKeyword => Some(BinaryOperator::And),
        BinaryOperator::Or | BinaryOperator::OrKeyword => Some(BinaryOperator::Or),
        BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => Some(op),
        _ => None,
    }
}

/// Whether `inner` right under `parent` needs parentheses to read right, even where precedence
/// doesn't. That's `*`, `/` and `%` mixed and `&&` inside `||`, as in Prettier, and every mix
/// where Monkey C groups differently from C, like `a + b << c` meaning `a + (b << c)`.
fn needs_clarifying_parens(inner: BinaryOperator, parent: BinaryOperator) -> bool {
    match (operator_family(inner), operator_family(parent)) {
        (OperatorFamily::Logical, OperatorFamily::Logical) => {
            associative(inner) != associative(parent)
        }
        (OperatorFamily::Logical, _)
        | (_, OperatorFamily::Logical)
        | (OperatorFamily::Additive, OperatorFamily::Additive) => false,
        (inner_family, parent_family) if inner_family == parent_family => inner != parent,
        (
            OperatorFamily::Additive | OperatorFamily::Multiplicative | OperatorFamily::Comparison,
            OperatorFamily::Additive | OperatorFamily::Multiplicative | OperatorFamily::Comparison,
        )
        | (OperatorFamily::Shift, OperatorFamily::Comparison)
        | (OperatorFamily::Comparison, OperatorFamily::Shift) => false,
        _ => true,
    }
}

fn operator_family(op: BinaryOperator) -> OperatorFamily {
    match op {
        BinaryOperator::Add | BinaryOperator::Sub => OperatorFamily::Additive,
        BinaryOperator::Mul | BinaryOperator::Div | BinaryOperator::Mod => {
            OperatorFamily::Multiplicative
        }
        BinaryOperator::LeftShift | BinaryOperator::RightShift => OperatorFamily::Shift,
        BinaryOperator::BitAnd | BinaryOperator::BitOr | BinaryOperator::BitXor => {
            OperatorFamily::Bitwise
        }
        BinaryOperator::Eq
        | BinaryOperator::NotEq
        | BinaryOperator::Lt
        | BinaryOperator::LtEq
        | BinaryOperator::Gt
        | BinaryOperator::GtEq => OperatorFamily::Comparison,
        BinaryOperator::InstanceOf | BinaryOperator::Has => OperatorFamily::TypeCheck,
        BinaryOperator::And
        | BinaryOperator::AndKeyword
        | BinaryOperator::Or
        | BinaryOperator::OrKeyword => OperatorFamily::Logical,
    }
}

fn is_prefix_unary(expr: &Expr) -> bool {
    matches!(expr, Expr::Unary(e) if !matches!(e.operator, UnaryOperator::PostInc | UnaryOperator::PostDec))
}

/// Whether the rightmost part of `expr` is a cast, whose type would take a `|` or `or` after it
/// as one more union member.
fn ends_with_cast(expr: &Expr) -> bool {
    match expr {
        Expr::TypeCast(_) => true,
        Expr::Binary(e) => ends_with_cast(&e.right),
        Expr::Unary(e) => {
            !matches!(e.operator, UnaryOperator::PostInc | UnaryOperator::PostDec)
                && ends_with_cast(&e.operand)
        }
        Expr::Ternary(e) => ends_with_cast(&e.else_expr),
        Expr::Assign(e) => ends_with_cast(&e.value),
        _ => false,
    }
}
