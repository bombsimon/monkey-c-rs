//! Expressions, including binary chains that break before their operators.

use monkey_c_parser::ast::{
    BinaryOperator, DoubleLit, Expr, FloatLit, LiteralValue, UnaryOperator,
};

use crate::Formatter;
use crate::doc::Doc;
use crate::operators;

impl Formatter {
    fn binary_chain_parts(
        &self,
        operands: &[&Expr],
        ops: &[(&BinaryOperator, usize)],
        outer_max_pos: usize,
    ) -> Vec<Doc> {
        let mut parts: Vec<Doc> = Vec::new();

        for (i, operand) in operands.iter().enumerate() {
            let span = *operand.span();
            let leading = self.drain_leading_doc(span.start);

            // Bound the inner expression at the operator position so it cannot
            // steal comments that sit between the operand and the operator.
            // For the last operand fall back to `outer_max_pos`.
            let inner_max = ops
                .get(i)
                .map(|(_, op_pos)| *op_pos)
                .unwrap_or(outer_max_pos);

            let inner = match operand {
                Expr::Binary(e) => self.binary_chain_to_doc(operand, &e.operator, false, inner_max),
                _ => self.expr_inner_to_doc_ctx(operand, inner_max),
            };

            parts.push(leading);
            parts.push(inner);

            if i + 1 < operands.len() {
                let (op, op_pos) = ops[i];
                let op_text = operators::binary_op(op);
                let op_end = op_pos + op_text.len();
                let op_line = self.line_index.line(op_pos as u32);
                let next_start = operands[i + 1].span().start;

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
        let mut operands: Vec<&Expr> = Vec::new();
        let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
        collect_binary_chain(expr, op, &mut operands, &mut ops);

        let inner = self.binary_chain_parts(&operands, &ops, outer_max_pos);

        if outermost {
            Doc::Group(vec![Doc::Indent(inner)])
        } else {
            Doc::Group(inner)
        }
    }

    pub(crate) fn condition_to_doc(&self, expr: &Expr) -> Doc {
        if let Expr::Binary(e) = expr {
            let mut operands: Vec<&Expr> = Vec::new();
            let mut ops: Vec<(&BinaryOperator, usize)> = Vec::new();
            collect_binary_chain(expr, &e.operator, &mut operands, &mut ops);

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
                _ => Doc::concat(vec![
                    Doc::text(operators::unary_prefix_op(&e.operator)),
                    self.expr_with_leading(&e.operand),
                ]),
            },
            Expr::Ternary(e) => {
                let condition = self.expr_with_leading(&e.condition);
                let condition_end = e.condition.span().end;
                let question_break = self.line_before_token(condition_end, e.question_pos);
                let before_question = self.drain_before_token(condition_end, e.question_pos);
                let then_expr =
                    self.after_token(e.question_pos + 1, e.then_expr.span().start, || {
                        self.expr_with_leading(&e.then_expr)
                    });
                let then_end = e.then_expr.span().end;
                let colon_break = self.line_before_token(then_end, e.colon_pos);
                let before_colon = self.drain_before_token(then_end, e.colon_pos);
                let else_expr = self.after_token(e.colon_pos + 1, e.else_expr.span().start, || {
                    self.expr_with_leading(&e.else_expr)
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
                    &e.value,
                ),
            ]),
            Expr::Call(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                let callee = self.expr_with_leading(&e.callee);

                Doc::concat(vec![callee, self.call_arguments_to_doc(e)])
            }
            Expr::Member(e) => {
                if let Some(chain) = self.member_chain_to_doc(expr) {
                    return chain;
                }

                Doc::concat(vec![
                    self.expr_with_leading(&e.object),
                    Doc::text(format!(".{}", e.property)),
                ])
            }
            Expr::Index(e) => {
                let mut parts = vec![
                    self.expr_with_leading(&e.object),
                    Doc::text("["),
                    self.expr_with_leading(&e.index),
                ];
                let before_close = self.drain_leading_doc(e.span.end - 1);
                if !matches!(before_close, Doc::Empty) {
                    parts.push(Doc::text(" "));
                    parts.push(before_close);
                }
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
                parts.push(self.expr_with_leading(&e.size));
                let close_pos = e.span.end - if e.is_byte_array { 2 } else { 1 };
                let before_close = self.drain_leading_doc(close_pos);
                if !matches!(before_close, Doc::Empty) {
                    parts.push(Doc::text(" "));
                    parts.push(before_close);
                }
                parts.push(Doc::text(if e.is_byte_array { "]b" } else { "]" }));

                Doc::Concat(parts)
            }
            Expr::TypeCast(e) => Doc::concat(vec![
                self.expr_with_leading(&e.expr),
                Doc::text(" as "),
                self.type_to_doc(&e.target_type),
            ]),
            Expr::Array(e) => self.format_array(e),
            Expr::Dict(e) => self.format_dict(e),
            Expr::Lit(e) => Doc::text(match &e.value {
                LiteralValue::Number(v)
                | LiteralValue::Long(v)
                | LiteralValue::Hex(v)
                | LiteralValue::HexLong(v) => v.clone(),
                LiteralValue::Float(lit) => format_float_lit(lit),
                LiteralValue::Double(lit) => format_double_lit(lit),
                LiteralValue::String(v) => format!("\"{v}\""),
                LiteralValue::Char(v) => format!("'{v}'"),
                LiteralValue::Boolean(v) => v.to_string(),
                LiteralValue::Symbol(v) => format!(":{v}"),
                LiteralValue::Null => "null".to_string(),
                LiteralValue::NaN => "NaN".to_string(),
            }),
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

/// Re-emit a [`FloatLit`] in the exact source form recorded by the lexer.
pub(crate) fn format_float_lit(lit: &FloatLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    match lit.suffix {
        Some(suffix) => format!("{body}{suffix}"),
        None => body,
    }
}

/// Re-emit a [`DoubleLit`] preserving its source style.
pub(crate) fn format_double_lit(lit: &DoubleLit) -> String {
    let body = if let Some(exp) = &lit.exponent {
        format!("{}{exp}", lit.digits)
    } else {
        lit.digits.clone()
    };

    format!("{body}{}", lit.suffix)
}

fn collect_binary_chain<'a>(
    expr: &'a Expr,
    chain_op: &BinaryOperator,
    operands: &mut Vec<&'a Expr>,
    ops: &mut Vec<(&'a BinaryOperator, usize)>,
) {
    if let Expr::Binary(be) = expr
        && operators::precedence_group(&be.operator) == operators::precedence_group(chain_op)
    {
        collect_binary_chain(&be.left, chain_op, operands, ops);
        ops.push((&be.operator, be.op_pos));
        collect_binary_chain(&be.right, chain_op, operands, ops);

        return;
    }

    operands.push(expr);
}
