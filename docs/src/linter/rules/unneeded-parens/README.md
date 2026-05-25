# `unneeded-parens`

Flags `(<expr>)` written in positions where the parentheses don't change
parsing.

## Rationale

A parenthesised expression in certain positions is unambiguous — the grammar
accepts any expression there regardless of operator precedence. Wrapping it in
extra parens reads as either a typo or copy-paste residue.

## What triggers

The rule fires when a `(<expr>)` appears as:

- the right-hand side of an assignment (`x = (1 + 2);`),
- the initializer of a `var` / `const` binding (`var x = (1 + 2);`),
- the value of a `return` statement (`return (x);`).

In all three slots, removing the parentheses cannot affect operator precedence.

## What does not trigger

The rule is intentionally conservative and skips positions where parentheses
*can* matter:

- Operand of a binary operator — `1 * (2 + 3)` *needs* the parens.
- Object position of a `.member` or `[index]` — `(x + 1).foo` *needs* the
  parens.
- Anywhere else not in the explicit list above.

## Example

Before:

```monkey-c
var x = (1 + 2);
function f() {
    return (foo());
}
```

After `--fix`:

```monkey-c
var x = 1 + 2;
function f() {
    return foo();
}
```

## Fix

The fix replaces the source between the parentheses (trimmed of surrounding
whitespace) into the outer span. Comments inside the parens are preserved:
`(/* tag */ 2 + 3)` becomes `/* tag */ 2 + 3`.
