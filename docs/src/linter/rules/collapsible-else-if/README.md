# `collapsible-else-if`

Flags an `else` whose entire body is a single `if`, which can be written as
`else if`.

## Rationale

`else { if (…) { … } }` is the same control flow as `else if (…) { … }` with an
extra level of braces and indentation. The `else if` form is shorter and is how
the chain is normally written.

## What triggers

The rule fires when an `else` branch is a block containing exactly one
statement, and that statement is an `if`. The nested `if` may keep its own
`else` chain — it is preserved as-is:

```monkey-c
if (a) {
} else {
    if (b) {
    } else {
    }
}
```

becomes

```monkey-c
if (a) {
} else if (b) {
} else {
}
```

## What does not trigger

- A comment in the `else` block around the nested `if`. Collapsing would discard
  it, so the rule backs off.
- An `else` block with more than the single nested `if`.
- An `else if` already written as such — there is no wrapping block to remove.
