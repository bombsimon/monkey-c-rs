# `collapsible-if`

Flags an `if` whose entire body is a single nested `if`, where the two can be
merged into one by `&&`-combining their conditions.

## Rationale

Each `if` adds a level of nesting. When the inner `if` is the only thing the
outer one does, that nesting is noise — the guard is really a single compound
condition, and reads more clearly written as one.

## What triggers

The rule fires when all of the following hold:

1. The outer `if` has no `else`.
2. Its body is exactly one statement, and that statement is an `if`.
3. The inner `if` has no `else`.

Either `else` would change which condition guards the fall-through, so the
merge would not preserve behaviour — those cases are left alone.

## What does not trigger

- A comment in the outer block around the nested `if`. Collapsing would discard
  it, so the rule backs off.
- An outer block with more than the single nested `if`.

## Example

```monkey-c
if (ready) {
    if (count > 0) {
        process();
    }
}
```

Fixed:

```monkey-c
if (ready && count > 0) {
    process();
}
```

A condition that binds looser than `&&` (an `||` / `or`, a ternary) is wrapped
in parentheses when merged, so `if (a || b) { if (c) { … } }` becomes
`if ((a || b) && c) { … }`.
