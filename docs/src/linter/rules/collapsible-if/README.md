# `collapsible-if`

Flags an `if` that only contains another `if`, where the two conditions can be
joined with `&&`.

## Why

When the inner `if` is all the outer one does, the nesting doesn't add anything.
The two checks are really one condition and read better written as one.

## What it flags

An `if` without an `else` whose only statement is another `if` without an
`else`. With an `else` on either of them, merging would change what runs, so
those are left alone.

A condition that binds looser than `&&`, like an `||` or a ternary, gets
parentheses when merged: `if (a || b) { if (c) { … } }` becomes
`if ((a || b) && c) { … }`.

## What it leaves alone

- An `if` with a comment next to the inner `if`, since merging would lose the
  comment.
- An `if` with anything besides the inner `if`.

## Example

```monkey-c
// Before
if (ready) {
    if (count > 0) {
        process();
    }
}

// After
if (ready && count > 0) {
    process();
}
```
