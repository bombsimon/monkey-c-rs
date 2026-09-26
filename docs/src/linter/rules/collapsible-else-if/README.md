# `collapsible-else-if`

Flags an `else` block that only contains an `if`, which can be written as
`else if`.

## Why

`else { if (…) { … } }` behaves exactly like `else if (…) { … }`, with an extra
level of braces and indentation.

## What it flags

An `else` block whose only statement is an `if`. The inner `if` keeps its own
`else`, if it has one.

## What it leaves alone

- An `else` block with a comment next to the `if`, since collapsing would lose
  the comment.
- An `else` block with anything besides the `if`.

## Example

```monkey-c
// Before
if (a) {
    first();
} else {
    if (b) {
        second();
    } else {
        third();
    }
}

// After
if (a) {
    first();
} else if (b) {
    second();
} else {
    third();
}
```
