# `bool-comparison`

Flags a comparison with `true` or `false`, which can be written as the value
itself or its negation.

## Why

`if (ready == true)` says the same thing as `if (ready)`, and
`if (ready == false)` the same as `if (!ready)`. The comparison only adds
something to read past.

## What it flags

An `==` or `!=` where one side is `true` or `false`, on either side:

| Comparison   | Written as |
| ------------ | ---------- |
| `x == true`  | `x`        |
| `x != false` | `x`        |
| `x == false` | `!x`       |
| `x != true`  | `!x`       |

When the fix negates a comparison or a ternary, it adds parentheses so the `!`
applies to all of it: `a < b == false` becomes `!(a < b)`.

## What it leaves alone

- A comparison between two literals, like `true == false`.
- Other operators, like `count > 0`.

## Example

```monkey-c
// Before
if (ready == true) {
    start();
}

return done != false;

// After
if (ready) {
    start();
}

return done;
```
