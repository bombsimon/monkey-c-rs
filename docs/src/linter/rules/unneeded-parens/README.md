# `unneeded-parens`

Flags parentheses where they can't change how the code is read.

## Why

Parentheses that don't do anything make the reader stop and look for a reason
that isn't there.

## What it flags

- The value of an assignment or a `var` or `const`, like `x = (1 + 2);`.
- The value of a `return`, like `return (x);`.
- A `Method(…)` type in parentheses where it doesn't need them, like the return
  type in `function f() as (Method() as Boolean)`.

Comments inside the parentheses are kept.

## What it leaves alone

Places where parentheses can matter:

- Around an operand, like `1 * (2 + 3)`.
- Before `.` or `[`, like `(x + 1).toString()`.
- A nullable `Method(…)` type, like `(Method() as Boolean)?`, where removing
  them would make only the return type nullable.

## Example

```monkey-c
// Before
function f() as (Method() as Boolean) {
    var x = (1 + 2);
    return (method(:g));
}

// After
function f() as Method() as Boolean {
    var x = 1 + 2;
    return method(:g);
}
```
