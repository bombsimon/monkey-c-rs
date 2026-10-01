# `pipe-union`

Flags a `|` used to separate the types in a union, like `Number | Float`, and
replaces it with `or`.

## Why

`or` and `|` mean the same thing in a union type, so a codebase easily ends up
mixing both, sometimes in the same type. `or` is what Garmin's documentation
uses and what almost all Monkey C code writes. It also can't be mistaken for a
bitwise `|` or a logical `||`, which matters in casts where a type and an
expression sit next to each other.

## What it flags

Every `|` between two types, wherever a type can be written: parameters,
return types, variables, `typedef`s, casts, generic parameters, dictionary and
tuple types, `Method(…)` types and interfaces.

| Written as                 | Becomes                    |
| -------------------------- | -------------------------- |
| `Number \| Float`          | `Number or Float`          |
| `String or Number \| Null` | `String or Number or Null` |
| `Array<Number \| Float>`   | `Array<Number or Float>`   |
| `Number\|Float`            | `Number or Float`          |

Comments around the `|` are kept where they are.

## What it leaves alone

- A bitwise OR after a cast, like `0x00 as Number | 0xFF`. A `|` there only
  separates union types when a type follows it.
- Bitwise OR in any other expression.

## Example

```monkey-c
// Before
typedef Numeric as Number | Float;

function f(value as String or Number | Null) as Void {}

// After
typedef Numeric as Number or Float;

function f(value as String or Number or Null) as Void {}
```
