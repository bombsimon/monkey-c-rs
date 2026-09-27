# `null-union`

Flags a type written as a union with `Null`, like `String or Null`, that can
use the `?` shorthand instead.

## Why

`String?` and `String or Null` mean the same thing. The shorthand is shorter,
and reading `?` as "may be `null`" is quicker than spotting `Null` at the end
of a union.

## What it flags

A union of exactly two types where one of them is `Null`, wherever a type can
be written: parameters, return types, variables, `typedef`s, casts, generic
parameters, dictionary and tuple types, `Method(…)` types and interfaces.

| Written as                         | Becomes                        |
| ---------------------------------- | ------------------------------ |
| `String or Null`                   | `String?`                      |
| `Null or String`                   | `String?`                      |
| `String \| Null`                   | `String?`                      |
| `String or Toybox.Lang.Null`       | `String?`                      |
| `String? or Null`                  | `String?`                      |
| `Array<String> or Null`            | `Array<String>?`               |
| `[Number, String] or Null`         | `[Number, String]?`            |
| `{ :flag as Boolean } or Null`     | `{ :flag as Boolean }?`        |
| `(Method() as Boolean) or Null`    | `(Method() as Boolean)?`       |
| `Method() as Boolean or Null`      | `Method() as Boolean?`         |

The last row is a nullable return type, not a nullable `Method(…)`, since
`or Null` binds to the return type. The fix keeps that meaning.

## What it leaves alone

- Unions of more than two types, like `String or Number or Null`. `?` can only
  make a single type nullable.
- Unions without `Null`, like `String or Number`.
- A plain `Null`, like `function f() as Null`.

## Example

```monkey-c
// Before
var label as String or Null;

function onUpdate(dc as Dc or Null) as Array<Number or Null> {
    return [];
}

// After
var label as String?;

function onUpdate(dc as Dc?) as Array<Number?> {
    return [];
}
```
