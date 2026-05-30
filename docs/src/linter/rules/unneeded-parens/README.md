# `unneeded-parens`

Flags parentheses written in positions where they don't change parsing.

## Rationale

Parens that don't affect parsing read as either a typo or copy-paste residue.
Removing them clarifies the code without changing behavior.

## What triggers

- The right-hand side of an assignment: `x = (1 + 2);`
- The initializer of a `var` / `const`: `var x = (1 + 2);`
- The value of a `return`: `return (x);`
- Parens around a `Method(…)` type annotation when they aren't load-bearing,
  e.g. `(Method() as Boolean)` as a function return type.

## What does not trigger

Positions where parens *can* affect parsing:

- Operand of a binary operator — `1 * (2 + 3)` needs the parens.
- Object position of `.member` or `[index]` — `(x + 1).foo` needs the parens.
- `(Method(…) as Return)?` — without the parens, the trailing `?` binds to
  `Return` rather than to the whole callable.

## Example

Before:

```monkey-c
function f(cb as (Method(x as Number) as Void)?) as (Method() as Boolean) {
    var x = (1 + 2);
    return (method(:g));
}
```

After `--fix`:

```monkey-c
function f(cb as (Method(x as Number) as Void)?) as Method() as Boolean {
    var x = 1 + 2;
    return method(:g);
}
```

## Fix

The fix replaces the source between the parens (trimmed of surrounding
whitespace) into the outer span. Comments inside the parens are preserved:
`(/* tag */ 2 + 3)` becomes `/* tag */ 2 + 3`.
