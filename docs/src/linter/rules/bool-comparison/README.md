# `bool-comparison`

Flags `==` / `!=` comparisons against a boolean literal, which can be written
as the operand itself or its negation.

## Rationale

Comparing a boolean expression to `true` or `false` restates what the
expression already says. `if (ready == true)` reads as "if ready is true",
which is just "if ready"; `if (ready == false)` is "if not ready". Dropping
the literal removes the redundant comparison and leaves the condition saying
exactly what it means.

## What triggers

An `==` or `!=` binary expression where exactly one side is the literal `true`
or `false`. The literal may be on either side, so `true == ready` is treated
the same as `ready == true`.

| Comparison    | Rewrite |
| ------------- | ------- |
| `x == true`   | `x`     |
| `x != false`  | `x`     |
| `x == false`  | `!x`    |
| `x != true`   | `!x`    |

## What does not trigger

- Both sides literal — `true == false` is a constant with no clearer form.
- Any operator other than `==` / `!=`; `count > 0` is left alone.
- Expressions that don't compare against a boolean literal at all.

## Example

Before:

```monkey-c
function f() {
    if (ready == true) {
        start();
    }

    return done != false;
}
```

After `--fix`:

```monkey-c
function f() {
    if (ready) {
        start();
    }

    return done;
}
```

## Fix

The fix replaces the whole comparison with the surviving operand, copied
verbatim from the source. When the rewrite negates an operand that is itself a
binary or ternary expression, it is wrapped in parentheses so `!` still binds
the whole expression — `a < b == false` becomes `!(a < b)`, not `!a < b`.
