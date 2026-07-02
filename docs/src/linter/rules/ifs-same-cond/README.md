# `ifs-same-cond`

Flags an `if` / `else if` chain where two arms test the same condition.

## Rationale

A repeated condition in a chain is almost always a copy-paste error: the
earlier arm always matches first, so the later arm with the identical
condition is unreachable. What the author meant to write was a *different*
condition in the second arm.

## What triggers

The rule fires when two arms of the same `if` / `else if` chain have
structurally equal conditions — formatting and whitespace are ignored, so
`a == b` matches `a==b`. The duplicate need not be adjacent —
`if (a) … else if (b) … else if (a) …` is flagged too. Each repeated arm is
reported once.

## What does not trigger

- Two separate `if` statements that happen to share a condition. They are not
  one chain — the first can fall through to the second, so the repetition may
  be intentional.
- Any condition containing a call (`foo()`) or a `new`. A call may have side
  effects, so two textually-identical calls need not evaluate to the same
  value on each run.

## Example

```monkey-c
if (status == OK) {
    handleOk();
} else if (status == OK) { // can never run — likely meant `status == ERROR`
    handleError();
}
```

No auto-fix — the rule can't know what the second condition was meant to be.
