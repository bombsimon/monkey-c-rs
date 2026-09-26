# `ifs-same-cond`

Flags an `if` / `else if` chain where two branches have the same condition.

## Why

The first branch with a condition always wins, so a later branch with the same
condition can never run. It's almost always a copy and paste mistake where the
second condition should have been something else.

## What it flags

Two branches in the same chain with the same condition, ignoring whitespace.
They don't have to be next to each other.

## What it leaves alone

- Separate `if` statements with the same condition, since both can run.
- Conditions with a function call or `new`, since those can give a different
  result each time.

## Example

```monkey-c
if (status == OK) {
    handleOk();
} else if (status == OK) {
    handleError();
}
```

There's no fix, since the rule can't know what the second condition should be.
