# `invalid-directive`

Flags a `rafiki:` comment that rafiki can't read, like a
[`rafiki: ignore`](../../#suppressing-findings) with a typo in it.

## Why

A directive with a mistake in it does nothing, so a finding it was meant to hide
is still reported and the comment looks like it should work.

## What it flags

- A word after `rafiki:` that isn't a known directive
- `rafiki: fmt` without `off` or `on` after it
- A `rafiki: fmt off` or `rafiki: fmt on` that shares its line with code, or a
  `rafiki: fmt off` with no declaration or statement after it, which the
  formatter leaves as a plain comment
- A `rafiki: fmt on` with no `rafiki: fmt off` before it in the same block, or a
  `rafiki: fmt off` when formatting is already off
- A rule name that doesn't exist
- A missing `[` or `]`, or an empty rule list
- A `file-ignore` after the first line of code
- `unused-ignore` or `invalid-directive` in a rule list, since these can't be
  silenced with a comment

## Example

```monkey-c
// rafiki: ignore[unneeded-paren]
var ready = (count > 0);
```

`unneeded-paren` is flagged, since the rule is called `unneeded-parens`. There's
no fix, since rafiki can't know what was meant.
