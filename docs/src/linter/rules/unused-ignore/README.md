# `unused-ignore`

Flags a rule in a [`rafiki: ignore`](../../#suppressing-findings) comment that
has nothing to silence.

## Why

Suppressions outlive the code they were written for. Once the finding is gone,
the comment only hides the next real finding of that rule in the same place.

## What it flags

Each rule in an `ignore` or `file-ignore` comment that reports nothing in the
code the comment covers.

## What it leaves alone

Rules turned off with `disable` or left out of `enable`, since they can't report
anything anyway. A suppression for them stays valid if they're turned back on.

## Example

```monkey-c
// Before
var total = count + 1; // rafiki: ignore[unneeded-parens]
var ready = (count > 0); // rafiki: ignore[unneeded-parens, bool-comparison]

// After
var total = count + 1;
var ready = (count > 0); // rafiki: ignore[unneeded-parens]
```

When some of a comment's rules are still in use, the fix keeps those and drops
the rest.

This rule can't be silenced with a `rafiki: ignore` comment, but it can be turned
off with `disable`.
