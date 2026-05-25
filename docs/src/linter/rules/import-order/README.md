# `import-order`

Flags contiguous runs of `using` / `import` declarations that aren't in
canonical order.

## Rationale

A consistent import order makes files easier to scan, reduces merge conflicts,
and groups related declarations together. Sorting alphabetically eliminates
the need for editors to argue about placement; grouping `Toybox.*` separately
keeps SDK imports visually distinct from project ones.

## What triggers

The rule reports a run of `using` / `import` declarations whose order doesn't
match the canonical form. Canonical order is four groups, each sorted
alphabetically by the dotted path, separated by a blank line:

1. `using Toybox.*`
2. `import Toybox.*`
3. `using <other>`
4. `import <other>`

A declaration is `Toybox.*` when its path is exactly `Toybox` or starts with
`Toybox.`.

## What does not trigger

Each contiguous run of `using` / `import` is treated as its own block. A
non-import declaration between two import blocks creates a hard boundary —
declarations in the second block aren't pulled up to join the first. This
matches the behaviour of [Ruff][ruff]'s import sorter.

When a comment interleaves the imports (e.g., a `// section` line between two
`using` statements) the rule only enforces *order* — blank-line placement
around the user's comments is left alone.

## Example

Before:

```monkey-c
import ModuleC;
using ModuleA;
import Toybox.D;
using Toybox.A;
import Toybox.C;
using Toybox.B as D;
```

After `--fix`:

```monkey-c
using Toybox.A;
using Toybox.B as D;

import Toybox.C;
import Toybox.D;

using ModuleA;

import ModuleC;
```

## Fix

The fix replaces the entire run with the canonical text. The auto-fix is
suppressed when a comment interleaves the imports — rearranging would lose
the user's comment placement, so the edit is left to the user. The
diagnostic is still reported in that case.

[ruff]: https://github.com/astral-sh/ruff
