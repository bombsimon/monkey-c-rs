# `annotation-comma`

Flags a comma between the entries of an annotation, like `(:test, :debug)`, and
replaces it with a space.

## Why

Monkey C accepts both commas and whitespace between annotations, so a codebase
easily ends up mixing both, sometimes in the same annotation. The only
documented form, and the one Garmin uses, is space separated.

## What it flags

Every comma between two annotations.

| Written as                 | Becomes                   |
| -------------------------- | ------------------------- |
| `(:test, :debug)`          | `(:test :debug)`          |
| `(:test,:debug)`           | `(:test :debug)`          |
| `(:test :debug, :release)` | `(:test :debug :release)` |

Comments after the comma are kept where they are.

## What it leaves alone

- Commas between the arguments of an annotation, like `(:typecheck(false, true))`.
- Annotations already separated by whitespace.

## Example

```monkey-c
// Before
(:background, :glance)
function getGlanceView() as [GlanceView] {}

// After
(:background :glance)
function getGlanceView() as [GlanceView] {}
```
