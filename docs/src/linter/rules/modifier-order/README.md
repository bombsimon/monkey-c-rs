# `modifier-order`

Flags `static` written before the visibility modifier.

## Why

Monkey C accepts `static` and the visibility in either order, and the formatter
keeps them as written, so a codebase easily ends up mixing both. Putting the
visibility first is the convention in Java and C# and required in TypeScript.

## What it flags

Any declaration where `static` comes before `public`, `private`, `protected` or
`hidden`.

## What it leaves alone

When a comment sits between the two keywords the declaration is still flagged,
but there's no fix since it's unclear which keyword the comment belongs to.

## Example

```monkey-c
// Before
static private const MAX = 10;
static public function create() as Void {}

// After
private static const MAX = 10;
public static function create() as Void {}
```
