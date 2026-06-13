# `redundant-resource-ref`

Flags the legacy `@` prefix on resource references.

## Rationale

`@Rez.Strings.foo` and `Rez.Strings.foo` compile to the same thing — the `@`
is a vestigial marker from older Monkey C syntax. Dropping it removes noise
without changing behavior.

## What triggers

Any expression of the form `@<resource reference>`, e.g.:

```monkey-c
dc.drawText(@Rez.Strings.AppName);
```

## Example

Before:

```monkey-c
function onLayout(dc as Dc) as Void {
    dc.drawText(@Rez.Strings.AppName);
}
```

After `--fix`:

```monkey-c
function onLayout(dc as Dc) as Void {
    dc.drawText(Rez.Strings.AppName);
}
```

## Fix

The fix replaces the `@`-prefixed expression with the underlying resource
reference, dropping the `@`.
