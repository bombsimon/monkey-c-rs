# `redundant-resource-ref`

Flags the legacy `@` before a resource reference.

## Why

`@Rez.Strings.AppName` and `Rez.Strings.AppName` compile to the same thing. The
`@` is left over from older versions of Monkey C.

## What it flags

Any resource reference written with `@`.

## Example

```monkey-c
// Before
label.setText(@Rez.Strings.Title);

// After
label.setText(Rez.Strings.Title);
```
