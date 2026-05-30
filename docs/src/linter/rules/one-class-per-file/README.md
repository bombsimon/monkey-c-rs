# `one-class-per-file`

Flags files that declare more than one class.

## Rationale

Keeping each class in its own file makes the file name a reliable index of
its primary type, lets readers locate code by filename alone, and keeps
related members (fields, methods, helper functions) grouped together.
Co-locating two classes usually means one of them is incidental and should
move to its own file.

## What triggers

Anything after the first class declaration anywhere in the file. The rule
counts across module boundaries, so a file with two top-level classes,
a module containing two classes, or two nested modules each holding a
class all trigger.

## What does not trigger

A file with exactly one class, regardless of how many functions, modules,
typedefs, or `using` declarations sit alongside it.

## Example

Before:

```monkey-c
class Foo {
    function foo() {}
}

class Bar {
    function bar() {}
}
```

The rule reports `Bar` as the offending second class. There is no
auto-fix — moving `Bar` to its own file requires choosing a file name and
deciding what context (imports, module wrapping) to copy across.
