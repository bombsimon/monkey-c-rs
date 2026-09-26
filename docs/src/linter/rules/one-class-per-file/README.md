# `one-class-per-file`

Flags a file with more than one class, following Garmin's
[Monkey C coding conventions][conventions].

## Why

With one class per file, the file name tells you where to find a class. A second
class in the same file is usually a helper that deserves its own file.

## What it flags

Every class after the first in a file, including classes inside modules.

## What it leaves alone

Functions, modules, typedefs and imports next to the one class.

## Example

```monkey-c
class Foo {
    function foo() {}
}

class Bar {
    function bar() {}
}
```

`Bar` is flagged. There's no fix, since moving it means picking a file name and
deciding what to bring along.

[conventions]: https://developer.garmin.com/connect-iq/monkey-c/coding-conventions/
