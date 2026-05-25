# Parser

The `monkey-c-parser` is the core that allows us to work with Monkey C at a
higher level of abstraction — an [Abstract Syntax Tree][ast].

Heavily inspired (and motivated) by the work on [astral-sh/ruff][ruff], which
has the AST used by [RustPython] and an extremely fast formatter and linter.

## Language Specification

The [Garmin Connect IQ][connect-iq] SDK does a decent job documenting the
[Monkey C language][monkey-c-doc] and has a [language
specification][monkey-c-language-reference] that the parser is based on.

Although not used in the parser, they also have [API docs][api-docs] documenting
the standard library.

[RustPython]: https://github.com/RustPython/RustPython
[api-docs]: https://developer.garmin.com/connect-iq/api-docs/
[ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[connect-iq]: https://developer.garmin.com/connect-iq/overview
[monkey-c-doc]: https://developer.garmin.com/connect-iq/monkey-c
[monkey-c-language-reference]: https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference
[ruff]: https://github.com/astral-sh/ruff
