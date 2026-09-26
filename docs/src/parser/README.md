# Parser library

`monkey-c-parser` turns Monkey C source into an [abstract syntax tree][ast]. The
formatter, linter and language server are all built on it, and it can be used on
its own to build other tools.

```rust,ignore
use monkey_c_parser::parser::Parser;

let output = Parser::new("function f() { return 1; }").parse()?;

// The syntax tree, and the comments kept in a separate table.
println!("{:#?}", output.ast);
println!("{:#?}", output.comments);
```

Comments aren't part of the tree itself. They're kept in their own table with
their positions, so tools that don't care about them can ignore them, and the
formatter can put each one back where it belongs.

The parser follows Garmin's [Monkey C reference][monkey-c-language-reference].
Its design is based on the parser in [ruff].

[ast]: https://en.wikipedia.org/wiki/Abstract_syntax_tree
[monkey-c-language-reference]: https://developer.garmin.com/connect-iq/reference-guides/monkey-c-reference
[ruff]: https://github.com/astral-sh/ruff
