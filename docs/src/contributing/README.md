# Contributing

Bug reports, ideas and pull requests are all welcome. For anything bigger than a
small fix, open an [issue] first so the approach can be discussed.

## Building and testing

The project is a Rust workspace. The [justfile] has the commands CI runs:

```sh
just test     # run all tests
just clippy   # lint, with warnings as errors
just format   # format the code
just doc      # build the API docs, with warnings as errors
just docs     # build the book and check its links
```

Most formatter tests are [insta] snapshot tests. When a change alters the
output, review the new snapshots with `cargo insta review` before accepting
them. Every bug fix should come with a snapshot that shows the fixed case.

The docs are an [mdBook] under `docs/`. Run `mdbook serve docs` to preview them,
and `just docs` to build them and check every link with [lychee], the same way
CI does.

## Where things live

| Crate                | Contains                                           |
| -------------------- | -------------------------------------------------- |
| `rafiki`             | The command line tool                              |
| `monkey-c-parser`    | The lexer, parser and syntax tree                  |
| `monkey-c-formatter` | The formatter                                      |
| `monkey-c-linter`    | The lint rules and their fixes                     |
| `monkey-c-lsp`       | The language server                                |
| `monkey-c-config`    | The `rafiki.toml` format                           |
| `monkey-c-coverage`  | The source rewriting behind `rafiki coverage`      |
| `monkey-c-jungle`    | A parser and printer for jungle files              |

## Adding a lint rule

1. Add a module under `monkey-c-linter/src/rules` with a `check_expr` or
   `check_stmt` function that returns a `Diagnostic`, and a fix if one is safe.
2. Call it from the matching `dispatch_*` function in `visit.rs`.
3. Add its name to `rules::ALL`, which the command line uses for `--enable`,
   `--disable` and `--list-rules`.
4. Add tests next to the rule for what it flags and what it leaves alone.
5. Add a page under `docs/src/linter/rules`, and list it in the
   [Rules](../linter/rules) table and in `docs/src/SUMMARY.md`.

[insta]: https://insta.rs/
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[justfile]: https://github.com/bombsimon/monkey-c-rs/blob/main/justfile
[lychee]: https://github.com/lycheeverse/lychee
[mdBook]: https://rust-lang.github.io/mdBook/
