# `monkey-c-rs`

<p align="center">
  <img src="./docs/src/assets/monkey-c-rs-logo.png" alt="monkey-c-rs logo" />
</p>

<p align="center">
  <a href="https://github.com/bombsimon/monkey-c-rs/actions/workflows/ci.yml">
    <img src="https://github.com/bombsimon/monkey-c-rs/actions/workflows/ci.yml/badge.svg" alt="Build status" />
  </a>
  <a href="https://bombsimon.github.io/monkey-c-rs">
    <img src="https://github.com/bombsimon/monkey-c-rs/actions/workflows/docs.yml/badge.svg" alt="Docs status" />
  </a>
</p>

> [!NOTE]
> I'd love feedback and testing on the formatter and linter, both help catch
> bugs and inconsistencies, but I'm just as interested in input on the
> formatting algorithm and lint rules, and what should be configurable. Please
> open an issue for any bugs or feature requests you run into.

## Installing

```sh
cargo install --git https://github.com/bombsimon/monkey-c-rs rafiki
```

> [!TIP]
> Since nothing here is stable yet, remember to pin your installation to avoid
> breaking changes.
>
> ```sh
> cargo install --git https://github.com/user/repo --rev a1b2c3d
> ```

## Usage

```sh
rafiki fmt                               # format the project
rafiki lint --fix                        # lint, applying what can be fixed
rafiki server                            # run the language server (for your editor)
rafiki coverage test -d <device> -y key  # measure test coverage
```

Settings can be pinned per project in a `rafiki.toml`, which the language server
reads too, so an editor and the command line always agree.

## What's in this project

### `rafiki`

The command-line interface and single binary for all tools. The formatter,
linter, language server and coverage instrumentation are linked into it and
reached as `rafiki fmt`, `rafiki lint`, `rafiki server` and `rafiki coverage`.

---

### `monkey-c-parser`

A lexer, parser and AST for [Monkey C].

A full representation of the Monkey C AST to support development of tools like
formatters and linters.

---

### `monkey-c-jungle`

A parser and printer for [jungle files][jungle-reference], the Connect IQ build
language.

Parses a `.jungle` file into an AST that keeps its comments and blank lines, can
be edited, and writes itself back out.

---

### `monkey-c-formatter`

The formatter behind `rafiki fmt`.

The main reason this project was created. An opinionated near-zero-config
formatter that produces a deterministic formatting experience similar to [ruff]
and [rustfmt].

---

### `monkey-c-linter`

The linter behind `rafiki lint`, with machine-applicable fixes.

Rules walk the AST produced by `monkey-c-parser` and emit diagnostics with
optional `--fix` suggestions that patch source byte ranges directly. The
linter is independent of the formatter and fixes don't reformat unrelated code.
Run `rafiki fmt` after `--fix` if you want whitespace normalised.

---

### `monkey-c-lsp`

The [Language Server][lsp] behind `rafiki server`.

Exposes the parser, linter, and formatter over LSP so any compatible editor
gets live diagnostics (parse errors and lints) and document formatting. It
speaks over stdio and keeps whole documents in memory (full sync).

> [!IMPORTANT]
> Garmin has their own LSP server bundled with the SDK that supports
> definitions, declarations, references and other standard features. However
> they do not have a formatter or linter so for now the plan for this project is
> _only_ to support the missing pieces. This LSP will therefore support
> `codeAction`, `formatting` and diagnostics for code that isn't parsed
> successfully.
>
> Notes on the LSP server, its capabilities and how to run it from Neovim can be
> found in [garmin-monkeyc.nvim]. For the best developer experience it's
> suggested to run both servers together.

---

### `monkey-c-config`

The `rafiki.toml` format, shared by the CLI and the language server so both
resolve a project's settings the same way.

---

### `monkey-c-coverage`

The function-level test coverage instrumentation behind `rafiki coverage`.

Connect IQ has no native coverage support, so the source is instrumented
before compilation: a probe after each function's opening brace records which
functions the test suite actually executes, including those reached
indirectly.

> [!WARNING]
> Coverage is function-level only, a fully executed 200-line function counts
> the same as a one-line one.

---

## Notes on AI

For transparency: this project is being built with assistance of AI. However,
all implementations are suggested by me, discussed, and reviewed before being
committed. The focus is still to build the best tools possible and know and
understand 100% of the codebase. Although not every line of code is written by
hand, the goal is to to avoid slop.

## Other projects

In addition to this project I'm also working on other projects to improve the
developer experience for Garmin Connect IQ development.

- [garmin-monkeyc.nvim] - A Neovim port of the VS Code Connect IQ plugin
- [tree-sitter-monkey-c] - Tree-sitter implementation for Monkey C
- [awesome-garmin] - An awesome list of Garmin applications and tools for Garmin

## License

This repository is licensed under the [MIT License][license]

[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[awesome-garmin]: https://github.com/bombsimon/awesome-garmin
[garmin-monkeyc.nvim]: https://github.com/bombsimon/garmin-monkeyc.nvim
[jungle-reference]: https://developer.garmin.com/connect-iq/reference-guides/jungle-reference/
[license]: LICENSE
[lsp]: https://microsoft.github.io/language-server-protocol/
[ruff]: https://github.com/astral-sh/ruff
[rustfmt]: https://github.com/rust-lang/rustfmt
[tree-sitter-monkey-c]: https://github.com/bombsimon/tree-sitter-monkey-c
