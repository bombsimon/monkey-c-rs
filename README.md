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
> I'd love any input and testing on the formatter and linter. Both help finding
> bugs and inconsistencies but also input on the formatting algorithm and rules,
> what could or should be configurable etc. Please create an issue for any bug
> or feature request.

## What's in this project

### `monkey-c-parser`

A lexer, parser and AST for [Monkey C]. See [`monkey-c-parser`][parser]

A full representation of the Monkey C AST to support development of tools like
formatters and linters.

---

### `monkey-c-formatter`

A formatter to format [Monkey C] code. See [`monkey-c-formatter`][formatter]

The main reason this project was created. An opinionated zero-config formatter
that produces a deterministic formatting experience similar to [ruff] and
[rustfmt].

---

### `monkey-c-linter`

A linter for [Monkey C] with machine-applicable fixes. See
[`monkey-c-linter`][linter]

Rules walk the AST produced by `monkey-c-parser` and emit diagnostics with
optional `--fix` suggestions that patch source byte ranges directly. The
linter is independent of the formatter and fixes don't reformat unrelated code.
Run the formatter after `--fix` if you want whitespace normalised.

---

### `monkey-c-lsp`

A [Language Server][lsp] for [Monkey C]. See [`monkey-c-lsp`][lsp-crate]

Exposes the parser, linter, and formatter over LSP so any compatible editor
gets live diagnostics (parse errors and lints) and document formatting. It
speaks over stdio and keeps whole documents in memory (full sync).

---

## Notes on AI

For transparency: this project is being built with assistance of AI. However,
all implementations are suggested by me, discussed, and reviewed before being
committed. My focus is still to build the best tools possible and know and
understand 100% of the codebase. Although not every line of code is written by
hand, I want to avoid slop.

## License

This repository is licensed under the [MIT License][license]

[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[formatter]: monkey-c-formatter
[license]: LICENSE
[linter]: monkey-c-linter
[lsp]: https://microsoft.github.io/language-server-protocol/
[lsp-crate]: monkey-c-lsp
[parser]: monkey-c-parser
[ruff]: https://github.com/astral-sh/ruff
[rustfmt]: https://github.com/rust-lang/rustfmt
