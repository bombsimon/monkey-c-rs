# `monkey-c-rs`

<p align="center">
  <img src="./monkey-c-rs-logo.png" alt="monkey-c-rs logo" />
</p>

## What's in this project

### `monkey-c-parser`

A lexer, parser and ast for [Monkey C]. See [`monkey-c-parser`][parser]

A full representation of the Monkey C AST to support future development of
tools, for now primarily the `monkey-c-formatter`.

### `monkey-c-formatter`

A formatter to format [Monkey C] code. See [`monkey-c-formatter`][formatter]

The main reason this project was created. An opinionated zero-config formatter
that produces a deterministic formatting experience similar to [ruff] and
[rustfmt].

> [!NOTE]
> I'd love any input and testing on the formatter. Both help finding bugs and
> inconsistencies but also input on the formatting algorithm. Please create an
> issue for any bug or feature request.

## Notes on AI

For transparency: this project uses AI. Implementations are suggested by me,
discussed, and reviewed before being committed.

## License

This repository is licensed under the [MIT License][license]

[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[formatter]: monkey-c-formatter
[license]: LICENSE
[parser]: monkey-c-parser
[ruff]: https://github.com/astral-sh/ruff
[rustfmt]: https://github.com/rust-lang/rustfmt
