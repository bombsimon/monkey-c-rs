# Introduction

<p align="center">
  <img src="./assets/monkey-c-rs-logo.png" alt="monkey-c-rs logo" />
</p>

`monkey-c-rs` is a set of developer tools for [Monkey C], the language used to
build apps for [Garmin] devices with the Connect IQ SDK. Everything ships as a
single command line tool called `rafiki`:

- `rafiki fmt` formats your code.
- `rafiki lint` finds common mistakes and can fix many of them for you.
- `rafiki server` brings both into your editor as a language server.
- `rafiki coverage` shows which functions your unit tests run.

## Why it exists

Monkey C has been around since 2014, but it still lacks the tooling most
languages take for granted. The Connect IQ SDK comes with a compiler and an
editor extension for code navigation, but there is no formatter or linter.

This project fills that gap. It starts from a full parser for the language and
builds the tools on top of it, taking inspiration from [ruff] and [rustfmt].
The tools are fast and have close to nothing to configure. See
[Design principles](design) for the reasoning behind them.

## Status

The tools are usable today, but nothing is stable yet. Output and settings can
still change between versions, so pin the version you install. See
[Getting started](getting-started) for how.

Some things aren't supported yet:

- Comments that turn off a lint rule or formatting for a section of code.
- Files that aren't UTF-8, which are skipped with an error.
- An extension for VS Code. The language server works with any editor that can
  run one, see [Editor integration](lsp).

## Feedback

Bug reports and ideas are welcome, whether it's a formatting choice you
disagree with, a lint rule you'd like or something that doesn't work. Please
open an [issue] on GitHub.

[Garmin]: https://www.garmin.com/
[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[ruff]: https://github.com/astral-sh/ruff
[rustfmt]: https://github.com/rust-lang/rustfmt
