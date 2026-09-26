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

Developer tools for [Monkey C], the language used to build apps for Garmin
devices. A formatter, a linter, a language server for your editor and test
coverage, all in one command called `rafiki`.

Read the [documentation] to get started.

## Install

```sh
cargo install --git https://github.com/bombsimon/monkey-c-rs rafiki --rev <commit>
```

Nothing is stable yet, so pin the version with `--rev`.

## Usage

```sh
rafiki fmt                                # format the project
rafiki lint --fix                         # lint, applying the fixes that are safe
rafiki server                             # run the language server for your editor
rafiki coverage test -d fr965 -y key.der  # measure which functions the tests run
```

Settings go in a `rafiki.toml`, which the editor and the command line both read.
See [Configuration] for what can be set.

## Feedback

Bug reports and ideas are welcome. Please open an [issue] for anything you run
into or would like to see.

## Notes on AI

For transparency: this project is being built with assistance of AI. However,
all implementations are suggested by me, discussed, and reviewed before being
committed. The focus is still to build the best tools possible and know and
understand 100% of the codebase. Although not every line of code is written by
hand, the goal is to avoid slop.

## Other projects

In addition to this project I'm also working on other projects to improve the
developer experience for Garmin Connect IQ development.

- [garmin-monkeyc.nvim] - A Neovim port of the VS Code Connect IQ plugin
- [tree-sitter-monkey-c] - Tree-sitter implementation for Monkey C
- [awesome-garmin] - An awesome list of Garmin applications and tools for Garmin

## License

This repository is licensed under the [MIT License][license].

[Configuration]: https://bombsimon.github.io/monkey-c-rs/configuration/
[Monkey C]: https://developer.garmin.com/connect-iq/monkey-c/
[awesome-garmin]: https://github.com/bombsimon/awesome-garmin
[documentation]: https://bombsimon.github.io/monkey-c-rs
[garmin-monkeyc.nvim]: https://github.com/bombsimon/garmin-monkeyc.nvim
[issue]: https://github.com/bombsimon/monkey-c-rs/issues/new
[license]: LICENSE
[tree-sitter-monkey-c]: https://github.com/bombsimon/tree-sitter-monkey-c
