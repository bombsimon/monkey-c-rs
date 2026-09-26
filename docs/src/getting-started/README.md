# Getting started

## Install

There are no prebuilt binaries yet, so `rafiki` is installed from source with
[Cargo]. Pin a commit, since output and settings can still change between
versions:

```sh
cargo install --git https://github.com/bombsimon/monkey-c-rs rafiki --rev <commit>
```

Leave out `--rev` to get the latest version instead.

## Format your code

Run the formatter from anywhere in your project. It formats every `.mc` file it
finds and writes the result back:

```sh
rafiki fmt
```

To see what would change without touching any files, use `--check` to list the
files or `--diff` to print the changes.

## Lint your code

```sh
rafiki lint
```

Each finding names the rule that raised it. Many can be fixed automatically, and
the fixes don't reformat the code around them, so format again afterwards:

```sh
rafiki lint --fix
rafiki fmt
```

## Configure

Most projects need no configuration. To change a setting, add a `rafiki.toml`
to the root of the project:

```toml
[format]
line-width = 100

[lint]
disable = ["one-class-per-file"]
```

The file is read by both the command line and the editor, so they always agree.
All settings are listed under [Configuration](../configuration).

## Set up your editor

`rafiki server` runs a language server, so your editor can show lint findings
as you type and format files when you save them. See
[Editor integration](../lsp) for how to set it up.

## Run it in CI

Both commands exit with `1` when they find something, so they can gate a pull
request. A GitHub Actions job could look like this:

```yaml
jobs:
  rafiki:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v7
      - uses: dtolnay/rust-toolchain@stable
      - run: cargo install --git https://github.com/bombsimon/monkey-c-rs rafiki --rev <commit>
      - run: rafiki fmt --check
      - run: rafiki lint
```

Installing from source compiles `rafiki`, which takes a few minutes. Caching
`~/.cargo/bin` between runs avoids doing it every time.

[Cargo]: https://doc.rust-lang.org/cargo/getting-started/installation.html
