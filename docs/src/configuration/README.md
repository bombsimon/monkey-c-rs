# Configuration

Settings live in a `rafiki.toml` at the root of your project. Every setting is
optional, and a project without the file uses the defaults. The available
settings are listed under [Settings](settings).

```toml
[format]
line-width = 111
alignment = true

[lint]
disable = ["one-class-per-file"]

[files]
exclude = ["bin/**", "vendor/**"]
```

## Where it's read from

`rafiki` uses the nearest `rafiki.toml` in or above the first path you pass it,
so it finds the project's settings from anywhere inside the project. The
language server looks from the root of the workspace your editor opens.

To use a specific file, or none at all:

```sh
rafiki fmt --config path/to/rafiki.toml
rafiki fmt --no-config
```

## What takes precedence

A setting can come from three places, and each one overrides the one before it:

1. The default.
2. `rafiki.toml`.
3. A flag on the command line, or an option sent by your editor.

Only the settings you actually pass override anything, so `--line-width 80`
changes the width and keeps the rest of the file. A list given as a flag
replaces the list from the file instead of adding to it, so
`--disable naming-convention` disables exactly that one rule.

## Mistakes in the file

An unknown key is an error, so a typo doesn't silently do nothing:

```text
rafiki: rafiki.toml: TOML parse error at line 2, column 1
  |
2 | line_width = 40
  | ^^^^^^^^^^
unknown field `line_width`, expected `line-width` or `alignment`
```

On the command line that stops the run with exit code `2`. The language server
shows the error in your editor instead and carries on with the defaults, so you
keep your diagnostics while you fix the file.
