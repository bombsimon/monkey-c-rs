# Configuration

A project can pin its settings in a `rafiki.toml`. Both the [CLI](../cli/README.md)
and the [language server](../lsp/README.md) read it, so a file formatted from an
editor comes out exactly as `rafiki fmt` would write it.

Every key is optional. Setting none of them is the same as having no file at all,
and the defaults are what the tools use with no configuration anywhere.

```toml
[format]
line-width = 111
alignment = true
wrap-declarations = false

[lint]
enable = []
disable = ["one-class-per-file"]

[files]
exclude = ["bin/**", "vendor/**"]
respect-gitignore = true
```

## Discovery

The nearest `rafiki.toml` at or above the first path on the command line is used,
so `rafiki fmt src/Foo.mc` picks up the project's settings from anywhere inside
it. The language server walks up from the workspace root the editor reports.

Two flags override discovery:

```sh
rafiki fmt --config path/to/rafiki.toml   # use this file
rafiki fmt --no-config                    # use the built-in defaults
```

## Precedence

Three layers, each overriding the one before it:

1. The built-in defaults.
2. `rafiki.toml`.
3. The explicit request — a CLI flag, or the language server client's
   `initializationOptions`.

Only keys that are actually set overlay, so `--line-width 80` changes the width
and leaves everything else as the file had it. For lists, a flag replaces the
file's list rather than extending it, so `--disable naming-convention` gives
exactly that one rule and not the file's disables as well.

## `[format]`

| Key                 | Type    | Default | Meaning                                              |
| ------------------- | ------- | ------- | ---------------------------------------------------- |
| `line-width`        | integer | `111`   | Target width before a group is broken onto lines.    |
| `alignment`         | boolean | `true`  | Column-align separators across related entries.      |
| `wrap-declarations` | boolean | `false` | Break each binding of a multi-binding `var`/`const`. |

As flags: `--line-width`/`-l`, `--alignment`/`--no-alignment`,
`--wrap-declarations`/`-w`/`--no-wrap-declarations`.

## `[lint]`

| Key       | Type              | Default | Meaning                               |
| --------- | ----------------- | ------- | ------------------------------------- |
| `enable`  | list of rule name | all     | When non-empty, only these rules run. |
| `disable` | list of rule name | none    | These rules are silenced.             |

A rule in both lists is silenced. Because each flag replaces only its own key,
that also holds across layers: with `disable = ["naming-convention"]` in the
file, `--enable naming-convention` still reports nothing, since the file's
`disable` is untouched and wins. Pass `--no-config` to start from a clean slate.

A name no rule answers to is an error rather than a no-op — a misspelling in
`enable` would otherwise silence everything and look like a clean run.
`rafiki lint --list-rules` prints the valid names, and they are documented under
[Rules](../linter/rules/README.md).

As flags: `--enable`, `--disable`, both comma-separated and repeatable.

## `[files]`

| Key                 | Type         | Default | Meaning                              |
| ------------------- | ------------ | ------- | ------------------------------------ |
| `exclude`           | list of glob | none    | Paths to skip, in gitignore syntax.  |
| `respect-gitignore` | boolean      | `true`  | Skip whatever `.gitignore` excludes. |

Globs are relative to the configuration file's directory. Hidden directories are
always skipped, whatever `respect-gitignore` says: turning it off asks for
ignored source files, not for `.git/`.

Globs passed directly with `--exclude` are relative to the current working
directory.

As flags: `--exclude` (repeatable), `--respect-gitignore`/`--no-respect-gitignore`.

## Errors

Unknown keys are rejected rather than ignored, so a typo is reported with the
keys that were expected:

```text
rafiki: rafiki.toml: TOML parse error at line 2, column 1
  |
2 | line_width = 40
  | ^^^^^^^^^^
unknown field `line_width`, expected one of `line-width`, `alignment`, `wrap-declarations`
```

The CLI treats that as a fatal error (exit code `2`). The language server instead
reports it to the editor with `window/showMessage` and carries on with the
defaults, since a bad configuration file should not leave you without
diagnostics.
