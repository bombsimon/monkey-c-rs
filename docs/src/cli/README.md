# CLI

`rafiki` is the single command for everything in this project.

| Command              | Does                                                 |
| -------------------- | ---------------------------------------------------- |
| `rafiki fmt`         | Format source files                                  |
| `rafiki lint`        | Report and optionally fix lint findings              |
| `rafiki server`      | Run the [language server](../lsp/README.md) on stdio |
| `rafiki completions` | Print a shell completion script                      |

## Installing

Prebuilt binaries are not published yet, so install from source:

```sh
cargo install --git https://github.com/bombsimon/monkey-c-rs rafiki

# or from a checkout
cargo install --path rafiki
```

## Paths

`fmt` and `lint` take any mix of files and directories, defaulting to the current
directory:

```sh
rafiki fmt                     # the whole project
rafiki fmt src/ Tests.mc       # named paths
rafiki lint --fix src/
```

Directories are walked recursively for `.mc` files. A file named explicitly is
used whatever its extension, so an unusual name is always reachable. Hidden
directories are skipped, as is anything `.gitignore` excludes — pass
`--no-respect-gitignore` to include those, or `--exclude <glob>` to skip more.

`-` reads from stdin instead, which cannot be combined with paths:

```sh
cat File.mc | rafiki fmt -      # formatted source on stdout
cat File.mc | rafiki lint -     # findings on stderr
```

## Checking without writing

`rafiki fmt` rewrites files in place. Two flags report instead:

```sh
rafiki fmt --check     # names files that would change
rafiki fmt --diff      # prints a unified diff of the changes
```

## Exit codes

| Code | Meaning                                                                          |
| ---- | -------------------------------------------------------------------------------- |
| `0`  | Clean: nothing to report                                                         |
| `1`  | The check found something — unformatted files, lint findings                     |
| `2`  | The command could not run — bad arguments, unreadable file, broken configuration |

Separating `1` from `2` lets CI tell a failing check apart from a broken
invocation:

```sh
rafiki fmt --check && rafiki lint
```

## Colour

Diagnostics are coloured when the stream is a terminal. `--color always|never`
overrides that, and the `NO_COLOR`, `CLICOLOR_FORCE` and `TERM=dumb` conventions
are honoured. stdout and stderr are decided independently, so
`rafiki fmt --diff | less` stays plain even when diagnostics beside it are not.

## Shell completions

```sh
rafiki completions zsh  > "${fpath[1]}/_rafiki"
rafiki completions bash > ~/.local/share/bash-completion/completions/rafiki
rafiki completions fish > ~/.config/fish/completions/rafiki.fish
```

`elvish` and `powershell` work too. The scripts are generated from the same
definitions that parse the arguments, so they cannot describe flags that do not
exist.

## Configuration

Formatter settings, rule selection and file selection can all be pinned in a
[`rafiki.toml`](../configuration/README.md), which the language server reads too.
