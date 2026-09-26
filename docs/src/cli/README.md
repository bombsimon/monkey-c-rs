# CLI

Everything runs through the `rafiki` command:

| Command              | Does                                                 |
| -------------------- | ---------------------------------------------------- |
| `rafiki fmt`         | Formats source files                                 |
| `rafiki lint`        | Reports lint findings, and fixes them with `--fix`   |
| `rafiki server`      | Runs the [language server](../lsp) on stdio          |
| `rafiki coverage`    | Measures [test coverage](../coverage)                |
| `rafiki completions` | Prints a shell completion script                     |

Every command has `--help`. The flags for each setting are listed under
[Settings](../configuration/settings).

## Choosing files

`rafiki fmt` and `rafiki lint` take any mix of files and directories, and use
the current directory when given none:

```sh
rafiki fmt
rafiki fmt source/ tests/Tests.mc
```

Directories are searched for `.mc` files. A file you name directly is used
whatever its extension. Hidden directories and anything `.gitignore` excludes
are skipped, see [`[files]`](../configuration/settings#files) to change that.

Pass `-` to read from stdin instead:

```sh
cat Clock.mc | rafiki fmt -
cat Clock.mc | rafiki lint -
```

The formatted code goes to stdout and findings go to stderr. `--fix` has no
effect on stdin.

## Checking without writing

`rafiki fmt` writes the formatted code back to each file. To only report what
would change:

```sh
rafiki fmt --check
rafiki fmt --diff
```

`--check` lists the files that aren't formatted, and `--diff` prints the
changes.

## Exit codes

| Code | Meaning                                                             |
| ---- | ------------------------------------------------------------------- |
| `0`  | Nothing to report.                                                  |
| `1`  | Something was found, like an unformatted file or a lint finding.    |
| `2`  | The command couldn't run, for example because of a bad argument.    |

Keeping `1` and `2` apart lets CI tell failing checks from a broken setup.

## Color

Output is colored when it goes to a terminal. `--color always` or
`--color never` overrides that, and the `NO_COLOR`, `CLICOLOR_FORCE` and
`TERM=dumb` environment variables are respected.

## Shell completions

```sh
rafiki completions zsh  > "${fpath[1]}/_rafiki"
rafiki completions bash > ~/.local/share/bash-completion/completions/rafiki
rafiki completions fish > ~/.config/fish/completions/rafiki.fish
```

`elvish` and `powershell` are supported too.
