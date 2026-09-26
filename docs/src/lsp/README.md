# Editor integration

`rafiki server` is a [language server][lsp], so any editor that supports the
protocol can use the formatter and linter while you work. It gives you:

- Lint findings and syntax errors as you type.
- Formatting of the whole file, for example on save.
- Lint fixes as code actions, either for a single finding or for the whole file.

The server is meant to run next to the language server that comes with the
Connect IQ SDK, which handles code navigation like going to a definition. It
doesn't try to replace it. Setup for Neovim is covered in [Neovim](neovim), and
[garmin-monkeyc.nvim] shows how to run both servers together.

## Setting it up

Point your editor at the command `rafiki server` for `.mc` files. The server
talks over stdio and needs nothing else. There's no packaged extension for
VS Code yet, but an editor that can start a custom language server works the
same way.

## Settings

The server reads the same `rafiki.toml` as the command line, found from the root
of the workspace your editor opens. Your editor can also send the formatting
settings as `initializationOptions`, which then override the file. Their names
are listed under [Settings](../configuration/settings).

Settings are read when the server starts, so restart it after changing them.

## Good to know

- A file that doesn't parse can't be formatted, so formatting does nothing
  until the syntax error is fixed. The error shows up as a diagnostic.
- Each lint finding is a warning tagged with its rule name, like
  `unneeded-parens`.
- When two fixes overlap, fixing the whole file applies the first one. The
  other is applied the next time you fix the file.

[garmin-monkeyc.nvim]: https://github.com/bombsimon/garmin-monkeyc.nvim
[lsp]: https://microsoft.github.io/language-server-protocol/
