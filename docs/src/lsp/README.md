# LSP

`rafiki server` is a [Language Server][lsp] for Monkey C. It wraps the parser,
linter, and formatter in one server so any LSP-capable editor gets live feedback
without shelling out to the command line.

## Capabilities

| Feature      | LSP request                       | Backed by                              |
| ------------ | --------------------------------- | -------------------------------------- |
| Diagnostics  | `textDocument/publishDiagnostics` | parser (syntax errors), linter (lints) |
| Formatting   | `textDocument/formatting`         | formatter                              |
| Code actions | `textDocument/codeAction`         | linter fixes                           |

A parse error becomes a single error diagnostic. Every lint finding becomes a
warning tagged with its rule name, for example `unneeded-parens`. Formatting
returns the whole document re-rendered by the formatter.

There are two kinds of code action. A `quickfix` fixes one finding: invoke code
actions on a diagnostic, or anywhere on its line, and apply that single fix. A
`source.fixAll` action bundles every auto-fixable finding into one edit. Bind it
to run on save and it fixes the whole file at once (see below). If two fixes
would overlap, one pass keeps the first and drops the rest; the dropped ones
apply on the next run, the same way the linter's `--fix` does.

Positions are translated between the parser's UTF-8 byte offsets and LSP's
UTF-16 `(line, character)` coordinates, so diagnostics stay aligned on lines
that contain accents or emoji.

## Transport and sync

The server talks over stdio with standard LSP framing. Sync is full document:
the editor sends the complete text on every change. That keeps the server simple
and is fast enough for source files of the usual size.

## Configuration

The server reads the project's [`rafiki.toml`](../configuration/README.md),
discovered by walking up from the workspace root the editor reports. That is the
same file `rafiki fmt` reads, so formatting from an editor and formatting from a
terminal produce identical output.

A client can also pass settings directly through `initializationOptions`, which
override the file. Keys are camelCase there, to match LSP convention rather than
the file's kebab-case; unknown keys are ignored and any key left unset falls
through to the file and then to the default.

| Key                | `rafiki.toml`                | Type    | Default |
| ------------------ | ---------------------------- | ------- | ------- |
| `lineWidth`        | `[format] line-width`        | integer | `111`   |
| `alignment`        | `[format] alignment`         | boolean | `true`  |
| `wrapDeclarations` | `[format] wrap-declarations` | boolean | `false` |

Both sources are read at startup, so change either and restart the server
(`:LspRestart` in Neovim) to take effect. A `rafiki.toml` that fails to parse is
reported through `window/showMessage` and the defaults are used, so a broken file
never costs you diagnostics.

## Editor setup

The server is a plain stdio LSP program, so any client can launch it: point the
client at `rafiki server`. Nothing but LSP traffic is ever written to stdout, so
configuration problems and other messages cannot corrupt the stream.

Install rafiki as described in [CLI](../cli/README.md), or build it from a
checkout with `--release` — the program your editor spawns runs on every
keystroke:

```sh
cargo build --release
# creates target/release/rafiki
```

### Neovim

No plugin is needed; `vim.lsp.start` is built in. Neovim doesn't recognise the
`.mc` extension, so setup has two parts: register the filetype, then start the
server for it. Drop this in your config (for example `init.lua`) and point `cmd`
at the built binary:

```lua
-- Teach Neovim that .mc is Monkey C.
vim.filetype.add({ extension = { mc = "monkeyc" } })

-- Start the server whenever a Monkey C buffer opens.
vim.api.nvim_create_autocmd("FileType", {
  pattern = "monkeyc",
  callback = function(args)
    vim.lsp.start({
      name = "rafiki",
      cmd = { "rafiki", "server" },
      root_dir = vim.fs.root(args.buf, { "rafiki.toml", "manifest.xml", ".git" }) or vim.fn.getcwd(),
      -- Optional. Prefer a `rafiki.toml` in the project, so the command line
      -- and the editor agree; anything set here overrides it.
      init_options = {
        lineWidth = 111,
        alignment = true,
        wrapDeclarations = false,
      },
    })
  end,
})
```

Open a `.mc` file and diagnostics show up on their own. They refresh as you
edit, because the server re-analyses on every change.

### Format on save

The server advertises `textDocument/formatting`, so `vim.lsp.buf.format()`
routes through it:

```lua
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.mc",
  callback = function() vim.lsp.buf.format() end,
})
```

If you already have a global format-on-save, check two things. It must not be
filtered to another client (a `filter = function(c) return c.name == "..." end`
that leaves out `rafiki`). And formatter-manager plugins such as
conform.nvim or none-ls need to fall back to the LSP for the `monkeyc` filetype,
or they skip this server.

Formatting does nothing when the document doesn't parse. A file with a syntax
error can't be re-rendered from its AST, so the server returns no edits. If
format-on-save goes quiet, look for an error diagnostic first with
`:lua =vim.diagnostic.get(0)`.

### Fix all lints on save

To apply every auto-fixable lint on save, run the `source.fixAll` code action
before formatting:

```lua
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.mc",
  callback = function()
    vim.lsp.buf.code_action({
      apply = true,
      async = false,
      context = { only = { "source.fixAll" } },
    })
    vim.lsp.buf.format()
  end,
})
```

The server returns a single `source.fixAll` action whenever something is
fixable, so `apply = true` applies it without a prompt. With `only` set to
`source.fixAll`, per-finding quick-fixes stay out of the way here. Reach those on
demand with `vim.lsp.buf.code_action()` and no filter.

### Troubleshooting

- Check that the client attached and offers formatting:
  `:lua =vim.lsp.get_clients({ bufnr = 0 })[1].server_capabilities.documentFormattingProvider`
  should print `true`. An empty `get_clients` means nothing attached, so confirm
  the filetype is `monkeyc`.
- After you rebuild the binary, run `:LspRestart` (or reopen the buffer) so
  Neovim spawns the new process.
- For protocol-level debugging, raise the log level with
  `:lua vim.lsp.set_log_level("debug")` and read the file at
  `vim.lsp.get_log_path()`.

### Other editors

Any client that can register a custom stdio language server for the Monkey C
file type works the same way. Point it at the built binary.

[lsp]: https://microsoft.github.io/language-server-protocol/
