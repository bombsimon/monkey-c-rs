# Neovim

Neovim has a built-in language server client, so no plugin is needed. It
doesn't know the `.mc` extension, though, so the setup registers the file type
and then starts the server for it:

```lua
vim.filetype.add({ extension = { mc = "monkeyc" } })

vim.api.nvim_create_autocmd("FileType", {
  pattern = "monkeyc",
  callback = function(args)
    vim.lsp.start({
      name = "rafiki",
      cmd = { "rafiki", "server" },
      root_dir = vim.fs.root(args.buf, { "rafiki.toml", "manifest.xml", ".git" }) or vim.fn.getcwd(),
    })
  end,
})
```

Open a `.mc` file and diagnostics show up as you edit. Settings are best kept in
the project's `rafiki.toml`, but you can override the formatting ones by adding
`init_options = { lineWidth = 100 }` to `vim.lsp.start`.

## Format and fix on save

This applies every lint fix and then formats the file each time it's saved:

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

Leave out the `code_action` call to only format. Fixes for a single finding are
still available on demand through `vim.lsp.buf.code_action()`.

If you use a plugin that manages formatting, like conform.nvim or none-ls, make
sure it falls back to the language server for the `monkeyc` file type.

## Troubleshooting

- Check that the server is attached with `:checkhealth vim.lsp`. If it isn't,
  make sure `:set filetype?` prints `monkeyc`.
- If formatting on save does nothing, the file probably has a syntax error.
  `:lua =vim.diagnostic.get(0)` lists the diagnostics.
- Run `:LspRestart` after installing a new version of `rafiki` or changing
  `rafiki.toml`.
- For more detail, run `:lua vim.lsp.set_log_level("debug")` and read the log at
  `vim.lsp.get_log_path()`.
