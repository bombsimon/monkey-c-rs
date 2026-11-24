# Neovim

With Neovim 0.11 or later no plugin is needed:

```lua
vim.filetype.add({ extension = { mc = "monkeyc" } })

vim.lsp.config("rafiki", {
  cmd = { "rafiki", "server" },
  filetypes = { "monkeyc" },
  root_markers = { "rafiki.toml", "manifest.xml", ".git" },
})

vim.lsp.enable("rafiki")
```

Open a `.mc` file and diagnostics show up as you edit. Settings are best kept in
the project's `rafiki.toml`, but you can override the formatting ones by adding
`init_options = { lineWidth = 100 }` to `vim.lsp.config`.

## Format and fix on save

```lua
vim.api.nvim_create_autocmd("BufWritePre", {
  pattern = "*.mc",
  callback = function()
    local client = vim.lsp.get_clients({ bufnr = 0, name = "rafiki" })[1]
    if not client then
      return
    end

    local params = vim.lsp.util.make_range_params(0, client.offset_encoding)
    params.context = { diagnostics = {}, only = { "source.fixAll" } }

    local response = client:request_sync("textDocument/codeAction", params)
    for _, action in ipairs(response and response.result or {}) do
      vim.lsp.util.apply_workspace_edit(action.edit, client.offset_encoding)
    end

    vim.lsp.buf.format({ id = client.id })
  end,
})
```

The fixes are requested directly since `vim.lsp.buf.code_action()` doesn't wait
for them before the file is formatted. To only format, call just
`vim.lsp.buf.format({ name = "rafiki" })`.

If you use a plugin that manages formatting, like conform.nvim or none-ls, make
sure it falls back to the language server for the `monkeyc` file type.

## Troubleshooting

- Check that the server is attached with `:checkhealth vim.lsp`. If it isn't,
  make sure `:set filetype?` prints `monkeyc`.
- If formatting on save does nothing, the file probably has a syntax error.
  `:lua =vim.diagnostic.get(0)` lists the diagnostics.
- Run `:lsp restart rafiki` (Neovim 0.12+) or restart Neovim after installing a
  new version of `rafiki` or changing `rafiki.toml`.
- For more detail, run `:lua vim.lsp.set_log_level("debug")` and read the log at
  `vim.lsp.get_log_path()`.
