-- LSP stuff, specifically mason and lsp-config
--

-- LSP mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local lsp_opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, lsp_opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, lsp_opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, lsp_opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, lsp_opts)

-- LSP on_attach function
-- Use to only map the following keys
-- after the language server attaches to the current buffer
local lsp_on_attach = function(client, bufnr)
  -- Enable completion triggered by <c-x><c-o>
  vim.api.nvim_buf_set_option(bufnr, 'omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- Mappings.
  -- See `:help vim.lsp.*` for documentation on any of the below functions
  local bufopts = { noremap=true, silent=true, buffer=bufnr }
  vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, bufopts)
  vim.keymap.set('n', 'gd', vim.lsp.buf.definition, bufopts)
  vim.keymap.set('n', 'K', vim.lsp.buf.hover, bufopts)
  vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, bufopts)
  vim.keymap.set('n', '<C-k>', vim.lsp.buf.signature_help, bufopts)
  vim.keymap.set('n', '<space>wa', vim.lsp.buf.add_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wr', vim.lsp.buf.remove_workspace_folder, bufopts)
  vim.keymap.set('n', '<space>wl', function()
    print(vim.inspect(vim.lsp.buf.list_workspace_folders()))
  end, bufopts)
  vim.keymap.set('n', '<space>D', vim.lsp.buf.type_definition, bufopts)
  vim.keymap.set('n', '<space>rn', vim.lsp.buf.rename, bufopts)
  vim.keymap.set('n', '<space>ca', vim.lsp.buf.code_action, bufopts)
  vim.keymap.set('n', 'gr', vim.lsp.buf.references, bufopts)
  vim.keymap.set('n', '<space>f', function() vim.lsp.buf.format { async = true } end, bufopts)
end

local lsp_flags = {}

-- Require the various language servers

-- order is important for these:
require("mason").setup()
require("mason-lspconfig").setup()
-- these can be in any order:
require("lspconfig").ansiblels.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").bashls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").cssls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").dockerls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").jsonls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").marksman.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").salt_ls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").lua_ls.setup {
  on_attach=lsp_on_attach,
  flags=lsp_flags,
  settings = {
      Lua = {
          diagnostics = {
              globals = {
                  'vim'
              }
          }
      }
  }
}
--require("lspconfig").tailwindcss.setup {}
require("lspconfig").terraformls.setup {on_attach=lsp_on_attach, flags=lsp_flags}
require("lspconfig").yamlls.setup {on_attach=lsp_on_attach, flags=lsp_flags}


-- python
-- markdown
-- javascript/typescript ?
-- html
-- terraform

