-- Main neovim init file

-- Put stuff that doesn't require plugins at the top
-- If plugins aren't working, at least the first part will load

-- Reminders/docs/notes-to-self
--
-- 1. In vimscript, 0 is falsy and every other number is truthy since there is no boolean.
--    In Lua, only false and nil express to false, anything else expresses to true (including 0).
--    !!!!!!! ZERO IS TRUE IN LUA, FOR FUCK'S SAKE !!!!!!!
--    Take special care with translating eg "if has(...)" in vimscript,
--    which is a vimscript-native function that returns 0 or 1,
--    to "if vim.fn.has(...)" in lua, because if has() returns 0, lua will interpret it as true.
--    Many such cases.

-- case insensitive searches
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.autoindent = true
vim.opt.expandtab = true

local indent = 4
vim.opt.tabstop = indent
vim.opt.shiftwidth = indent

vim.opt.list = true
vim.opt.listchars = "tab:␉·,trail:␠,nbsp:⎵"

vim.opt.showmatch = true
vim.opt.errorbells = false

vim.opt.hlsearch = true
vim.opt.termguicolors = true
vim.opt.background = "dark"

-- allow mouse scroll
vim.opt.mouse = "a"

-- For some reason this is required for fucking copy and paste from the OS
--set clipboard^=unnamed,unnamedplus
vim.opt.clipboard = "unnamed,unnamedplus"

require('plugins')

-- neovim GUIs are... fucking annoying about how to support GUI-only configuration.
-- gvim looks for a ginit.vim, but that isn't consitent with all frontends,
-- eg vimr doesn't support ginit.<anything>, and neovide supports ginit.vim but not ginit.lua;
-- vimr sets has('gui_running') to 1, but neovide doesn't...
-- so we have to do this.
-- Sad!
if vim.fn.has('gui_running') == 1 or vim.g.neovide then
  vim.cmd("colorscheme edge")
end

if vim.g.neovide then
  vim.g.neovide_cursor_vfx_mode = "pixiedust"
  vim.g.neovide_cursor_vfx_particle_density = 50.0
  vim.g.neovide_cursor_vfx_particle_lifetime = 1.2
end

-- Telescope (ctrl-p, except not with that key combo)
--
-- Notes
--
-- - When doing ff, enter will load the file into the current buffer
-- - Instead of enter, hit ctrl-t and it will load it in a new buffer (new "tab")
-- - ctrl-x will load it in a new split
-- - ctrl-v will load it in a new vsplit
--
--
local builtin = require('telescope.builtin')
-- hidden=true means look in .hidden directories too
vim.keymap.set('n', '<leader>ff', function() builtin.find_files({hidden=true}) end)
vim.keymap.set('n', '<leader>fa', function() builtin.find_files({hidden=true}) end)
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

-- Mappings.
-- See `:help vim.diagnostic.*` for documentation on any of the below functions
local lsp_opts = { noremap=true, silent=true }
vim.keymap.set('n', '<space>e', vim.diagnostic.open_float, lsp_opts)
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, lsp_opts)
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, lsp_opts)
vim.keymap.set('n', '<space>q', vim.diagnostic.setloclist, lsp_opts)

-- Use an on_attach function to only map the following keys
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
require("lspconfig").sumneko_lua.setup {
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

