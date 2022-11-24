-- Main neovim init file

-- Put stuff that doesn't require plugins at the top
-- If plugins aren't working, at least the first part will load

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

if vim.fn.has('gui_running') then
  vim.cmd "colorscheme edge"
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
vim.keymap.set('n', '<leader>fg', builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', builtin.buffers, {})
vim.keymap.set('n', '<leader>fh', builtin.help_tags, {})

