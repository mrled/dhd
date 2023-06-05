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


-- Disable netrw before doing anything else
-- Recommended by the nvim-tree.lua docs
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- case insensitive searches
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.autoindent = true
vim.opt.expandtab = true

vim.opt.guifont="FiraCode Nerd Font Mono:h10:w57"

local indent = 4
vim.opt.tabstop = indent
vim.opt.shiftwidth = indent

--vim.opt.list = true
--vim.opt.listchars = "tab:␉·,trail:␠,nbsp:⎵"

vim.opt.showmatch = true
vim.opt.errorbells = false

vim.opt.hlsearch = true
vim.opt.background = "dark"

-- Use left-most colum to do things like report errors.
-- Without this, it will insert a column whenever your lang server
-- wants to display that a line has an error, shifting everything right.
vim.opt.signcolumn = "yes"

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
  vim.g.neovide_padding_left = 25
end

require('mrl-telescope')
require("mrl-mason")
require("mrl-keymap")
