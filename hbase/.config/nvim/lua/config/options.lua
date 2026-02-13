-- Options
-- This file is automatically loaded by config.lazy

-- Disable netrw before doing anything else
-- Recommended by the nvim-tree.lua docs
vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

-- Search settings
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.hlsearch = true

-- Indentation (2 spaces)
vim.opt.autoindent = true
vim.opt.expandtab = true
local indent = 2
vim.opt.tabstop = indent
vim.opt.shiftwidth = indent

-- UI settings
vim.opt.showmatch = true
vim.opt.errorbells = false
vim.opt.background = "dark"

-- Soft line wrap
vim.opt.wrap = true
vim.opt.linebreak = true      -- Break at word boundaries
vim.opt.breakindent = true    -- Preserve indentation on wrapped lines

-- Use left-most column to do things like report errors.
-- Without this, it will insert a column whenever your lang server
-- wants to display that a line has an error, shifting everything right.
vim.opt.signcolumn = "yes"

-- Allow mouse scroll
vim.opt.mouse = "a"

-- Clipboard integration
-- For some reason this is required for copy and paste from the OS
vim.opt.clipboard = "unnamed,unnamedplus"

-- Font settings
vim.opt.guifont = "FiraCode Nerd Font Mono:h12:w57"

-- Neovide-specific settings
-- neovim GUIs are... fucking annoying about how to support GUI-only configuration.
-- gvim looks for a ginit.vim, but that isn't consistent with all frontends,
-- eg vimr doesn't support ginit.<anything>, and neovide supports ginit.vim but not ginit.lua;
-- vimr sets has('gui_running') to 1, but neovide doesn't...
-- so we have to do this.
if vim.g.neovide then
  vim.g.neovide_cursor_vfx_mode = "pixiedust"
  vim.g.neovide_cursor_vfx_particle_density = 50.0
  vim.g.neovide_cursor_vfx_particle_lifetime = 1.2
  vim.g.neovide_padding_left = 25
end
