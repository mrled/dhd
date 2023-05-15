-- Keyboard mappings, requires all other plugins to be loaded
--
-- if you see a vim setting like 'nmap ...', you can use nvim_set_keymap('n', ...),
-- like: vim.api.nvim_set_keymap('n', '<leader>t', 'Neotree')
-- <https://vi.stackexchange.com/questions/37187/how-can-i-port-nmap-map-and-imap-commands-to-lua-configuration>
--
-- If you want to add a keymap for some command, like neo-tree's :Neotree,
-- you can add it for <Cmd>Neotree<CR>.

-- built-in
--
-- Create a new file like :e, but relative to the directory of the currently-open file
vim.keymap.set('n', '<leader>e', '<C-R=expand("%:p:h") . "/" <CR>')


-- telescope
--
local tele_builtin = require('telescope.builtin')
local tele_actions = require "telescope.actions"
--
-- When doing ff, enter will load the file into the current buffer
--vim.keymap.set('n', '<leader>ff', tele_builtin.find_files, {})
-- hidden=true means look in .hidden directories too
-- Instead of enter, hit ctrl-t and it will load it in a new buffer (new "tab")
-- ctrl-x will load it in a new split
-- ctrl-v will load it in a new vsplit
vim.keymap.set('n', '<leader>ff', function() tele_builtin.find_files({hidden=true}) end)
vim.keymap.set('n', '<leader>fg', tele_builtin.live_grep, {})
vim.keymap.set('n', '<leader>fb', function() tele_builtin.buffers({sort_lastused=true}) end)
vim.keymap.set('n', '<leader>fh', tele_builtin.help_tags, {})
--
require("telescope").setup {
  pickers = {
    buffers = {
      mappings = {
        i = {
          -- delete a buffer with c-d
          ["<c-d>"] = tele_actions.delete_buffer + tele_actions.move_to_top,
        }
      }
    }
  }
}

-- neo-tree
--
-- If you use this to open Neotree, it'll keep the focus in the Neotree window.
-- You can 'C-w p' to put focus in the window you were in before opening Neotree
-- (standard neovim keyboard shortcut).
vim.keymap.set('n', '<leader>t', '<Cmd>Neotree toggle<CR>')
--
-- Open a file relative to the current buffer's path
vim.api.nvim_set_keymap('n', '<leader>e', ':e ' .. vim.fn.expand("%:p:h") .. '/', {noremap = true, silent = true})
--vim.api.nvim_set_keymap('n', '<leader>t', ':tabe ' .. vim.fn.expand("%:p:h") .. '/', {noremap = true, silent = true})
--vim.api.nvim_set_keymap('n', '<leader>s', ':split ' .. vim.fn.expand("%:p:h") .. '/', {noremap = true, silent = true})

