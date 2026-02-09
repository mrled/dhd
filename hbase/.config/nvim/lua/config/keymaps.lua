-- Keymaps
-- This file is automatically loaded by config.lazy

-- Create a new file like :e, but relative to the directory of the currently-open file
vim.keymap.set('n', '<leader>e', '<C-R>=expand("%:p:h") . "/" <CR>', { desc = "Edit file relative to current buffer" })
