-- Keymaps
-- This file is automatically loaded by config.lazy

-- Create a new file like :e, but relative to the directory of the currently-open file
vim.keymap.set('n', '<leader>E', '<C-R>=expand("%:p:h") . "/" <CR>', { desc = "Edit file relative to current buffer" })

-- Save without formatting
vim.keymap.set('n', '<leader>W', '<cmd>noautocmd write<cr>', { desc = "Save without formatting" })

-- Diagnostic keymaps
vim.keymap.set('n', '<leader>e', vim.diagnostic.open_float, { desc = "Show line diagnostics" })
vim.keymap.set('n', '[d', vim.diagnostic.goto_prev, { desc = "Previous diagnostic" })
vim.keymap.set('n', ']d', vim.diagnostic.goto_next, { desc = "Next diagnostic" })
vim.keymap.set('n', '<leader>q', vim.diagnostic.setloclist, { desc = "Diagnostics to loclist" })
