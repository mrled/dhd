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


