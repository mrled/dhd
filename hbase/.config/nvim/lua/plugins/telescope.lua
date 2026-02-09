-- Telescope configuration
return {
  {
    "nvim-telescope/telescope.nvim",
    keys = {
      -- When doing ff, enter will load the file into the current buffer
      -- hidden=true means look in .hidden directories too
      -- Instead of enter, hit ctrl-t and it will load it in a new buffer (new "tab")
      -- ctrl-x will load it in a new split
      -- ctrl-v will load it in a new vsplit
      { "<leader>ff", function() require("telescope.builtin").find_files({ hidden = true }) end, desc = "Find files (including hidden)" },
      { "<leader>fg", function() require("telescope.builtin").live_grep() end, desc = "Grep in files" },
      { "<leader>fb", function() require("telescope.builtin").buffers({ sort_lastused = true }) end, desc = "Find buffers" },
      { "<leader>fh", function() require("telescope.builtin").help_tags() end, desc = "Find help tags" },
    },
    opts = {
      defaults = {
        file_ignore_patterns = {
          ".git/",
          "node_modules/",
        },
      },
      pickers = {
        buffers = {
          mappings = {
            i = {
              -- Delete a buffer with c-d
              ["<c-d>"] = function(...)
                local actions = require("telescope.actions")
                return actions.delete_buffer(...) + actions.move_to_top(...)
              end,
            },
          },
        },
      },
    },
  },
}
