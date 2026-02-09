-- Edge colorscheme configuration
return {
  {
    "sainnhe/edge",
    lazy = false,
    priority = 1000,
    config = function()
      -- Load the colorscheme
      vim.cmd([[colorscheme edge]])
    end,
  },
  -- Configure LazyVim to use edge as the default colorscheme
  {
    "LazyVim/LazyVim",
    opts = {
      colorscheme = "edge",
    },
  },
}
