-- Disable smooth scrolling
return {
  {
    "echasnovski/mini.animate",
    enabled = false,
  },
  {
    "folke/snacks.nvim",
    opts = {
      scroll = { enabled = false },
    },
  },
}
