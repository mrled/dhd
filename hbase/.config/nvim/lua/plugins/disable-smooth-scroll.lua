-- Disable smooth scrolling
return {
  {
    "nvim-mini/mini.animate",
    enabled = false,
  },
  {
    "folke/snacks.nvim",
    opts = {
      scroll = { enabled = false },
    },
  },
}
