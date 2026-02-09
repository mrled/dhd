-- Neo-tree configuration
return {
  {
    "nvim-neo-tree/neo-tree.nvim",
    keys = {
      -- If you use this to open Neotree, it'll keep the focus in the Neotree window.
      -- You can 'C-w p' to put focus in the window you were in before opening Neotree
      -- (standard neovim keyboard shortcut).
      { "<leader>t", "<cmd>Neotree toggle<cr>", desc = "Toggle Neo-tree" },
    },
    opts = {
      default_component_configs = {
        icon = {
          folder_empty = "󰜌",
          folder_empty_open = "󰜌",
        },
        git_status = {
          symbols = {
            renamed = "󰁕",
            unstaged = "󰄱",
          },
        },
      },
      document_symbols = {
        kinds = {
          File = { icon = "󰈙", hl = "Tag" },
          Namespace = { icon = "󰌗", hl = "Include" },
          Package = { icon = "󰏖", hl = "Label" },
          Class = { icon = "󰌗", hl = "Include" },
          Property = { icon = "󰆧", hl = "@property" },
          Enum = { icon = "󰒻", hl = "@number" },
          Function = { icon = "󰊕", hl = "Function" },
          String = { icon = "󰀬", hl = "String" },
          Number = { icon = "󰎠", hl = "Number" },
          Array = { icon = "󰅪", hl = "Type" },
          Object = { icon = "󰅩", hl = "Type" },
          Key = { icon = "󰌋", hl = "" },
          Struct = { icon = "󰌗", hl = "Type" },
          Operator = { icon = "󰆕", hl = "Operator" },
          TypeParameter = { icon = "󰊄", hl = "Type" },
          StaticMethod = { icon = "󰠄 ", hl = "Function" },
        },
      },
      filesystem = {
        filtered_items = {
          hide_by_name = {
            "node_modules",
          },
          hide_dotfiles = false,
          never_show = {
            ".git",
          },
        },
        follow_current_file = {
          enabled = true,
        },
        use_libuv_file_watcher = true,
      },
    },
  },
}
