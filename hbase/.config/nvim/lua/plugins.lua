-- This file can be loaded by calling `lua require('plugins')` from your init.vim

-- run :PackerCompile whenever this file changes
vim.cmd([[
  augroup packer_user_config
    autocmd!
    autocmd BufWritePost plugins.lua source <afile> | PackerCompile
  augroup end
]])




return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use "nvim-treesitter/nvim-treesitter"
  use "sharkdp/fd"
  use "nvim-tree/nvim-web-devicons"
  use "tpope/vim-sensible"
  use "tpope/vim-commentary"
  use "sainnhe/edge"
  use "sheerun/vim-polyglot"

  use {'nvim-telescope/telescope.nvim', branch = '0.1.x', requires = { {'nvim-lua/plenary.nvim'} }}

  use {
    "nvim-neo-tree/neo-tree.nvim",
      branch = "v2.x",
      requires = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons", -- not strictly required, but recommended
        "MunifTanjim/nui.nvim",
      },
      config = function()

        vim.cmd([[ let g:neo_tree_remove_legacy_commands = 1 ]])

        require("neo-tree").setup({
          default_component_configs = {
            icon = {
              folder_empty = "󰜌",
              folder_empty_open = "󰜌",
            },
            git_status = {
              symbols = {
                renamed   = "󰁕",
                unstaged  = "󰄱",
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
              StaticMethod = { icon = '󰠄 ', hl = 'Function' },
            }
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
            follow_current_file = true,
            use_libuv_file_watcher = true,
          },
        })
      end
    }

    -- use {'nvim-tree/nvim-tree.lua',
    --   requires = {
    --     'nvim-tree/nvim-web-devicons', -- optional, for file icons
    --   },
    -- }

    use "vim-airline/vim-airline"

    use "williamboman/mason.nvim"
    use "williamboman/mason-lspconfig.nvim"
    use "neovim/nvim-lspconfig"
  end)

