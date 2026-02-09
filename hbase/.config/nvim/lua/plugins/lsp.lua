-- LSP configuration
return {
  -- Configure Mason to ensure LSP servers are installed
  {
    "mason-org/mason.nvim",
    opts = {
      ensure_installed = {
        "ansible-language-server",
        "bash-language-server",
        "css-lsp",
        "docker-compose-language-service",
        "dockerfile-language-server",
        "harper-ls",
        "json-lsp",
        "lua-language-server",
        "marksman",
        "terraform-ls",
        "yaml-language-server",
        -- Note: salt_ls may need manual installation if not in Mason registry
      },
    },
  },

  -- Configure LSP servers
  {
    "neovim/nvim-lspconfig",
    opts = {
      -- LSP Server Settings
      servers = {
        ansiblels = {},
        bashls = {},
        cssls = {},
        dockerls = {},
        harper_ls = {
          filetypes = { "markdown", "text" },
        },
        jsonls = {},
        marksman = {},
        salt_ls = {},
        lua_ls = {
          settings = {
            Lua = {
              diagnostics = {
                globals = {
                  "vim",
                },
              },
              telemetry = {
                enable = false,
              },
            },
          },
        },
        terraformls = {},
        yamlls = {},
      },
    },
    keys = {
      -- LSP buffer keymaps (these get set when LSP attaches)
      { "gD", vim.lsp.buf.declaration, desc = "Go to Declaration" },
      { "gd", vim.lsp.buf.definition, desc = "Go to Definition" },
      { "K", vim.lsp.buf.hover, desc = "Hover Documentation" },
      { "gi", vim.lsp.buf.implementation, desc = "Go to Implementation" },
      { "<C-k>", vim.lsp.buf.signature_help, desc = "Signature Help", mode = "n" },
      { "<leader>wa", vim.lsp.buf.add_workspace_folder, desc = "Add Workspace Folder" },
      { "<leader>wr", vim.lsp.buf.remove_workspace_folder, desc = "Remove Workspace Folder" },
      { "<leader>wl", function() print(vim.inspect(vim.lsp.buf.list_workspace_folders())) end, desc = "List Workspace Folders" },
      { "<leader>D", vim.lsp.buf.type_definition, desc = "Type Definition" },
      { "<leader>rn", vim.lsp.buf.rename, desc = "Rename Symbol" },
      { "<leader>ca", vim.lsp.buf.code_action, desc = "Code Action" },
      { "gr", vim.lsp.buf.references, desc = "References" },
      { "<leader>f", function() vim.lsp.buf.format({ async = true }) end, desc = "Format Document" },
    },
  },
}
