-- This file can be loaded by calling `lua require('plugins')` from your init.vim

return require('packer').startup(function(use)
  -- Packer can manage itself
  use 'wbthomason/packer.nvim'

  use "nvim-treesitter/nvim-treesitter"
  use "sharkdp/fd"
  use "nvim-tree/nvim-web-devicons"
  use "tpope/vim-sensible"
  use "sainnhe/edge"
  use "sheerun/vim-polyglot"

  use {'nvim-telescope/telescope.nvim', branch = '0.1.x', requires = { {'nvim-lua/plenary.nvim'} }}

end)

