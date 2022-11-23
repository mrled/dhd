" neovim config file

" Notes to self
" All basic stuff that I am just writing here so I don't forget it
"
" - leader key is \
" - reload the config file with :source ~/.dhd/hbase/.config/nvim/init.vim
" - :tab term ... opens a new buffer (tab) with a terminal in it

" case insensitive searches
set ignorecase
set smartcase
set autoindent
set expandtab
set tabstop=4
set shiftwidth=4

set list
set listchars=tab:␉·,trail:␠,nbsp:⎵
"set listchars=tab:␉·,trail:␠,nbsp:⎵
 
" Plugins will be downloaded under the specified directory.
call plug#begin('~/.config/nvim/plugged')
 
" Declare the list of plugins.
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-treesitter/nvim-treesitter'
Plug 'sharkdp/fd'
Plug 'nvim-tree/nvim-web-devicons'
Plug 'nvim-telescope/telescope.nvim'
Plug 'tpope/vim-sensible'
Plug 'sainnhe/edge'
Plug 'sheerun/vim-polyglot'
 
" List ends here. Plugins become visible to Vim after this call.
call plug#end()
 
 
set showmatch
set noerrorbells
 
syntax on
set hlsearch
set termguicolors
set background=dark
colorscheme edge

" allow mouse scroll
set mouse=a

" For some reason this is required for fucking copy and paste from the OS
set clipboard^=unnamed,unnamedplus

if exists("g:neovide")
    let g:neovide_cursor_vfx_mode = "pixiedust"
    let g:neovide_cursor_vfx_particle_density = 50.0
    let g:neovide_cursor_vfx_particle_lifetime = 1.2
endif

" Telescope (ctrl-p, except not with that key combo)
"
" Notes
" - When doing ff, enter will load the file into the current buffer
" - Instead of enter, hit ctrl-t and it will load it in a new buffer (new "tab")
" - ctrl-x will load it in a new split
" - ctrl-v will load it in a new vsplit
"
" hidden=true means look in hidden directories too
nnoremap <leader>ff <cmd>Telescope find_files hidden=true<cr>
nnoremap <leader>fg <cmd>Telescope live_grep<cr>
nnoremap <leader>fb <cmd>Telescope buffers<cr>
nnoremap <leader>fh <cmd>Telescope help_tags<cr>

