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
