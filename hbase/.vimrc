
" fuck vi compatibility
set nocompatible

" case insensitive searches
set ignorecase
set smartcase
set autoindent

set showmatch
set noerrorbells

" allow backspacing over everything in insert mode
" allow backspacing and cursoring to previous/next line
set backspace=indent,eol,start whichwrap+=<,>,[,]

set ruler               " show the cursor position all the time
set showcmd             " display incomplete commands
set incsearch           " do incremental searching

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
" Also don't do it when the mark is in the first line, that is the default
" position when opening a file.
autocmd BufReadPost *
\ if line("'\"") > 1 && line("'\"") <= line("$") |
\   exe "normal! g`\"" |
\ endif

" For all text files set 'textwidth' to 80 characters.
autocmd FileType text setlocal textwidth=80

" Alt-Space is System menu
if has("gui")
  noremap <M-Space> :simalt ~<CR>
  inoremap <M-Space> <C-O>:simalt ~<CR>
  cnoremap <M-Space> <C-C>:simalt ~<CR>
endif

behave mswin

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if &t_Co > 2 || has("gui_running")
  syntax on
  set hlsearch
endif

