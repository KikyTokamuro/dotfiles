" Tabs
set expandtab
set smarttab
set tabstop=4
set softtabstop=4
set shiftwidth=4

" Numbers
set number

" Syntax
syntax on

" Mute bells
set noerrorbells
set novisualbell

" Mouse
set mouse=a

" Search
set ignorecase
set smartcase
set hlsearch
set incsearch

" Encoding
set encoding=utf8
set ffs=unix,dos,mac

" Fold
set foldenable
set foldlevel=100
set foldmethod=indent
set foldcolumn=2

" ShowMatch
set showmatch

" Show current command
set showcmd

" Show 80 char column
set colorcolumn=80

" Gui panel
set guioptions-=T
"set guioptions-=m
set guioptions-=r
set guioptions-=L

" Font
set guifont=Hack\ 10

" Cursorline
set cursorline

" Statusline
set laststatus=2

" Indent
set autoindent
set smartindent

filetype plugin on

" Install vim-plug
if empty(glob('~/.vim/autoload/plug.vim'))
    silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs \
    https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
endif

" Vim-Plug plugins
call plug#begin('~/.vim/plugged')
    Plug 'joshdick/onedark.vim'
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
    Plug 'ervandew/supertab'
    Plug 'itchyny/lightline.vim'
call plug#end()

" lightline settings
let g:lightline = {
      \ 'colorscheme': 'one',
      \ }

" Colorscheme
colorscheme onedark
if (has("termguicolors"))
    set termguicolors
endif

" vim-go settings
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1

" Auto close preview window
augroup completion_preview_close
  autocmd!
  autocmd CompleteDone * if !&previewwindow && &completeopt =~ 'preview' | silent! pclose | endif
augroup END
