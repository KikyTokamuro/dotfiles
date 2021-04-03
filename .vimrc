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
set guifont=Hack\ 11

" Cursorline
set cursorline

" Statusline
set laststatus=2

" Indent
set autoindent
set smartindent

filetype plugin on

" Vim-Plug plugins
call plug#begin('~/.vim/plugged')
    " Nord colors
    Plug 'arcticicestudio/nord-vim'
    " Statusline
    Plug 'itchyny/lightline.vim'
    " NERDTree
    Plug 'preservim/nerdtree'
    " Golang
    Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }
    " C/C++
    Plug 'maralla/completor.vim'
call plug#end()

" lightline settings
let g:lightline = {
      \ 'colorscheme': 'nord',
      \ }

" Colorscheme
colorscheme nord
if (has("termguicolors"))
    set termguicolors
endif

" NERDTree
map <C-n> :NERDTreeToggle<CR>

" Golang
let g:go_fmt_command = "goimports"
let g:go_auto_type_info = 1

" C/C++
let g:completor_clang_binary = '/usr/bin/clang'
function! Tab_Or_Complete() abort
  if pumvisible()
    return "\<C-N>"
  elseif col('.')>1 && strpart( getline('.'), col('.')-2, 3 ) =~ '^[[:keyword:][:ident:]]'
    return "\<C-R>=completor#do('complete')\<CR>"
  else
    return "\<Tab>"
  endif
endfunction

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
let g:completor_auto_trigger = 0
inoremap <expr> <Tab> Tab_Or_Complete()

" Notepad like keys :)
"nmap <C-a> ggVG
vmap <C-c> "+y
nmap <C-c> "+yy
nnoremap <C-b> <C-v>
nnoremap <C-v> "+p
vnoremap <C-v> "+p
nnoremap <C-x> "+dd
vnoremap <C-x> "+d
