" l7ssha vim config
" 29.12.2017

"Update file when edited from outside
set autoread

" Colors 
syntax enable 

set foldcolumn=1
" Tabs
set tabstop=4
set softtabstop=4
set expandtab
set nowrap
set smartindent
set autoindent

"show matching parenthesis
set showmatch

" Numbers +_+
set number
set relativenumber

" Some shit
set showcmd
set wildmenu
set lazyredraw

set showmatch

set incsearch
set hlsearch

set magic

"No sound on error
set noerrorbells
set novisualbell
set t_vb=
set tm=500

nnoremap j gj
nnoremap k gk

let mapleader=" "

"History od changes
set history=1000
set hidden

"allow backspacing over autoindent, line breaks
set backspace=indent,eol,start

"map Y to acty like dd
map Y y$

