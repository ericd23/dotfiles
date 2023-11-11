set nocompatible
syntax on
set number
set relativenumber
set viminfo=
set scrolloff=5

" Enable mouse
set mouse+=a

set shortmess+=I

" Always show status line
set laststatus=2

" Make backspace more reasonably
set backspace=indent,eol,start

" Enable hidden buffer
set hidden

" This setting makes search case-insensitive when all characters in the string
" being searched are lowercase. However, the search becomes case-sensitive if
" it contains any capital letters. This makes searching more convenient.
set ignorecase
set smartcase

set incsearch

" Unbind some useless/annoying default key bindings.
nmap Q <Nop> " 'Q' in normal mode enters Ex mode. You almost never want this.

" Disable audible bell because it's annoying.
set noerrorbells visualbell t_vb=

" Persistent indentation state
:vnoremap < <gv
:vnoremap > >gv

" Defaults to `+' regirster
set clipboard=unnamedplus

" Vim-plug
" call plug#begin()
" Plug 'tpope/vim-surround'
" Plug 'machakann/vim-highlightedyank'
" Plug 'tpope/vim-commentary'
" call plug#end()
