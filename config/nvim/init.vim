call plug#begin('~/.local/share/nvim/plugged')

 " Themes
  Plug 'vim-airline/vim-airline'
  Plug 'vim-airline/vim-airline-themes'
  Plug 'tomasiser/vim-code-dark'

  " Fuzzy find
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'

call plug#end()

" Default settings
syntax on
set encoding=utf-8
set termguicolors
colorscheme codedark
set number relativenumber
set signcolumn=yes
set mouse=a
set ignorecase
set smartcase
let mapleader = ","

" Tab Settings
set shiftwidth=2
set softtabstop=2
set tabstop=1
set expandtab

" Cursor
set cursorline

" Fzf mappings
nnoremap <silent> <Leader>s :call fzf#run({
\   'down': '40%',
\   'sink': 'botright split' })<CR>

nnoremap <silent> <Leader>v :call fzf#run({
\   'right': winwidth('.') / 2,
\   'sink': 'vertical botright split' })<CR>
