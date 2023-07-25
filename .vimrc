" Az' vimrc, take MKLLVXII.

set shell=/bin/bash " Vim chokes on fish.
set nocompatible    " be iMproved, required

" Setting up Vundle - the vim plugin bundler
let vundle_readme=expand('~/.vim/bundle/vundle/README.md')
if !filereadable(vundle_readme)
    silent !mkdir -p ~/.vim/bundle
    silent !git clone https://github.com/VundleVim/Vundle.vim ~/.vim/bundle/vundle
endif

" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

filetype off                  " required
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'
" vim 'infrastructure'
Plugin 'junegunn/goyo.vim'                " Lightroom-like functionality.

" Code Navigation
Plugin 'junegunn/fzf'                     " Fuzzy File Finder, replacement for command-t
Plugin 'junegunn/fzf.vim'                 " Adds FZF vim bindings for Extra Shit
Plugin 'tpope/vim-unimpaired'             " A lot of very useful paired motions.
Plugin 'tpope/vim-surround'               " Makes changing delimiters far less of a pain.
Plugin 'tpope/vim-dispatch'               " non-focus stealing builds/tests hooray!
Plugin 'tpope/vim-commentary'             " Makes commenting not a pain.
Plugin 'tpope/vim-endwise'                " automatically adds 'end' and similar to certain languages.

" Themes and colorschemes.
Plugin 'flazz/vim-colorschemes'           " Giant-ass collection because why not.

call vundle#end()                         " required
filetype plugin indent on                 " required

set autoread                   " automatically read file-changes from disk.
set showmatch                  " Matching brackets.
set showcmd                    " Shadowing partial commands for completion!
set backspace=indent,eol,start " Allow Backspace to delete everythng.
set expandtab                  " We use spaces here.
set autoindent                 " You can't escape
set incsearch                  " search while typing, not just after hitting CR
set hlsearch                   " Highlight search terms
set lazyredraw                 " Make vim redraw the screen less
set wildmenu                   " Visual tab complete menu.
set number                     " And I like to see my numbers.

" Remaps.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let mapleader=' '
nnoremap <leader>evm :e ~/dotfiles/.vimrc<CR>
inoremap ± <c-o>~
nnoremap ; :
nnoremap : ;
nnoremap <leader>ln :lnext<CR>
nnoremap <leader>lp :lprevious<CR>

" clear search highlights
nnoremap <leader>ch :noh<CR>

" Kills WhiteSpace, at the end of the line.
nnoremap <leader>kws :%s/\s\+$//e<CR>

" Remaps Q to 'run last macro used'
nnoremap Q @@

" Remaps backspace and CR to be about paragraph wise handling instead of doing
" nothing.
nnoremap <BS> {
onoremap <BS> {
vnoremap <BS> {

nnoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
onoremap <expr> <CR> empty(&buftype) ? '}' : '<CR>'
vnoremap <CR> }

" Vim dispatch
nnoremap <leader>ro <ESC>:w<CR>:Dispatch<CR><CR>
nnoremap <leader>rh <ESC>:w<CR>:Dispatch!<CR><CR>

" FZF config and remaps
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_command_prefix = 'Fzf'
let g:fzf_layout         = { 'down': '~20%' }
let g:fzf_tags_command   = 'ctags -R'
let g:fzf_history_dir    = '~/.fzf/history'

nnoremap <leader>tf  :FzfFiles<CR>
nnoremap <leader>tgf :FzfGitFiles<CR>
nnoremap <leader>tt  :FzfTags<CR>
nnoremap <leader>tgg :!ctags -R<CR><CR>
nnoremap <leader>;   :w<CR>:FzfBuffers<CR>
nnoremap <leader>th  :FzfHistory<CR>
" Search Word
nnoremap <leader>w   :FzfAg<CR>
" Search word under cursor
nnoremap <leader>tw  :FzfAg <C-R><C-W><CR>
nnoremap <leader>gs  :FzfGFiles?<CR>
nnoremap <leader>ts  :FzfSnippets<CR>
nnoremap <leader>hh  :FzfHelptags<CR>
