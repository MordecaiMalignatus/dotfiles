
" Az' vimrc, take MKLLVXII.

set shell=/usr/local/bin/zsh " This solely so vim doesn't shit itself with fish.
set nocompatible             " be iMproved, required
syntax on                    " We ain't found shit!

set showmatch                " Matching brackets.
set showcmd                  " Shadowing partial commands for completion!

" Vundle! :D
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

filetype off                  " required
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

Plugin 'tpope/vim-fugitive'
Plugin 'git://git.wincent.com/command-t.git'
Plugin 'junegunn/vim-easy-align'
Plugin 'jnurmine/Zenburn'
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
Plugin 'airblade/vim-gitgutter' 

call vundle#end()            " required
filetype plugin indent on    " required

set expandtab                     " We use spaces here.
set tabstop=2                     " And they're two spaces. Because Scala.
set softtabstop=2                 " Because Scala.
set shiftwidth=2                  " Scala aint changing soon sonny.
set autoindent                    " You can't escape.

set foldenable                    " Make shit orderly.
set cursorline                    " I do like to find my cursor
set number                        " And I like to see my numbers.
set relativenumber                " And I want vim motions to be usable.
set t_Co=256                      " Terminal stuff for Zenburn
colors zenburn                    " Be pretty

let g:airline_theme='zenburn'     " Make our powerline suit the theme at hand.
let g:airline_powerline_fonts = 1 " And make it pretty.
set laststatus=2                  " And make it... work.

" Remaps.
let mapleader=' ' " we emacs now. 

nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gc :Gcommit<CR>
nnoremap <leader>gl :Glog<CR><CR><CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gg :!tig<CR>

nnoremap <leader>cf :EasyAlign
vnoremap <leader>cf :EasyAlign
