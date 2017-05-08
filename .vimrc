"NeoBundle Scripts-----------------------------
if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=/Users/az/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('/Users/az/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:
NeoBundle 'tpope/vim-fugitive'      " git support
NeoBundle 'ctrlpvim/ctrlp.vim'      " codebase search
NeoBundle 'flazz/vim-colorschemes'  " extra themes
NeoBundle 'davidhalter/jedi-vim'    " python integration
NeoBundle 'derekwyatt/vim-scala'    " scala integration
NeoBundle 'elixir-lang/vim-elixir'  " elixir integration.
NeoBundle 'vim-ruby/vim-ruby'       " Ruby integration.
NeoBundle 'Valloric/YouCompleteMe'  " autocompletion for a lot of languages
NeoBundle 'oblitum/rainbow'         " rainbow braces
NeoBundle 'fatih/vim-go'            " go integration
NeoBundle 'bling/vim-airline'       " status bar
NeoBundle 'tpope/vim-surround'      " command to surround selection with text
NeoBundle 'tpope/vim-commentary'    " command to comment out stuff
NeoBundle 'airblade/vim-gitgutter'  " git gutter

" Required:
call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------

set nocp

set smartindent
set tabstop=4
set shiftwidth=4
set expandtab

filetype on
syntax on

" Colour Schemes
set t_Co=256
set number
set backspace=indent,eol,start
colorscheme desert
hi LineNr ctermfg=grey

" Encodings
autocmd BufWritePre * :%s/\s\+$//e
set encoding=utf-8

" Status Bar
set laststatus=2
let g:airline_powerline_fonts = 1

" Rainbow brackets
let g:rainbow_active = 1

" Keymaps
nnoremap <F2> :set nu!<CR>
nnoremap <F3> :set paste!<CR>
nnoremap <F4> :GitGutterToggle<CR>
au FileType tex map <F5> :w<CR> :exec "!pdflatex %"<CR> :exec "!pdflatex %"<CR><CR>
au FileType python map <F5> :w<CR> :exec "!python %"<CR>
nmap <F8> :TagbarToggle<CR>

autocmd BufRead,BufNewFile *.csdl set filetype=csdl
