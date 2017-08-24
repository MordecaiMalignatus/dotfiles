" Az' vimrc, take MKLLVXII.

set shell=/bin/bash " Vim chokes on fish.
set nocompatible    " be iMproved, required
syntax on                   

" Vundle! :D
" set the runtime path to include Vundle and initialize
set rtp+=~/.vim/bundle/Vundle.vim

filetype off                  " required
call vundle#begin()
Plugin 'VundleVim/Vundle.vim'

" language specific things.
Plugin 'rust-lang/rust.vim'               " Racer/RLS integration.
Plugin 'plasticboy/vim-markdown'          " good markdown support.
Plugin 'vim-ruby/vim-ruby'                " Ruby. 

" Git Things.
Plugin 'tpope/vim-fugitive'               " Git integration by tpope. May get tossed.
Plugin 'airblade/vim-gitgutter'           " Shows changed/added/removed lines in gutter.

" Code Navigation
Plugin 'Valloric/YouCompleteMe'           " Generic expander, integrates with ultisnips
Plugin 'junegunn/fzf'                     " Fuzzy File Finder, replacement for command-t
Plugin 'junegunn/vim-easy-align'          " Make shit look pretty.
Plugin 'vim-syntastic/syntastic'          " Syntax checking for a lot of languages.
Plugin 'tpope/vim-surround'               " Makes changing delimiters far less of a pain.
Plugin 'tpope/vim-dispatch'               " non-focus stealing builds/tests hooray!
Plugin 'tpope/vim-commentary'             " Makes commenting not a pain.
Plugin 'godlygeek/tabular'                " Required for markdown.

" Snippets.
Plugin 'honza/vim-snippets'               " Snippet collection that comes in handy.
Plugin 'SirVer/ultisnips'                 " Snippet engine, integrates with YCM

" Themes and colorschemes.
Plugin 'altercation/vim-colors-solarized' " Solarised yessss
Plugin 'jnurmine/Zenburn'                 " For dark themes. Still not a fan of solarized-dark.
Plugin 'flazz/vim-colorschemes'           " Giant-ass collection because why not.

" Status/Air/Powerline
Plugin 'vim-airline/vim-airline-themes'   " Make the status bar match the theme.
Plugin 'vim-airline/vim-airline'          " Swag up my statusbar.

call vundle#end()                 " required
filetype plugin indent on         " required

set showmatch                  " Matching brackets.
set showcmd                    " Shadowing partial commands for completion!
set backspace=indent,eol,start " Allow Backspace to delete everythng.
set expandtab                  " We use spaces here.
set tabstop=2                  " And they're two spaces. Because Scala.
set softtabstop=2              " Because Scala.
set shiftwidth=2               " Scala aint changing soon sonny.
set autoindent                 " You can't escape
set incsearch                  " search while typing, not just after hitting CR
set hlsearch                   " Highlight search terms
set lazyredraw                 " Make vim redraw the screen less
set wildmenu                   " Visual tab complete menu.
set foldenable                 " Make shit orderly.
set cursorline                 " I do like to find my cursor
set number                     " And I like to see my numbers.
set relativenumber             " And I want vim motions to be usable.
set t_Co=256                   " Terminal stuff for Zenburn
colors seoul256                " Be pretty

" Remaps.
let mapleader=' '                        " we emacs now.
nnoremap <leader>evim :tabe ~/.vimrc<CR> " I type this entirely too often.
nnoremap H gT                            " Tab navigation left.
nnoremap L gt                            " Tab navigation right.
inoremap ZXZ <c-o>zz                     " re-center the screen without leaving insert

" Switching theme
nnoremap <leader>cd :AirlineTheme seoul256<CR>:colors seoul256<CR>:set background=dark<CR>
nnoremap <leader>cl :AirlineTheme solarized<CR>:colors solarized<CR>:set background=light<CR>

" Statusbar
let g:airline_theme='solarized'   " Make our powerline suit the theme at hand.
let g:airline_powerline_fonts = 1 " And make it pretty.
set laststatus=2                  " And make it... appear.

" Snippers
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsExpandTrigger='<c-e>'

" Vim dispatch
nnoremap <leader>ro <ESC>:w<CR>:Dispatch<CR>  " Run Open
nnoremap <leader>rh <ESC>:w<CR>:Dispatch!<CR> " Run Hidden

" FZF stuff
nnoremap <leader>ti :FZF<CR>
nnoremap <leader>tt :tabe<CR>:FZF<CR>
nnoremap <leader>th :FZF ~<CR>

" Syntastic Settings
let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list            = 1
let g:syntastic_check_on_open            = 1
let g:syntastic_check_on_wq              = 0

" Git things
nnoremap <leader>gs :Gstatus<CR>
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gg :!tig<CR>

" Easy Align things
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Rust
nnoremap <leader>rf :RustFmt<CR>
nnoremap <leader>rt :Dispatch cargo test<CR>
au FileType rust let b:dispatch = 'cargo run'
let g:ycm_rust_src_path = '/Users/az/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/'

" Python
nnoremap <leader>pt :Dispatch pytest %<CR>
let g:syntastic_python_checkers = ['pyflakes', 'pylint']
au FileType python let b:dispatch = 'python3 %'

" Ruby Things.
nnoremap <leader>rt :Dispatch rspec<CR> 
let g:syntastic_ruby_checkers = ['rubocop'] 
au FileType ruby let b:dispatch = 'ruby %'

" Markdown things.
au FileType markdown let b:dispatch = 'pandoc %:p -f markdown -t latex -o pandoc_output.pdf -S --latex-engine=xelatex'
au FileType markdown set tw=79
nnoremap <leader>mo :!open -a Skim pandoc_output.pdf<CR><CR>
nnoremap <leader>mt :Toc<CR>
let g:vim_markdown_folding_disabled     = 1 " Fuck folding in markdown documents.
let g:vim_markdown_toc_autofit          = 1 " Shrink TOC to avoid wasted whitespace.
let g:vim_markdown_math                 = 1 " Turn on Latex math, $...$ and $$...$$
let g:vim_markdown_new_list_item_indent = 2 " Make o insert indentation as 'new list item'
