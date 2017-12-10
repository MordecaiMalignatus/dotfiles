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
Plugin 'elixir-editors/vim-elixir'
Plugin 'slashmili/alchemist.vim'
Plugin 'dag/vim-fish'                     " vim support for fish code.
Plugin 'derekwyatt/vim-scala'
       
" Git Things.
Plugin 'tpope/vim-fugitive'               " Git integration by tpope. May get tossed.
Plugin 'airblade/vim-gitgutter'           " Shows changed/added/removed lines in gutter.

" Code Navigation
Plugin 'junegunn/fzf'                     " Fuzzy File Finder, replacement for command-t
Plugin 'junegunn/fzf.vim'                 " Adds FZF vim bindings for Extra Shit
Plugin 'junegunn/vim-easy-align'          " Make shit look pretty.
Plugin 'w0rp/ale'
Plugin 'tpope/vim-unimpaired'             " A lot of very useful paired motions.
Plugin 'tpope/vim-surround'               " Makes changing delimiters far less of a pain.
Plugin 'tpope/vim-dispatch'               " non-focus stealing builds/tests hooray!
Plugin 'tpope/vim-commentary'             " Makes commenting not a pain.
Plugin 'tpope/vim-endwise'                " automatically adds 'end' and similar to certain languages.
Plugin 'tpope/vim-ragtag'                 " Helps HTML be less awful.
Plugin 'tpope/vim-repeat'                 " Adds repeat motion for plugins, at least some.
Plugin 'godlygeek/tabular'                " Required for markdown.
Plugin 'jiangmiao/auto-pairs'             " Automatically match pairs.
Plugin 'justinmk/vim-sneak'               " f, aber in gut und wiederholbar.

" Snippets.
Plugin 'honza/vim-snippets'               " Snippet collection that comes in handy.
Plugin 'SirVer/ultisnips'                 " Snippet engine, integrates with YCM

" Themes and colorschemes.
Plugin 'altercation/vim-colors-solarized' " Solarised yessss
Plugin 'flazz/vim-colorschemes'           " Giant-ass collection because why not.

" Status/Air/Powerline
Plugin 'vim-airline/vim-airline-themes'   " Make the status bar match the theme.
Plugin 'vim-airline/vim-airline'          " Swag up my statusbar.

call vundle#end()                 " required
filetype plugin indent on         " required

set autoread                   " automatically read file-changes from disk.
set showmatch                  " Matching brackets.
set showcmd                    " Shadowing partial commands for completion!
set backspace=indent,eol,start " Allow Backspace to delete everythng.
set expandtab                  " We use spaces here.
set tabstop=4                  " And they're two spaces. Because Scala.
set softtabstop=4              " Because Scala.
set shiftwidth=4               " Scala aint changing soon sonny.
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
colors solarized               " Be pretty

" Remaps.
let mapleader=' '               
nnoremap <leader>evm :e ~/dotfiles/.vimrc<CR>
inoremap ZXZ <c-o>zz
inoremap ± <c-o>~

" Switching theme
nnoremap <leader>cd :set background=dark<CR>
nnoremap <leader>cl :set background=light<CR>

" Statusbar
let g:airline_theme='solarized'   " Make our powerline suit the theme at hand.
let g:airline_powerline_fonts = 1 " And make it pretty.
set laststatus=2                  " And make it... appear.

" Snippers
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"
let g:UltiSnipsExpandTrigger='<c-e>'

" Vim dispatch
nnoremap <leader>ro <ESC>:w<CR>:Dispatch<CR><CR>
nnoremap <leader>rh <ESC>:w<CR>:Dispatch!<CR><CR>

" FZF stuff
let $FZF_DEFAULT_COMMAND = 'ag -g ""'
let g:fzf_command_prefix = 'Fzf'
let g:fzf_layout         = { 'down': '~20%' }
let g:fzf_tags_command   = 'ctags -R -f .tags'
let g:fzf_history_dir    = '~/.fzf/history'

nnoremap <leader>tf  :FzfFiles<CR>
nnoremap <leader>tgf :FzfGitFiles<CR>
nnoremap <leader>tt  :FzfTags<CR>
nnoremap <leader>;   :w<CR>:FzfBuffers<CR>
nnoremap <leader>th  :FzfHistory<CR>
" Search Word
nnoremap <leader>w   :FzfAg<CR>
" Search word under cursor
nnoremap <leader>tw  :FzfAg <C-R><C-W><CR>
nnoremap <leader>gs  :FzfGFiles?<CR>
nnoremap <leader>hh  :FzfHelptags<CR>

" ALE settings.
let g:ale_sign_column_always = 1
let g:airline#extensions#ale#enabled = 1
let g:ale_open_list = 1
nmap <silent> <C-n> <Plug>(ale_next_wrap)
nmap <silent> <C-m> <Plug>(ale_previous_wrap)

 " I'd use stakc-build but that only works on-save
let g:ale_linters = {
      \ 'haskell':['hdevtools'],
      \}

" Git things
nnoremap <leader>gb :Gblame<CR>
nnoremap <leader>gd :Gdiff<CR>
nnoremap <leader>gg :execute "!cd " . expand('%:p:h') . "; tig status"<CR><CR>

" Easy Align things
xmap ga <Plug>(EasyAlign)
nmap ga <Plug>(EasyAlign)

" Rust
au FileType rust nnoremap <leader>rf :RustFmt<CR>
au FileType rust nnoremap <leader>rt :Dispatch cargo test<CR>
au FileType rust let b:dispatch = 'cargo run'
let g:ycm_rust_src_path = '/Users/az/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/'

" Python
nnoremap <leader>pt :Dispatch pytest %<CR>
au FileType python let b:dispatch = 'python3 %'

" Ruby Things.
nnoremap <leader>rt :Dispatch rspec<CR> 
let g:syntastic_ruby_checkers = ['rubocop'] 
au FileType ruby let b:dispatch = 'ruby %'
au FileType ruby nnoremap <leader>rt :Dispatch rspec<CR>

""""""
" Markdown things.

au FileType markdown let b:dispatch = 'pandoc %:p -f markdown+smart -t latex -o '. expand('%:t:r') . '.pdf --pdf-engine=xelatex'
au FileType markdown set tw=79
nnoremap <leader>mo :!open -a Skim pandoc_output.pdf<CR><CR>
nnoremap <leader>mt :Toc<CR>
let g:vim_markdown_folding_disabled     = 1 " Fuck folding in markdown documents.
let g:vim_markdown_toc_autofit          = 1 " Shrink TOC to avoid wasted whitespace.
let g:vim_markdown_math                 = 1 " Turn on Latex math, $...$ and $$...$$
let g:vim_markdown_new_list_item_indent = 2 " Make o insert indentation as 'new list item'

" Toggle spellchecker. 
au FileType markdown nnoremap <leader>sct :setlocal spell! spelllang=en_gb<CR>

" HTML bindings
au FileType html let b:dispatch = "open %"

" Scala
au FileType scala let b:dispatch = 'sbt compile'
au FileType scala nnoremap <leader>rt :Dispatch sbt test<CR>
au FileType scala nnoremap <leader>rf :SortScalaImports<CR>
