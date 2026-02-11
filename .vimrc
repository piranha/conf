" Vim config
" (c) 2011 Alexander Solovyov

syntax enable
filetype plugin on
filetype indent on

" autoreload vimrc on saved change
autocmd! bufwritepost .vimrc source %

" Display a status bar.
set laststatus=2
if has("statusline")
    set statusline=%5*%0*%<%f\ %3*%m%1*%r%0*\ %2*%y%4*%w%0*%=[%b\ 0x%B]\ \ %8l,%10([%c%V/%{strlen(getline(line('.')))}]%)\ %P
endif

" some settings
set autoread " reload file when changed outside
set ruler " show current position
set wildmenu " better minibuffer autocomplete
set backspace=indent,eol,start
set whichwrap+=<,>,h,l
set ignorecase " when searching
set smartcase " huh, like Emacs!
set incsearch
set showmatch " parentheses
set mat=1 "How many tenths of a second to blink
set nostartofline " try to keep position of cursor
set scrolloff=5 " do not touch end of display

set noerrorbells
set novisualbell
set nobackup
set nowb
set noswapfile
set history=10000
if has("viminfo")
    if filewritable(expand("$HOME/.vim/viminfo")) == 1 || filewritable(expand("$HOME/.vim/")) == 2
        set viminfo=!,%,'5000,\"10000,:10000,/10000,n~/.vim/viminfo
    else
        set viminfo=
    endif
endif

set expandtab
set shiftwidth=4
set tabstop=4
set smarttab

set paste

" python
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class

" Universal theme: works on both dark and light backgrounds without
" any detection (t_RB, defaults read, etc). The trick is ctermbg=NONE
" everywhere so the terminal background shows through, and using colors
" that are readable on both dark and light.
colorscheme default
hi Normal     ctermbg=NONE guibg=NONE
hi NonText    ctermbg=NONE guibg=NONE
hi LineNr     ctermbg=NONE guibg=NONE
hi StatusLine cterm=bold ctermbg=252 ctermfg=235 guibg=#d0d0d0 guifg=#262626
hi Comment    cterm=NONE ctermfg=darkgray gui=NONE guifg=red2
hi Special    cterm=NONE ctermfg=darkred  gui=NONE guifg=deeppink
hi MatchParen cterm=bold ctermbg=252 ctermfg=NONE guibg=#d0d0d0 guifg=NONE
hi Error      cterm=bold ctermbg=NONE ctermfg=131 guibg=NONE guifg=#af5f5f
hi ErrorMsg   cterm=bold ctermbg=NONE ctermfg=131 guibg=NONE guifg=#af5f5f

" bindings

let mapleader = ","
let g:mapleader = ","
noremap ; :

nmap <leader>w :w!<cr>
nmap <leader>x :x<cr>
nmap <leader>q :q<cr>
nmap <leader>Q :q!<cr>

map <leader>s :setlocal spell!<cr>
map <leader>p :setlocal paste!<cr>

" disable annoying help
map <F1> <Esc>
imap <F1> <Esc>

" Smart way to move between windows
map <C-j> <C-W>j<C-W>_
map <C-k> <C-W>k<C-W>_
map <C-h> <C-W>h<C-W>_
map <C-l> <C-W>l<C-W>_
map <leader>j <C-W>j
map <leader>k <C-W>k
map <leader>h <C-W>h
map <leader>l <C-W>l

" TODO: M-; to comment/uncomment

cmap w!! w !sudo tee % >/dev/null
cmap x!! x !sudo tee % >/dev/null
nmap ; :

" earlier I had something like
" https://github.com/tpope/vim-rsi/blob/master/plugin/rsi.vim
" but it's not working and no idea why
