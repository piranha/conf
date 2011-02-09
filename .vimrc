" Vim config
" (c) Alexander Solovyov, 2003-2006
" piranha AT piranha.org.ua
"
" Thanks to all, who has helped me in creation, especially to:
" Max Krasilnikov

" Settings
set paste
" To be secure & Vi nocompatible
set secure nocompatible
syntax enable
set nofen
filetype on
filetype plugin on

" For Python
let python_highlight_all = 1
" SmartIndent
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class

" History and viminfo settings
set history=10000
if has("viminfo")
	if filewritable(expand("$HOME/.vim/viminfo")) == 1 || filewritable(expand("$HOME/.vim/")) == 2
		set viminfo=!,%,'5000,\"10000,:10000,/10000,n~/.vim/viminfo
	else
		set viminfo=
	endif
endif
" Don't save backups of files.
set nobackup

" Display a status bar.
set laststatus=2
if has("statusline")
	set statusline=%5*%0*%<%f\ %3*%m%1*%r%0*\ %2*%y%4*%w%0*%=[%b\ 0x%B]\ \ %8l,%10([%c%V/%{strlen(getline(line('.')))}]%)\ %P
endif

" The cursor is kept in the same column (if possible).
set nostartofline

" Automatically setting options in various files
set modeline

" Available TAGS files
set tags=./TAGS,./tags,tags,~/TAGS

" Don't add EOF at end of file
set noendofline

" Do case insensitive matching
set ignorecase
set noautoindent
" set textwidth=80 
set showfulltag 
" set showmatch

set ch=2 bs=2 
set incsearch report=0 title
set showcmd showmatch showmode

" Indent of 1 tab with size of 4 spaces
set tabstop=4
set shiftwidth=4
set softtabstop=4
set smarttab
set expandtab
set undolevels=1000

" Settings for mouse (gvim under Xwindows)
set nomousefocus mousehide

" Cursor never touch end of screen
set scrolloff=5

" Make window maximalized
set winheight=20

" The screen will not be redrawn while executing macros, registers
" and other commands that have not been typed. To force an updates use |:redraw|.
set lazyredraw

" time out on mapping after one second, time out on key codes after
" a tenth of a second
set timeout timeoutlen=1000 ttimeoutlen=100

set noerrorbells
set novisualbell

" Set this, if you will open all windows for files specified
" on the commandline at vim startup.
"let g:open_all_win=1
let g:open_all_win=0

" Settings for folding long lines
let g:fold_long_lines=300

" set list
" set listchars=tab:>-,trail:-

" backspace:  '2' allows backspacing" over
" indentation, end-of-line, and start-of-line.
" see also "help bs".
"set backspace=indent,eol,start      
set   backspace=2


" Keybord mappings
"
" Deny annoying help!
map <F1> <Esc>
imap <F1> <Esc>

" Switching between windows by pressing one time CTRL-X keys.
noremap <C-X> <C-W><C-W>

" Tip from http://vim.sourceforge.net/tips/tip.php?tip_id=173
noremap <C-J> <C-W>j<C-W>_
noremap <C-K> <C-W>k<C-W>_

" Make Insert-mode completion more convenient:
imap <C-L> <C-X><C-L>

set remap
map <C-O><C-O> :split 
imap <C-O><C-O> <Esc>:split 

" Safe delete line (don't add line to registers)
":imap <C-D> <Esc>"_ddi
imap <C-D> <Esc>:call SafeLineDelete()<CR>i

" Search for the current Visual selection.
vmap S y/<C-R>=escape(@",'/\')<CR>

map <F5> :set hls!<bar>set hls?<CR>

" Colors

colorscheme delek
"colorscheme elflord

" statusline:  coloring the status line
hi StatusLine   term=NONE cterm=NONE ctermfg=white  ctermbg=blue
hi StatusLineNC term=NONE cterm=NONE ctermfg=black  ctermbg=white

cmap w!! w !sudo tee % >/dev/null
cmap x!! x !sudo tee % >/dev/null
nmap ; :

" Modeline
" vim:set ts=4:
" vim600:fdm=marker fdl=0 fdc=3 vb:
