" Vim config
" (c) Alexander Solovyov, 2003-2006
" piranha AT piranha.org.ua
"
" Thanks to all, who has helped me in creation, especially to:
" Max Krasilnikov

" Settings {{{
set paste
" To be secure & Vi nocompatible
set secure nocompatible
syntax enable
set nofen
filetype on
filetype plugin on

" For Python
let python_highlight_all = 1
au FileType python source ~/.vim/scripts/python.vim
" SmartIndent
autocmd BufRead *.py set smartindent cinwords=if,elif,else,for,while,try,except,finally,def,class
" Remove trailing whitespaces
autocmd BufWritePre *.py normal m`:%s/\s\+$//e ``

" History and viminfo settings {{{
set history=10000
if has("viminfo")
	if filewritable(expand("$HOME/.vim/viminfo")) == 1 || 
				\ filewritable(expand("$HOME/.vim/")) == 2
		set viminfo=!,%,'5000,\"10000,:10000,/10000,n~/.vim/viminfo
	else
		set viminfo=
	endif
endif
" Don't save backups of files.
set nobackup
" }}}

" Status line settings {{{
" Display a status-bar.
set laststatus=2
if has("statusline")
	set statusline=%5*%0*%<%f\ %3*%m%1*%r%0*\ %2*%y%4*%w%0*%=[%b\ 0x%B]\ \ %8l,%10([%c%V/%{strlen(getline(line('.')))}]%)\ %P
endif
" }}}


" The cursor is kept in the same column (if possible).  This applies to the
" commands: CTRL-D, CTRL-U, CTRL-B, CTRL-F, "G", "H", "M", "L", , and to the
" commands "d", "<<" and ">>" with a linewise operator, with "%" with a count
" and to buffer changing commands (CTRL-^, :bnext, :bNext, etc.).  Also for an
" Ex command that only has a line number, e.g., ":25" or ":+".
set nostartofline

" Automatically setting options in various files
set modeline

" Available TAGS files
set tags=./TAGS,./tags,tags

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

" Need more undolevels ??
" (default 100, 1000 for Unix, VMS, Win32 and OS/2)
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

" }}}

" Keybord mappings {{{
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

" Open new window with the file ~/.zshrc (my shell configuration file)
map <C-O><C-T> :split ~/.zshrc<CR>
imap <C-O><C-T> <Esc>:split ~/.zshrc<CR>

" Open new window with file ~/.vimrc (ViM configuration file)
map <C-O><C-K> :split ~/.vimrc<CR>
imap <C-O><C-K> <Esc>:split ~/.vimrc<CR>
" Open new window with dir ~/.vim (ViM configuration dir)
map <C-O><C-V> :split ~/.vim<CR>
imap <C-O><C-V> <Esc>:split ~/.vim<CR>

" Safe delete line (don't add line to registers)
":imap <C-D> <Esc>"_ddi
imap <C-D> <Esc>:call SafeLineDelete()<CR>i

" Search for the current Visual selection.
vmap S y/<C-R>=escape(@",'/\')<CR>

map <F7> !tr "\`qwertyuiop[]asdfghjkl;'zxcvbnm,.~@\#$\%^&*QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>" "£ÊÃÕËÅÎÇÛÝÚÈßÆÙ×ÁÐÒÏÌÄÖÜÑÞÓÍÉÔØÂÀ³\"\'*:,.;êãõëåHçûýúèÿæù÷áðòïìäöüñþóíéôøâà"<CR>
map <F8> !tr "£ÊÃÕËÅÎÇÛÝÚÈßÆÙ×ÁÐÒÏÌÄÖÜÑÞÓÍÉÔØÂÀ³\"\'*:,.;êãõëåHçûýúèÿæù÷áðòïìäöüñþóíéôøâà" "\`qwertyuiop[]asdfghjkl;'zxcvbnm,.~@\#$\%^&*QWERTYUIOP{}ASDFGHJKL:\"ZXCVBNM<>"<CR>

map <F5> :set hls!<bar>set hls?<CR>

"Tabs
map gn :tabnew<CR>
map gc :tabclose<CR>
map <A-1> :tabfirst<CR>
map <A-2> :tabfirst<CR>gt
map <A-3> :tabfirst<CR>:tabn 3<CR>
map <A-4> :tabfirst<CR>:tabn 4<CR>
map <A-5> :tabfirst<CR>:tabn 5<CR>
map <A-6> :tabfirst<CR>:tabn 6<CR>
map <A-7> :tabfirst<CR>:tabn 7<CR>
map <A-8> :tabfirst<CR>:tabn 8<CR>
map <A-9> :tabfirst<CR>:tabn 9<CR>
map <A-0> :tabfirst<CR>:tabn 10<CR>

" Encoding {{{

menu Encoding.koi8-r       :e! ++enc=koi8-r<CR>
menu Encoding.windows-1251 :e! ++enc=cp1251<CR>
menu Encoding.ibm-866      :e! ++enc=ibm866<CR>
menu Encoding.utf-8        :e! ++enc=utf-8 <CR>
set wildmenu
set wcm=<Tab>
map <C-C> :emenu Encoding.<TAB>

"}}}

map <Leader>re :help regexpref<cr>

map <F9> <Esc>:1<CR>/^$<CR>o--=off<CR>piranha<CR><CR><Up>

"}}}

" Autocomands for ~/.vimrc {{{
augroup VimConfig
	autocmd!
" Reread configuration of ViM if file ~/.vimrc is saved
	autocmd BufWritePost ~/.vimrc	so ~/.vimrc | exec "normal zv"
	autocmd BufWritePost vimrc   	so ~/.vimrc | exec "normal zv"
augroup END
" }}}

" Colors {{{

"colorscheme torte
colorscheme elflord

" statusline:  coloring the status line
hi StatusLine   term=NONE cterm=NONE ctermfg=white  ctermbg=blue
hi StatusLineNC term=NONE cterm=NONE ctermfg=black  ctermbg=white

set ft=conf
"syn on

" }}}

" Modeline {{{
" vim:set ts=4:
" vim600:fdm=marker fdl=0 fdc=3 vb:
" }}}
