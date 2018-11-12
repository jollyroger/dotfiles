" ~/.vimrc - Vim configuration file

" ------------------------------------------------------------------------------
" Plugin installation
" ------------------------------------------------------------------------------

call plug#begin('~/.vim/plugged')

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

Plug 'vim-airline/vim-airline'


Plug 'altercation/vim-colors-solarized'

call plug#end()


" ------------------------------------------------------------------------------
" Plugin setup
" ------------------------------------------------------------------------------

let g:airline_powerline_fonts = 1

set background=dark
colorscheme solarized


" ------------------------------------------------------------------------------
" Basic look'n'feel settings
" ------------------------------------------------------------------------------
"
" Do not use Vi compatible mode
set nocompatible

" Show cursor every time
set ruler

" Show not executed commands in status bar
set showcmd

" Show line numbers
set number

" Set Leader
let mapleader=","

" Show editing line (mostly not needed if cursor is highlighted)
"set cul

" Disabling bell
set novisualbell

" Set 256 colour support
set t_Co=256


" ------------------------------------------------------------------------------
" Indentation properties
" ------------------------------------------------------------------------------

" Default folding detection - by indent
set foldmethod=indent

" Enable autoindenting (ugly when trying to paste highlighted code)
set autoindent

" Enable ``smart'' indentation
set smartindent
	
" Tabulation properties (use spaces instead of tabs)
set smarttab
set expandtab
set shiftwidth=4
set softtabstop=4
set tabstop=4

" Text width and word wrap
set textwidth=79


" ------------------------------------------------------------------------------
" Search properties
" ------------------------------------------------------------------------------

" Enable incremental search
set incsearch

" Disabling search highlighting, it's obvious
set nohlsearch

" Keep a longer history
set history=1000

" Mouse support
set mouse=a
set mousemodel=popup
set mousehide

" Maintain more context around cursor
set scrolloff=3


" ------------------------------------------------------------------------------
" Work with buffers, tabs, etc
" ------------------------------------------------------------------------------

"Work with tabs instead of buffers
nmap :bn :tabNext
nmap :bp :tabprevious
nmap :e :tabnew
nmap :badd :tabnew
nmap :bc :tabc

" Default terminal encoding
set termencoding=utf-8

" Do not destroy buffer when switching to next file
set hidden

" Set the backupdir where all the swap/backup files will be stored
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp

" ------------------------------------------------------------------------------
" Space characters
" ------------------------------------------------------------------------------
set listchars=tab:>-,trail:·,eol:$,nbsp:•
nmap <silent> <leader>s :set nolist!<CR>


" ------------------------------------------------------------------------------
" Enable Syntax Formatting
" ------------------------------------------------------------------------------

syntax enable
filetype on
filetype indent on
filetype plugin on

" ------------------------------------------------------------------------------
" Additional file format detection rules
" ------------------------------------------------------------------------------
au BufNewFile,BufRead */.Xresources.d/* setf xdefaults


" ------------------------------------------------------------------------------
" Additional usefull keyboard mappings
" ------------------------------------------------------------------------------
set sessionoptions=curdir,buffers,tabpages

" Scroll pages wit <Space> in normal mode
nmap <Space> <PageDown>

imap <C-F> <C-X><C-O>
vmap <C-C> "+yi
imap <C-V> <esc>"+gPi

map <S-Insert> <MiddleMouse>

nmap <F3> :copen<cr>
vmap <F3> <esc>:copen<cr>
imap <F3> <esc>:copen<cr>

nmap <F5> <Esc>:NERDTreeToggle<cr>
vmap <F5> <esc>:NERDTreeToggle<cr>
imap <F5> <esc><esc>:NERDTreeToggle<cr>

map <F6> :bp<cr>
vmap <F6> <esc>:bp<cr>i
imap <F6> <esc>:bp<cr>i

map <F7> :bn<cr>
vmap <F7> <esc>:bn<cr>i
imap <F7> <esc>:bn<cr>i

map <F8> :MarksBrowser<cr>
vmap <F8> <esc>:MarksBrowser<cr>
imap <F8> <esc>:MarksBrowser<cr>

map <F9> :make<cr>
vmap <F9> <esc>:make<cr>i
imap <F9> <esc>:make<cr>i

map <F11> :TlistToggle<cr>
vmap <F11> <esc>:TlistToggle<cr>
imap <F11> <esc>:TlistToggle<cr>

map <F12> :Ex<cr>
vmap <F12> <esc>:Ex<cr>i
imap <F12> <esc>:Ex<cr>i

vmap < <gv
vmap > >gv

command W w !sudo tee % > /dev/null
