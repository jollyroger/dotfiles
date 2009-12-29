" ------------------------------------------------------------------------------
" Vim configuration file
" Created 08.08.2009
" ------------------------------------------------------------------------------

" ------------------------------------------------------------------------------
" Basic look'n'feel settings
" ------------------------------------------------------------------------------

" Do not use Vi compatible mode
set nocompatible

" Show cursor every time
set ruler

" Show not executed commands in status bar
set showcmd

" Show line numbers
set number

" Show editing line (mostly not needed if cursor is highlighted)
"set cul

" Disabling bell
set novisualbell
set t_vb=

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
set textwidth=80

" ------------------------------------------------------------------------------
" Search properties
" ------------------------------------------------------------------------------

" Enable incremental search
set incsearch

" Disabling search highlighting, it's obvious
set nohlsearch

" Mouse support
set mouse=a
set mousemodel=popup
set mousehide

" ------------------------------------------------------------------------------
" Work with buffers, tabs, etc
" ------------------------------------------------------------------------------

"Work with tabs instead of buffers
nmap :bn :tabNext
nmap :bp :tabprevious
nmap :e :tabnew
"nmap :badd :tabnew
"nmap :bc :tabc

" Default terminal encoding
set termencoding=utf-8

" Do not destroy buffer when switching to next file
set hidden

" ------------------------------------------------------------------------------
" Syntax highlighting options, filetype auto-detection
" ------------------------------------------------------------------------------

" Enable syntax highlighting
syntax on

"Enable filtype autodetection
filetype on
filetype indent on
filetype plugin on

" ------------------------------------------------------------------------------
" Status bar settings
" ------------------------------------------------------------------------------
" Show status bar always
set laststatus=2

" Sample status bars, need to think of it..
" Also I don't know how to split loong lines in vim config file :(
"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%c\",getftime(expand(\"%:p\")))}%=\ lin:%l\,%L\ col:%c%V\ pos:%o\ ascii:%b\ %P
"set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ total=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P
"set statusline=%F%m%r%h%w\ (%{&ff})\\t{%Y}[%l,%v][%p%%]\ %{strftime(\"%d/%m/%y\ -\ %H:%M\")}
"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d/%m/%Y-%H:%M\")}%=\ col:%c%V\ ascii:%b\ pos:%o\ lin:%l\,%L\ %P
"set statusline=%<%F%h%m%r%h%w%y\ %{&ff}\ %{strftime(\"%d.%m.%Y-%H:%M\")}%=\ pos:\ %c%V,\ %o\ lin:%l\,%L\ %P


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

nmap <F5> <Esc>:BufExplorer<cr>
vmap <F5> <esc>:BufExplorer<cr>
imap <F5> <esc><esc>:BufExplorer<cr>

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

colorscheme desert256
au BufRead,BufNewFile */nginx/nginx.conf set ft=nginx
au BufRead,BufNewFile */nginx/sites-*/* set ft=nginx
au BufRead,BufNewFile */nginx/conf/* set ft=nginx
au BufRead,BufNewFile */nginx/conf.d/* set ft=nginx
set runtimepath+=/usr/share/lilypond/2.12.2/vim
