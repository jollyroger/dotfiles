set nocompatible
set ruler
set showcmd
set nu
"set cul

set foldmethod=indent
set autoindent
set smartindent

set incsearch
set nohlsearch

set novisualbell
set t_vb=

set mouse=a
set mousemodel=popup
set mousehide

set termencoding=utf-8

set hidden


syntax on

filetype on
filetype indent on
filetype plugin on

set laststatus=2
"set statusline=%<%f\%h%m%r%=%-20.(line=%l\ \ col=%c%V\ \ total=%L%)\ \ \%h%m%r%=%-40(bytval=0x%B,%n%Y%)\%P

set sessionoptions=curdir,buffers,tabpages

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
