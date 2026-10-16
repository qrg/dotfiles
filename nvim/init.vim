set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix
set fileformats=unix,dos,mac
set history=1000
set undolevels=100000
set swapfile
set backup
if has('persistent_undo')
  set undofile
endif


set wildmenu
set wildmode=longest,full
set ignorecase
set smartcase

set incsearch

set clipboard+=unnamedplus

set showmatch
set backspace=indent,eol,start

set expandtab
set smarttab
set tabstop=2
set shiftwidth=2
set shiftround

syntax enable
set number
set nowrap
set notitle
set list

set listchars=eol:↲,tab:⇀\ ,extends:»,precedes:«,nbsp:▯,trail:▯
set hlsearch
set nocursorline
set nocursorcolumn
set colorcolumn=80,120

set laststatus=2
set showtabline=2
set noshowmode

set ruler

set cmdheight=3
set wildmenu
set wildmode=list:longest,full
set showcmd

set hidden
set visualbell
set mouse=a
set mousehide

function! s:on_yank(msg) abort
  if filewritable(expand("~/.local/run/clipper/clipper.sock"))
    call system('nc -NU ~/.local/run/clipper/clipper.sock', a:msg)
  endif
endfunction

augroup clipboard_copy
  autocmd!
  "autocmd TextYankPost * call system("clipboard-copy.sh", @")
  autocmd TextYankPost * if v:event.visual | call s:on_yank(@0)
augroup END

call plug#begin()
Plug 'folke/tokyonight.nvim', { 'branch': 'main' }
call plug#end()

colorscheme tokyonight
