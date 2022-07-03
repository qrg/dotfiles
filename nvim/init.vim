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

if &term =~ "xterm"
  let &t_SI .= "\e[?2004h"
  let &t_EI .= "\e[?2004l"
  let &pastetoggle = "\e[201~"

  function XTermPasteBegin(ret)
    set paste
      return a:ret
  endfunction

  inoremap <special> <expr> <Esc>[200~ XTermPasteBegin("")
endif

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
set visualbell t_vb=
set mouse=a
set mousehide
