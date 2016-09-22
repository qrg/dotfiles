let s:rc_dir = expand('$HOME/.vim/rc/')

" general "{{{
set nocompatible
set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix
set fileformats=unix,dos,mac
"}}}

" Echo startup time on start
if has('vim_starting') && has('reltime')
  let g:startuptime = reltime()
  augroup MyAutoCmd
    autocmd! VimEnter * let g:startuptime = reltime(g:startuptime) | redraw
          \ | echomsg 'startuptime: ' . reltimestr(g:startuptime)
  augroup END
endif

" functions "{{{
function! g:RealodVimrc()
  echomsg 'reloading ' . $MYVIMRC
endfunction
"}}}

" path "{{{
" vim history file dir
set viminfo&vim viminfo+=n$HOME/.vim/local/viminfo
" swap files dir
set directory=$HOME/.vim/local/swap
" backup files dir
set backupdir=$HOME/.vim/local/backup
" undo history file dir
if has('persistent_undo')
  set undodir=$HOME/.vim/local/undo
endif
"}}}

" local files "{{{
set history=100 " command and search history
set undolevels=100000
set swapfile
set backup
if has('persistent_undo')
  set undofile
endif
"}}}

" complement "{{{
" command-line completion
set wildmenu
set wildmode=longest,full
" 検索/補完時に大文字/小文字を区別しない
set ignorecase
" 検索語に大文字を含む場合、大文字/小文字を区別する
set smartcase
"}}}

" search "{{{
" enable incremental search
set incsearch
"}}}

" edit "{{{
" use clipboard
set clipboard=unnamed,autoselect

if has('unnamedplus')
  set clipboard=unnamedplus,unnamed,autoselect
endif

" 閉じ括弧が入力されたとき、対応する括弧を表示する
set showmatch
" バックスペースでインデントや改行を削除できるようにする
set backspace=indent,eol,start
" restore last cursor position when open a file
autocmd BufReadPost * if line("'\"") > 1 && line("'\"") <= line("$") | exe "normal! g`\"zz" | endif
"}}}

" tab "{{{
" use space instead of tab
set expandtab
" 行頭の余白内で Tab を打ち込むと、'shiftwidth' の数だけインデントする。
set smarttab
" Number of spaces to use for each step of (auto)indent. Used for |'cindent'|, |>>|, |<<|, etc.
" indent幅の画面上の見た目の文字数
" 'cindent' や shift operator (>>, <<) で挿入/削除される自動的に挿入される indent幅に適用される
" ファイル中の<Tab>を画面上の見た目で何文字分に展開するかを指定する
set tabstop=2
" indent を 'shiftwidth' の値の倍数に丸める
" command '>', '<' に適用される。
" insert-mode の <C-t> と <C-d> では、indent は常に 'shiftwidth' の倍数に丸められる
set shiftwidth=2
set shiftround
"}}}

" view "{{{
syntax on
set t_Co=256
set number
set nowrap
set notitle
" display unprintable characters | e.g.) tab, end of each line, trailing space, etc
set list
"  extends  ウィンドウの幅を超えて省略された文字が右にある場合に表示
"  precedes ウィンドウの幅を超えて省略された文字が左にある場合に表示
"  nbsp     半角スペース
set listchars=eol:↲,tab:⇀\ ,extends:»,precedes:«,nbsp:▯,trail:▯
set hlsearch " highlight search words
set nocursorline
set nocursorcolumn
set colorcolumn=80,120

set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

"}}}

" statusline "{{{
set ruler
"}}}

" cmdline "{{{
set cmdheight=3
set wildmenu " enable cmdline tab completion
set wildmode=list:longest,full
set showcmd " 入力途中の command を cmdline (右側) に表示する
"}}}

" misc "{{{

" switch buffers without saving
set hidden
" flash screen instead of sounding beep
set visualbell t_vb=
" support mouse
set mouse=a
set ttymouse=xterm2
set mousehide " hide cursor when editing

"}}}

" plugin configs {{{
let s:plugin_config_files = [
  \ 'dein.vim',
  \ 'unite.vim',
  \ 'neoyank.vim',
  \ 'vim-indent-guides.vim',
  \ 'vimfiler.vim',
  \ 'editorconfig.vim',
  \ 'gundo.vim',
  \ 'liteline.vim'
  \ ]

for s:file in s:plugin_config_files
  execute 'source ' . s:rc_dir . 'plugins/' . s:file
endfor

" }}}

" colorscheme "{{{
source $HOME/.vim/rc/colorscheme.vim
"}}}

" keymap "{{{
source $HOME/.vim/rc/keymap.vim
"}}}

" vim:foldmethod=marker:
