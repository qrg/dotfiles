scriptencoding utf-8

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" debug
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set verbose=5
set verbosefile=~/vimdebug

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" general
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

let $LANG='en'

set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix
set fileformats=unix,dos,mac
set nocompatible

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" path
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Vim documentation: os_msdos
" http://vim-jp.org/vimdoc-ja/os_msdos.html#msdos-linked-files

" vim runtimepath
set runtimepath^=~/.vim
set runtimepath+=~/.vim/after

" vim history file dir
set viminfo+=n~/.vim/local/info.vim

" vim swap files dir
"set directory = g:SwapDir

" vim backup files dir
"set backupdir = g:BackupDir

" undo history file dir
if has('persistent_undo')
    set undodir=~/.vim/local/undo
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" local files
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" history -------------------------------------------------------------
set undolevels=99999999

" swapfile ([no]bk) ---------------------------------------------------
"set swapfile
set noswapfile

" backup --------------------------------------------------------------
"set backup
set nobackup

" extension of backup file
"set backupext=.back

" session -------------------------------------------------------------

" TODO git commit 時 session を read/write するのを避ける良い方法なにか
" 現状だと gvim のときだけ session 読み書きする

if has("gui_running")
    " restore the previous session when vim started
    source ~/.vim/local/session.vim

    " save the current session when vim terminated
    " '!' option overwrite the session file
    autocmd VimLeave * mksession! ~/.vim/local/session.vim

    " session options
    set sessionoptions=blank,buffers,curdir,folds,help,resize,slash,
                  \tabpages,unix,winpos,winsize
endif

" undo history -------------------------------------------------------
if has('persistent_undo')
  augroup vimrc-undofile
    autocmd!
    autocmd BufReadPre ~/* setlocal undofile
  augroup END
endif

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" complement
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" command-line completion operates in an enhanced mode
set wildmenu
" http://vimwiki.net/?%27wildmode%27
set wildmode=longest,full

"検索/補完時に大文字/小文字を区別しない
set ignorecase
"検索語に大文字を含む場合、大文字/小文字を区別する
set smartcase

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" search
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" enable incremental search
set incsearch

" when search next/previows reaches end/beginning of file,
" it wraps around to the beginning/end
"set nowrapscan        " do not wrap around
set wrapscan          " wrap around
"set wrapscan!         " toggle wrap around on/off
"set ws! ws?           " toggle and show value


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" edit
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" バックスペースでインデントや改行を削除できるようにする
set backspace=indent,eol,start

" 新しい行を作ったときに高度な自動インデントを行う
set smartindent
set autoindent
set cindent

" カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" misc
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" http://vimwiki.net/?%27backupcopy%27
" 編集したファイルが hardlink/symlinkなど
" 特別な属性を持っているとき全て元のまま保つi

"set backupcopy=yes

" flash screen instead of sounding beep
" abbreviated as 'vb'
" 't_vb=
set visualbell t_vb=

"変更中のファイルでも、保存しないで他のファイルを表示
set hidden

set nowrap

set laststatus=2

" -----------------------------------------------------------------------------
" tab
" -----------------------------------------------------------------------------
" use space instead of tab
set expandtab

" 行頭の余白内で Tab を打ち込むと、'shiftwidth' の数だけインデントする。
set smarttab

" Number of spaces to use for each step of (auto)indent. Used for |'cindent'|, |>>|, |<<|, etc.
" indent幅の画面上の見た目の文字数
" 'cindent' や shift operator (>>, <<) で挿入/削除される自動的に挿入される indent幅に適用される
set shiftwidth=4

" ファイル中の<Tab>を画面上の見た目で何文字分に展開するかを指定する
set tabstop=4

" indent を 'shiftwidth' の値の倍数に丸める
" command '>', '<' に適用される。
" insert-mode の <C-t> と <C-d> では、indent は常に 'shiftwidth' の倍数に丸められる
set shiftround

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" appearance
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
source ~/.vim/vimrc/appearance.vimrc

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" keybind
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
source ~/.vim/vimrc/keymap.vimrc


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" function
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

