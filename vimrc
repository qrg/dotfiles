"===========================================================================
" neobundle
"===========================================================================
" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Plugin List:
" vimproc {{{
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows': 'tools\\update-dll-mingw',
\     'cygwin': 'make -f make_cygwin.mak',
\     'mac': 'make -f make_mac.mak',
\     'linux': 'make',
\     'unix': 'gmake',
\    },
\ }
" }}}

" unite {{{
NeoBundleLazy 'Shougo/unite.vim'
NeoBundleLazy 'Shougo/neomru.vim'
NeoBundleLazy 'Shougo/unite-outline'
NeoBundleLazy 'Shougo/unite-help'
" }}}

" vimfiler {{{
NeoBundleLazy 'Shougo/vimfiler'
" }}}

" vimshell {{{
NeoBundleLazy 'Shougo/vimshell'
" }}}

" neocomplcache {{{
if has('lua')
  NeoBundleLazy 'Shougo/neocomplete.vim', {'autoload': {'insert': 1}}
else
  NeoBundleLazy 'Shougo/neocomplcache', {'autoload': {'insert': 1}}
end
NeoBundleLazy 'Shougo/neosnippet'
NeoBundleLazy 'Shougo/neosnippet-snippets'
" }}}

" textobj {{{
NeoBundleLazy 'kana/vim-textobj-user'
NeoBundleLazy 'kana/vim-textobj-fold'
NeoBundleLazy 'kana/vim-textobj-indent'
NeoBundleLazy 'kana/vim-textobj-lastpat'
NeoBundleLazy 'osyo-manga/vim-textobj-multiblock'
NeoBundleLazy 'thinca/vim-textobj-between'
NeoBundleLazy 'rhysd/vim-textobj-anyblock'
" }}}

" operator {{{
NeoBundleLazy 'kana/vim-operator-user'
NeoBundleLazy 'rhysd/vim-operator-surround'
" }}}

" formatting {{{
NeoBundleLazy 'Align' " 特定文字ベースの文書整形
NeoBundleLazy 'PreserveNoEOL' " EOL設定
NeoBundleLazy 'editorconfig/editorconfig-vim' " エディタ設定共有
" }}}

" highlight {{{
NeoBundleLazy 'nathanaelkane/vim-indent-guides' " インデント
NeoBundleLazy 'vim-scripts/AnsiEsc.vim' " ログファイル
" }}}

" theme {{{
"NeoBundle 'morhetz/gruvbox'
NeoBundle 'altercation/vim-colors-solarized'
" }}}

" syntax highlight {{{
NeoBundleLazy 'othree/html5.vim' " HTML5
NeoBundleLazy 'digitaltoad/vim-jade' " Jade
NeoBundleLazy 'cakebaker/scss-syntax.vim' " Sass
NeoBundleLazy 'wavded/vim-stylus' " Stylus
" NeoBundle 'KohPoll/vim-less'
NeoBundleLazy 'kchmck/vim-coffee-script' " CoffeeScript
NeoBundleLazy 'endel/actionscript.vim' " ActionScript
" }}}

" external tools {{{
NeoBundleLazy 'JarrodCTaylor/vim-js2coffee' " coffee2js
NeoBundleLazy 'rizzatti/dash.vim' " Dash
" }}}

" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck

"===========================================================================
" powerline
"===========================================================================
python from powerline.vim import setup as powerline_setup
python powerline_setup()
python del powerline_setup

set laststatus=2 " Always display the statusline in all windows
set showtabline=2 " Always display the tabline, even if there is only one tab
set noshowmode " Hide the default mode text (e.g. -- INSERT -- below the statusline)

" vim-indent-guides
let g:indent_guides_auto_colors=0
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd   ctermbg=black
"autocmd VimEnter,Colorscheme * :hi IndentGuidesEven  ctermbg=darkgray
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size=2


"===========================================================================
" debug
"===========================================================================
" set verbose=5
" set verbosefile=~/vimdebug

"===========================================================================
" general
"===========================================================================

let $LANG='en'

set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix
set fileformats=unix,dos,mac
set nocompatible

set helplang=ja 

"===========================================================================
" path
"===========================================================================
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

"===========================================================================
" local files
"===========================================================================

" history -------------------------------------------------------------
set undolevels=99999999

" swapfile ([no]bk) ---------------------------------------------------
"set swapfile
set noswapfile

" backup --------------------------------------------------------------
"set backup
set nobackup

" undo history -------------------------------------------------------
if has('persistent_undo')
  augroup vimrc-undofile
    autocmd!
    autocmd BufReadPre ~/* setlocal undofile
  augroup END
endif

"===========================================================================
" complement
"===========================================================================
" command-line completion operates in an enhanced mode
set wildmenu
" http://vimwiki.net/?%27wildmode%27
set wildmode=longest,full

" 検索/補完時に大文字/小文字を区別しない
set ignorecase
" 検索語に大文字を含む場合、大文字/小文字を区別する
set smartcase

"===========================================================================
" search
"===========================================================================
" enable incremental search
set incsearch

" when search next/previows reaches end/beginning of file,
" it wraps around to the beginning/end
"set nowrapscan        " do not wrap around
set wrapscan          " wrap around
"set wrapscan!         " toggle wrap around on/off
"set ws! ws?           " toggle and show value


"===========================================================================
" edit
"===========================================================================

" バックスペースでインデントや改行を削除できるようにする
set backspace=indent,eol,start

" 新しい行を作ったときに高度な自動インデントを行う
set smartindent
set autoindent
set cindent

" カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]


"===========================================================================
" tab
"===========================================================================
" use space instead of tab
set expandtab

" 行頭の余白内で Tab を打ち込むと、'shiftwidth' の数だけインデントする。
set smarttab

" Number of spaces to use for each step of (auto)indent. Used for |'cindent'|, |>>|, |<<|, etc.
" indent幅の画面上の見た目の文字数
" 'cindent' や shift operator (>>, <<) で挿入/削除される自動的に挿入される indent幅に適用される
set shiftwidth=2

" ファイル中の<Tab>を画面上の見た目で何文字分に展開するかを指定する
set tabstop=2

" indent を 'shiftwidth' の値の倍数に丸める
" command '>', '<' に適用される。
" insert-mode の <C-t> と <C-d> では、indent は常に 'shiftwidth' の倍数に丸められる
set shiftround


" Apearance:

" enable syntax highlight
syntax on
" show line numbers
set number
" show ruler in status-line
" set ruler

set background=dark
colorscheme solarized
" colorscheme gruvbox
highlight LineNr ctermfg=243
highlight CursorLineNr ctermfg=214

if has('gui_running')
    " highlight current line
    set cursorline
    " highlight current column
    set cursorcolumn
endif

" 閉じ括弧が入力されたとき、対応する括弧を表示する
set showmatch
set cmdheight=2

" show file name in title
set title
" set notitle

highlight LineNr ctermfg=darkgrey guifg=Gray40
highlight CursorColumn ctermbg=7 guibg=Grey90

" http://vim-jp.org/vimdoc-ja/options.html#%27listchars%27
highlight NonText ctermfg=1
highlight NonText guifg=Gray
highlight SpecialKey ctermfg=1
"highlight SpecialKey ctermbg=7
highlight SpecialKey guifg=Gray
"highlight SpecialKey guibg=#012345 " GUI版での背景色指定

" Show (partial) command in the last line of the screen.
" 入力中のコマンドをステータスに表示 (sc)
set showcmd

set textwidth=80

" display unprintable characters | e.g.) tab, end of each line, trailing space, etc
set list
" Listで表示される文字のフォーマットを指定する
"  extends  ウィンドウの幅を超えて省略された文字が右にある場合に表示
"  precedes ウィンドウの幅を超えて省略された文字が左にある場合に表示
"  nbsp     半角スペース
set listchars=eol:↲,tab:⇀\ ,extends:»,precedes:«,nbsp:▯,trail:▯

" display multi-byte space
highlight JpSpace cterm=underline ctermfg=Blue guifg=Blue
au BufRead,BufNew * match JpSpace /　/

" highlight search words
set hlsearch
" status-line
"highlight statusline cterm=reverse
" status-line format
"set statusline=\ %F%m%r%h%w\ \|\ %{&fenc!=''?&fenc:&enc}\ \|\ %{&ff}\ \|\ type:%Y\ \|\ %04lL\ %04vC\ \|\ ascii:\%03.3b\ \|\ hex:\%02.2B\ \|\ %p%%\ \|\ total:%L

"change status-line color when insert-mode
"if has('syntax')
"    augroup InsertHook
"        autocmd!
"        autocmd InsertEnter *
"           \ highlight StatusLine
"           \ guifg=gray30
"           \ guibg=yellow
"           \ gui=none
"           \ ctermfg=black
"           \ ctermbg=yellow
"           \ cterm=none
"        autocmd InsertLeave *
"           \ highlight StatusLine
"           \ guifg=gray60
"           \ guibg=gray20
"           \ gui=none
"           \ ctermfg=black
"           \ ctermbg=grey
"           \ cterm=none
"    augroup END
"endif

" =====================================================================
" keymap
" =====================================================================
map q <Nop>

" search same as general apps
nnoremap <C-f> /

" C-k delete from the current cursor position to the end of line, like Emacs
noremap <C-k> D
inoremap <expr> <C-k> "\<C-g>u".(col('.') == col('$') ? '<C-o>gJ' : '<C-o>D')
cnoremap <C-k> <C-\>e getcmdpos() == 1 ? '' : getcmdline()[:getcmdpos()-2]<CR>

" move next/previous buffer
noremap <C-S-PageUp> :bprevious<CR>
noremap <C-S-PageDown> :bnext<CR>
noremap! <C-S-PageUp> <Esc>:bprevious<CR>
noremap! <C-S-PageDown> <Esc>:bnext<CR>

" emacs style canceling
noremap <C-g> <C-c>
"                                                                                                             test
" Enter inserts line break in normal-mode
nnoremap <Enter> i<Enter><Esc>
" BackSpace deletes 1 letter before cursor in normal-mode
nnoremap <BS> i<BS><Esc>

nnoremap <C-Enter> <C-v>

" move cursor center line of window after page scrolling (half scroll)
noremap <PageUp> <PageUp>zz
noremap <PageDown> <PageDown>zz
noremap! <PageUp> <Esc><PageUp>zz
noremap! <PageDown> <Esc><PageDown>zz

noremap <C-a> <Home>
noremap <C-e> <End>
noremap! <C-a> <Home>
noremap! <C-e> <End>


"===========================================================================
" misc
"===========================================================================

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

" vim から感謝されないようにする
set notitle

