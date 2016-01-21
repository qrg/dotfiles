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
set clipboard+=unnamed
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

" neobundle "{{{
" Note: Skip initialization for vim-tiny or vim-small.
if !1 | finish | endif

if has('vim_starting')
  if &compatible
    set nocompatible               " Be iMproved
  endif

  " Required:
  set runtimepath&vim runtimepath+=$HOME/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('$HOME/.vim/bundle/'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Plugin List:
" vimproc "{{{
NeoBundle 'Shougo/vimproc.vim', {
      \ 'build' : {
      \     'windows': 'tools\\update-dll-mingw',
      \     'cygwin': 'make -f make_cygwin.mak',
      \     'mac': 'make -f make_mac.mak',
      \     'linux': 'make',
      \     'unix': 'gmake',
      \    },
      \ }
"}}}


" unite "{{{
NeoBundle 'Shougo/unite.vim', { 'depends' : [ 'Shougo/vimproc.vim' ] }
NeoBundle 'Shougo/neomru.vim', { 'depends' : [ 'Shougo/unite.vim' ] }
NeoBundle 'Shougo/unite-outline', { 'depends' : [ 'Shougo/unite.vim' ] }
NeoBundle 'Shougo/unite-help', { 'depends' : [ 'Shougo/unite.vim' ] }
NeoBundle 'ujihisa/unite-colorscheme', { 'depends' : [ 'Shougo/unite.vim' ] }
NeoBundle 'thinca/vim-unite-history', { 'depends' : [ 'Shougo/unite.vim' ] }
"}}}

" filer "{{{
NeoBundle 'Shougo/vimfiler'
"}}}

" vimshell "{{{
NeoBundle 'Shougo/vimshell'
"}}}

" neocomplcache "{{{
if has('lua')
  NeoBundleLazy 'Shougo/neocomplete.vim', {'autoload': {'insert': 1}}
else
  NeoBundleLazy 'Shougo/neocomplcache', {'autoload': {'insert': 1}}
end
NeoBundle 'Shougo/neosnippet'
NeoBundle 'Shougo/neosnippet-snippets'
"}}}

" completions "{{{
NeoBundle 'kana/vim-smartinput' " 対応する括弧やクオートを補完
NeoBundle 'kana/vim-smartchr'   " 入力からの補完
NeoBundle 'tpope/vim-surround'  " 選択範囲を括弧やクオートで囲む
NeoBundle 'tyru/caw.vim'        " コメントアウト
"}}}

" cursor "{{{
NeoBundle 'terryma/vim-multiple-cursors'
"}}}

" undo history "{{{
NeoBundle 'sjl/gundo.vim'
"}}}

" textobj "{{{
NeoBundle 'kana/vim-textobj-user'
NeoBundle 'kana/vim-textobj-fold'
NeoBundle 'kana/vim-textobj-indent'
NeoBundle 'kana/vim-textobj-lastpat'
NeoBundle 'osyo-manga/vim-textobj-multiblock'
NeoBundle 'thinca/vim-textobj-between'
NeoBundle 'rhysd/vim-textobj-anyblock'
"}}}

" operator "{{{
NeoBundle 'kana/vim-operator-user'
NeoBundle 'rhysd/vim-operator-surround'
"}}}

" formatting "{{{
NeoBundle 'Align' " 特定文字ベースの文書整形
NeoBundle 'PreserveNoEOL' " EOL設定
NeoBundle 'editorconfig/editorconfig-vim' " エディタ設定共有
"}}}

" highlight "{{{
NeoBundle 'nathanaelkane/vim-indent-guides' " インデント
NeoBundle 'vim-scripts/AnsiEsc.vim' " ログファイル
NeoBundle 'osyo-manga/vim-anzu' " 検索位置表示
NeoBundle 'ap/vim-css-color' " preview colors in source code while editing
"}}}

" colorscheme "{{{
"NeoBundle 'morhetz/gruvbox'
NeoBundle 'altercation/vim-colors-solarized'
"NeoBundle 'w0ng/vim-hybrid'
"NeoBundle 'chriskempson/vim-tomorrow-theme'
"NeoBundle 'tomasr/molokai'
"NeoBundle 'flazz/vim-colorschemes'
"}}}

" status line "{{{
NeoBundle 'itchyny/lightline.vim'
"}}}

" scroll "{{{
NeoBundle 'terryma/vim-smooth-scroll'
"}}}

" git "{{{
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'airblade/vim-gitgutter'
"}}}

" syntax highlight "{{{
NeoBundle 'othree/html5.vim' " HTML5
NeoBundle 'digitaltoad/vim-jade' " Jade

NeoBundle 'pangloss/vim-javascript'
NeoBundle 'kchmck/vim-coffee-script' " CoffeeScript

NeoBundle 'hail2u/vim-css3-syntax'
NeoBundle 'cakebaker/scss-syntax.vim' " Sass
NeoBundle 'wavded/vim-stylus' " Stylus
NeoBundle 'KohPoll/vim-less'
"}}}

" help "{{{
NeoBundle 'vim-jp/vimdoc-ja'
"}}}

" external tools "{{{
NeoBundle 'JarrodCTaylor/vim-js2coffee' " coffee2js
NeoBundle 'rizzatti/dash.vim' " Dash
NeoBundle 'neilagabriel/vim-geeknote' " Evernote
"}}}

" Refer to |:NeoBundle-examples|.
" Note: You don't set neobundle setting in .gvimrc!

call neobundle#end()

" Required:
filetype plugin indent on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"}}}

" unite "{{{
call unite#custom#profile('default', 'context', {
      \  'start-insert' : 1,
      \  'smartcase' : 1,
      \  'prompt_direction': 'top',
      \  'split' : 1,
      \  'vertical' : 1,
      \  'direction' : 'topleft',
      \  'no-hide-icon': 1
      \})

let g:unite_source_history_yank_enable=1
let g:unite_enable_start_insert=1

if executable('ag')
  let g:unite_source_grep_command='ag'
  let g:unite_source_grep_default_opts='--nogroup --nocolor --column'
  let g:unite_source_grep_recursive_opt=''
endif
"}}}

" vim-indent-guides "{{{
let g:indent_guides_auto_colors=1
"autocmd VimEnter,Colorscheme * :hi IndentGuidesOdd   ctermbg=black
"autocmd VimEnter,Colorscheme * :hi IndentGuidesEven  ctermbg=darkgray
let g:indent_guides_enable_on_vim_startup=1
let g:indent_guides_guide_size=2
"}}}

" vimfiler "{{{
let g:vimfiler_as_default_explorer=1
let g:vimfiler_data_directory=expand("$HOME/.vim/local/vimfiler")
"}}}

" editorconfig "{{{
let g:EditorConfig_exec_path = '/usr/local/bin/editorconfig'
"}}}

" Gundo "{{{
let g:gundo_close_on_revert=1
"}}}

" liteline.vim "{{{
let g:lightline = {
      \ 'colorscheme': 'solarized',
      \ 'mode_map': { 'c': 'NORMAL' },
      \ 'active': {
      \ 'left': [ [ 'mode', 'paste' ], [ 'fugitive', 'filename' ] ]
      \ },
      \ 'component_function': {
      \ 'modified': 'MyModified',
      \ 'readonly': 'MyReadonly',
      \ 'fugitive': 'MyFugitive',
      \ 'filename': 'MyFilename',
      \ 'fileformat': 'MyFileformat',
      \ 'filetype': 'MyFiletype',
      \ 'fileencoding': 'MyFileencoding',
      \ 'mode': 'MyMode',
      \ },
      \ 'separator': { 'left': "\ue0b0", 'right': "\ue0b2" },
      \ 'subseparator': { 'left': "\ue0b1", 'right': "\ue0b3" }
      \ }
function! MyModified()
  return &ft =~ 'help\|vimfiler\|gundo' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction
function! MyReadonly()
  return &ft !~? 'help\|vimfiler\|gundo' && &readonly ? "\ue0a2" : ''
endfunction
function! MyFilename()
  return ('' != MyReadonly() ? MyReadonly() . ' ' : '') .
        \ (&ft == 'vimfiler' ? vimfiler#get_status_string() :
        \ &ft == 'unite' ? unite#get_status_string() :
        \ &ft == 'vimshell' ? vimshell#get_status_string() :
        \ '' != expand('%:t') ? expand('%:t') : '[No Name]') .
        \ ('' != MyModified() ? ' ' . MyModified() : '')
endfunction
function! MyFugitive()
  if &ft !~? 'vimfiler\|gundo' && exists("*fugitive#head")
    let _ = fugitive#head()
    return strlen(_) ? "\ue0a0 " ._ : ''
  endif
  return ''
endfunction
function! MyFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction
function! MyFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction
function! MyFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction
function! MyMode()
  return winwidth(0) > 60 ? lightline#mode() : ''
endfunction
"}}}

" vim-anzu "{{{
" statusline
set statusline=%{anzu#search_status()}
"}}}

" colorscheme "{{{
source $HOME/.vim/rc/colorscheme.vim
"}}}

" keymap "{{{
source $HOME/.vim/rc/keymap.vim
"}}}

" vim:foldmethod=marker:
