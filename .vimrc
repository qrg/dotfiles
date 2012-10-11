scriptencoding utf-8

"set encoding=utf-8
set fileencoding=utf-8
set fileformat=unix

set nocompatible

"------------------------------------------------------------------------------
" path
"------------------------------------------------------------------------------

" temporary dir for .swp files
set directory=~/.vim/tmp
" vim history
set viminfo+=n~/.vim/viminfo
set backupdir=~/.vim/backup


"------------------------------------------------------------------------------
" history 
"------------------------------------------------------------------------------
set undolevels=99999999


"------------------------------------------------------------------------------
" complement 
"------------------------------------------------------------------------------
"コマンドライン補完を便利に
set wildmenu

"検索/補完時に大文字小文字を区別しない
set ignorecase

"検索時に大文字を含んでいたら大/小を区別
set smartcase

"------------------------------------------------------------------------------
" misc 
"------------------------------------------------------------------------------

"変更中のファイルでも、保存しないで他のファイルを表示
set hidden

"インクリメンタルサーチを行う
set incsearch

"set term=xterm

"検索をファイルの先頭へループしない
set nowrapscan
"set wrapscan

"シフト移動幅
set shiftwidth=2

"閉じ括弧が入力されたとき、対応する括弧を表示する
set showmatch

"新しい行を作ったときに高度な自動インデントを行う
"set smartindent
"set autoindent
"set cindent

"行頭の余白内で Tab を打ち込むと、'shiftwidth' の数だけインデントする。
set smarttab

"ファイル内の <Tab> が対応する空白の数
set tabstop=2

"カーソルを行頭、行末で止まらないようにする
set whichwrap=b,s,h,l,<,>,[,]

set expandtab
set shiftround
set nowrap

set laststatus=2

"------------------------------------------------------------------------------
" appearance
"------------------------------------------------------------------------------
" シンタックスハイライトを利用する
syntax on

" 行番号を表示
set number

" ルーラーを表示
set ruler

" 現在行にアンダーラインを表示
"set cursorline

" 現在列をハイライト表示
"set cursorcolumn
set cmdheight=2

" タイトルにファイル名を表示
set title
"set notitle

highlight LineNr ctermfg=darkgrey
highlight cursorcolumn term=reverse ctermbg=7 guibg=Grey90
highlight SpecialKey term=underline ctermfg=grey guifg=Grey90

" 入力中のコマンドをステータスに表示
set showcmd


"タブ文字、行末など不可視文字を表示する
set list
"listで表示される文字のフォーマットを指定する
"  eol      改行コード
"  trail    行末に続く半角スペース
"  extends  ウィンドウの幅を超えて省略された文字が右にある場合に表示
"  precedes ウィンドウの幅を超えて省略された文字が左にある場合に表示
"  nbsp     半角スペース
set listchars=eol:↲,tab:⇀\ ,extends:»,precedes:«,nbsp:▯,trail:▯

"全角スペースの表示
highlight JpSpace cterm=underline ctermfg=Blue guifg=Blue
au BufRead,BufNew * match JpSpace /　/

"行番号を表示する
set number

"検索語を強調表示
set hlsearch

" <C-l>で検索後の強調表示を解除する
nnoremap <C-l> :nohl<CR><C-l>

" ステータスライン
highlight statusline cterm=reverse

set statusline=\ %F%m%r%h%w\ \|\ %{&fenc!=''?&fenc:&enc}\ \|\ FORMAT=%{&ff}\ \|\ TYPE=%Y\ \|\ ASCII=\%03.3b\ \|\ HEX=\%02.2B\ \|\ POS=%04l,%04v\ \|\ %p%%\ \|\ LEN=%L

"挿入モード時、ステータスラインの色を変更
if has('syntax')
augroup InsertHook
  autocmd!
  autocmd InsertEnter * highlight StatusLine guifg=darkblue guibg=darkyellow gui=none ctermfg=black ctermbg=yellow cterm=none
  autocmd InsertLeave * highlight StatusLine guifg=darkblue guibg=darkgrey gui=none ctermfg=black ctermbg=grey cterm=none
  augroup END
endif

