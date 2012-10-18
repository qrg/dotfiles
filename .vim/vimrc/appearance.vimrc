" =====================================================================
" appearance
" =====================================================================

" enable syntax highlight
syntax on
" show line numbers
set number
" show ruler in status-line
set ruler

" 閉じ括弧が入力されたとき、対応する括弧を表示する
set showmatch

if has('gui_running')
    " highlight current line
    set cursorline
    " highlight current column
    set cursorcolumn
endif

set cmdheight=2

" show file name in title
set title
"set notitle

highlight LineNr ctermfg=darkgrey
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
" listで表示される文字のフォーマットを指定する
"  eol      改行コード
"  trail    行末に続く半角スペース
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
highlight statusline cterm=reverse
" status-line format
set statusline=\ %F%m%r%h%w\ \|\ %{&fenc!=''?&fenc:&enc}\ \|\ %{&ff}\ \|\ type:%Y\ \|\ %04lL\ %04vC\ \|\ ascii:\%03.3b\ \|\ hex:\%02.2B\ \|\ %p%%\ \|\ total:%L

"change status-line color when insert-mode
if has('syntax')
    augroup InsertHook
        autocmd!
        autocmd InsertEnter *
           \ highlight StatusLine
           \ guifg=gray30
           \ guibg=yellow
           \ gui=none
           \ ctermfg=black
           \ ctermbg=yellow
           \ cterm=none
        autocmd InsertLeave *
           \ highlight StatusLine
           \ guifg=gray60
           \ guibg=gray20
           \ gui=none
           \ ctermfg=black
           \ ctermbg=grey
           \ cterm=none
    augroup END
endif

