set guifont=DjvSAKA-mono:h10
set guifontwide=DjvSAKA-mono:h10
set antialias
colorscheme qrg-dk

" -----------------------------------------------
" Path
" -----------------------------------------------
let g:gvimwinpos = '~/.vim/gvimwinpos'

" -----------------------------------------------
" Save/Restore Window Size/Position
" -----------------------------------------------
let g:save_window_file = expand(gvimwinpos)
augroup SaveWindow
  autocmd!
  autocmd VimLeavePre * call s:save_window()
  function! s:save_window()
    let options = [
      \ 'set columns=' . &columns,
      \ 'set lines=' . &lines,
      \ 'winpos ' . getwinposx() . ' ' . getwinposy(),
      \ ]
    call writefile(options, g:save_window_file)
  endfunction
augroup END

if filereadable(g:save_window_file)
  execute 'source' g:save_window_file
endif


" guioptions (go)
" default: "gmrLtT" (MS-Windows), "agimrLtT" (GTK, Motif and Athena)
"
" a  ビジュアルモードで選択した文字がシステムのクリップボードに入る。他のアプリケーションとクリップボードを共有するオプション
" A  "a"に似ているが、command-line で選択したときか、ctrl + shift を押しながらマウスで選択したときに共有のクリップボードを使用する
" c  簡単な質問をポップアップダイアログではなく、コンソールを使うようにする
" e  tabをGUIで
" f  シェルから実行されたときにfork()しない。-fオプションで起動したのと同じ
" i  Vimのアイコンを使用する。
" m  メニューを表示する。
" M  "$VIMRUNTIME/menu.vim"を読み込まなくする
" g  使用できないメニューをグレー表示する
" t  メニューの切り離しを出来るようにする
" T  ツールバーを表示する
" r  ウィンドウの右側にスクロールバーを表示する
" R  縦に分割されたウィンドウの右側にスクロールバーを表示する
" l  ウィンドウの左にスクロールバーを表示する
" L  縦に分割されたウィンドウの左側にスクロールバーを表示する
" b  水平スクロールバーを表示する
" v  ダイアログのボタンを縦に配置する
" p  ポインタコールバックを使う
" F  メッセージフッターを表示する
set guioptions-=T
set guioptions-=m

