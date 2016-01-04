"Fira\ Mono\ Regular\ for\ Powerline:h15
set guifont=FiraMonoTakaoEx\ for\ Powerline:h15
set guifontwide=FiraMonoTakaoEx\ for\ Powerline:h15
set antialias
" テスト日本語
" path
"let s:gvimwinpos="$HOME/.vim/local/gvim-window-position"

" Don't load MacVim Kaoriya's gvimrc
let g:macvim_skip_colorscheme=1
colorscheme solarized

set guioptions?
set guioptions+=a
set guioptions-=r
set guioptions-=R
set guioptions-=l
set guioptions-=L
set guioptions-=b
" }}}

" vim-jp » Hack #120: gVim でウィンドウの位置とサイズを記憶する
" http://vim-jp.org/vim-users-jp/2010/01/28/Hack-120.html
let g:save_window_file = expand('$HOME/.vim/local/.gvim-window-position')

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

