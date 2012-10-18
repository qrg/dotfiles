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

" complement brackets
inoremap < <><LEFT>
inoremap { {}<LEFT>
inoremap [ []<LEFT>
inoremap ( ()<LEFT>
inoremap " ""<LEFT>
inoremap ' ''<LEFT>
inoremap ` ``<LEFT>
vnoremap < "zdi<<C-R>z><Esc>
vnoremap { "zdi{<C-R>z}<Esc>
vnoremap [ "zdi[<C-R>z]<Esc>
vnoremap ( "zdi(<C-R>z)<Esc>
vnoremap " "zdi"<C-R>z^V"<Esc>
vnoremap ' "zdi'<C-R>z'<Esc>
vnoremap ` "zdi`<C-R>z`<Esc>

" 削除でレジスタに格納しない
" TODO 効いてない気がする
nnoremap x "_x
noremap dd "_dd


" edit vim config

noremap <C-F1> :e ~/dotfiles/.vimrc<CR>
noremap <C-F2> :e ~/dotfiles/.gvimrc<CR>
noremap <C-S-F1> :source ~/dotfiles/.vimrc<CR>
noremap <C-S-F2> :source ~/dotfiles/.gvimrc<CR>

