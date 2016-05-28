" keymap
" =============================================================================
"
"   command      normal  insert  cmdline  visual
" |============|=======|=======|========|========|
" map/noremap      @       -        -       @
" nmap/nnoremap    @       -        -       -
" imap/inoremap    -       @        -       -
" cmap/cnoremap    -       -        @       -
" vmap/vnoremap    -       -        -       @
" map!/noremap!    -       @        @       -
" |============|=======|=======|========|========|

let mapleader=','
inoremap <C-i> <Esc>

" disable some keymaps "{{{
nnoremap q <Nop>
nnoremap Q <Nop>
nnoremap ZZ <Nop>
nnoremap ZQ <Nop>
nnoremap a <Nop>
nnoremap A <Nop>
nnoremap Q <Nop>
"nnoremap u <Nop>
"nnoremap <C-r> <Nop>
"}}}

" move "{{{
" [Emacs] <C-e> to end of line
nnoremap <C-a> ^
nnoremap <C-e> $
"}}}

" scroll "{{{
map <PageUp> <C-u>
map <PageDown> <C-d>
"}}}

" edit "{{{
"nnoremap <silent> <C-z> :undo<CR>
"nnoremap <silent> <S-z> :redo<CR>
" <C-k> to delete from cursor to line end
nnoremap <C-k> d$
"}}}

" search "{{{
nnoremap <silent> <C-l> :<C-u>nohlsearch<CR>
"}}}

" buffer "{{{
" [Emacs] <C-x>k to close buffer completely
" nnoremap <C-x>k :bw<CR>
nnoremap <C-x>k :bd<CR>
"}}}

" insert mode "{{{
" 挿入モードでのカーソル移動
inoremap <C-j> <Down>
inoremap <C-k> <Up>
inoremap <C-h> <Left>
inoremap <C-l> <Right>
" カーソルから行末まで削除
inoremap <silent> <C-d>0 <Esc>lc$
" カーソルから行頭までヤンク
inoremap <silent> <C-y>e <Esc>ly0<Insert>
" カーソルから行末までヤンク
inoremap <silent> <C-y>0 <Esc>ly$<Insert>
"}}}

" window "{{{
nnoremap <C-w><C-h> <C-w>h
nnoremap <C-w><C-j> <C-w>j
nnoremap <C-w><C-k> <C-w>k
nnoremap <C-w><C-l> <C-w>l
"}}}

" help "{{{
autocmd FileType help nnoremap <silent> <buffer> <Esc><Esc> <C-w>c
nnoremap ,h :<C-u>vert bel help<Space>
"}}}

" edit/reload vimrc/gvimrc "{{{
nnoremap <silent> <Leader>r :<C-u>source $MYVIMRC<CR> :echomsg 'reloaded ' . $MYVIMRC<CR>
nnoremap <silent> <Leader>gr :<C-u>source $MYGVIMRC<CR> :echomsg 'reloaded ' . $MYGVIMRC<CR>
nnoremap <silent> <Leader>v :<C-u>edit $MYVIMRC<CR>
nnoremap <silent> <Leader>gv :<C-u>edit $MYGVIMRC<CR>
"}}}

" unite "{{{
" ---------------------------------------------------------------------
nnoremap [unite] <Nop>
nmap <Space> [unite]

" current file
nnoremap <silent> [unite]<Space> :<C-u>Unite -auto-preview -buffer-name=search-buffer line<CR>
nnoremap <silent> [unite]rs :<C-u>UniteResume search-buffer<CR>
" buffer list
nnoremap <silent> [unite]b :<C-u>Unite buffer<CR>
" command list
nnoremap <silent> [unite]x :<C-u>Unite command<CR>
" change history
vnoremap <silent> [unite]c :<C-u>Unite change<CR>
" file list
nnoremap <silent> [unite]f :<C-u>Unite file<CR>
nnoremap <silent> [unite]ff : <C-u>Unite file_mru<CR>
" grep
nnoremap <silent> [unite]g :<C-u>Unite grep -buffer-name=grep-buffer<CR>
" show last unite grep result
nnoremap <silent> [unite]rg :<C-u>UniteResume grep-buffer<CR>
" history/yank
nnoremap <silent> [unite]y :<C-u>Unite history/yank<CR>
" outline
nnoremap <silent> [unite]o :<C-u>Unite outline<CR>
" help
nnoremap <silent> [unite]h :<C-u>Unite help<CR>
" show last unite result
nnoremap <silent> [unite]r :<C-u>UniteResume<CR>

autocmd FileType unite nnoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
autocmd FileType unite inoremap <silent> <buffer> <expr> <C-j> unite#do_action('split')
autocmd FileType unite nnoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')
autocmd FileType unite inoremap <silent> <buffer> <expr> <C-l> unite#do_action('vsplit')

" close unite with twice <Esc>
autocmd FileType unite nmap <silent> <buffer> <Esc><Esc> q
autocmd FileType unite imap <silent> <buffer> <Esc><Esc> <Esc>q
"}}}

" gundo "{{{
" ---------------------------------------------------------------------
nnoremap <silent> <Leader>uu :<C-u>GundoToggle<CR>
autocmd FileType gundo nnoremap <silent> <buffer> <Esc><Esc> :<C-u>GundoHide<CR>
autocmd FileType gundo inoremap <silent> <buffer> <Esc><Esc> :<C-u>GundoHide<CR>
autocmd FileType gundo nnoremap <silent> <buffer> q :<C-u>GundoHide<CR>
"}}}

" clear status
nmap <Esc><Esc> <Plug>(anzu-clear-search-status)

"}}}

" vim:foldmethod=marker:foldlevel=0
