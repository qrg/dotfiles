
let g:syntastic_check_on_open=0
let g:syntastic_check_on_save=1
let g:syntastic_check_on_wq=0
let g:syntastic_auto_loc_list=1
let g:syntastic_loc_list_height=6
set statusline+=%#warningmsg# "エラーメッセージの書式
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*
let g:syntastic_javascript_checkers = ['eslint']
let g:syntastic_mode_map = {
      \ 'mode': 'active',
      \ 'active_filetypes': ['javascript'],
      \ 'passive_filetypes': []
      \ }
" vim:foldmethod=marker:
