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

