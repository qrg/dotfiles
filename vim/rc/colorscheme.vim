set t_8f=[38;2;%lu;%lu;%lum
set t_8b=[48;2;%lu;%lu;%lum
"set t_Co=256
set termguicolors

set background=dark

"{{{ gruvbox
"colorscheme gruvbox
"}}}

"{{{ Solarized
" colorscheme solarized
"let g:solarized_hitrail=1    "default 0

" The following items are available options, but do not need to be
" included in your .vimrc as they are currently set to their defaults.

"let g:solarized_termtrans=0
"let g:solarized_degrade=0
"let g:solarized_bold=1
"let g:solarized_underline=1
"let g:solarized_italic=1
"let g:solarized_termcolors=256
"let g:solarized_contrast="normal"
"let g:solarized_visibility="normal"
"let g:solarized_diffmode="normal"
"let g:solarized_menu=1
"}}}

"{{{ Solarized 8

" https://github.com/lifepillar/vim-solarized8#troubleshooting
colorscheme solarized8_low
let g:solarized_termtrans=1
"let g:solarized_visibility="low"
let g:solarized_term_italics=1
let g:solarized_statusline=1
"}}}

"{{{ default
"hi TabLine ctermbg=black
"hi TabLineFill ctermbg=black
"hi LineNr ctermfg=black
"hi ColorColumn ctermbg=black
"hi Folded ctermbg=black
"colorscheme default
"}}}


" vim:foldmethod=marker:

