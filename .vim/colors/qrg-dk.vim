set background=dark

hi clear

if exists("syntax_on")
  syntax reset
endif

let colors_name = "qrg-dk"

" Vim >= 7.0 specific colors
if version >= 700
  hi CursorLine   guibg=#222222
  hi CursorColumn guibg=#222222
  hi MatchParen   guifg=#f6f3e8 guibg=#857b6f gui=bold
  hi Pmenu        guifg=#f6f3e8 guibg=#444444
  hi PmenuSel     guifg=#000000 guibg=#cae682
endif

" General colors
hi Cursor         guifg=#000000 guibg=#ffffff
hi Normal         guifg=#f6f3e8 guibg=#151515 gui=none
hi LineNr         guifg=#555555 guibg=#222222 gui=none
hi StatusLine     guifg=#f6f3e8 guibg=#444444 gui=none
hi StatusLineNC   guifg=#857b6f guibg=#444444 gui=none
hi VertSplit      guifg=#444444 guibg=#444444 gui=none
hi Folded         guibg=#384048 guifg=#a0a8b0 gui=none
hi Title          guifg=#f6f3e8 guibg=NONE    gui=bold
hi Visual         guifg=#f6f3e8 guibg=#444444 gui=none
hi SpecialKey     guifg=#444444 guibg=NONE    gui=none
hi NonText        guifg=#444444 guibg=NONE    gui=none

" text width
hi ColorColumn    guifg=Red guibg=#666666

hi Question       guifg=#95e454 guibg=NONE gui=none

" Syntax highlighting
hi Comment        guifg=#99968b guibg=#333333 gui=none
hi Todo           guifg=#8f8f8f gui=none
hi Constant       guifg=#e5786d gui=none
hi String         guifg=#95e454 gui=none
hi Identifier     guifg=#cae682 gui=none
hi Function       guifg=#cae682 gui=none
hi Type           guifg=#cae682 gui=none
hi Statement      guifg=#8ac6f2 gui=none
hi Keyword        guifg=#8ac6f2 gui=none
hi PreProc        guifg=#e5786d gui=none
hi Number         guifg=#e5786d gui=none
hi Special        guifg=#e7f6da gui=none




