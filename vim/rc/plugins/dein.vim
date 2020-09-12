if &compatible
  set nocompatible               " Be iMproved
endif

let s:plugin_dir = expand('$HOME/.vim/dein/')
let s:dein_dir = s:plugin_dir . 'repos/github.com/Shougo/dein.vim'
execute 'set runtimepath+=' . s:dein_dir

" Install dein.vim if it was not exist
if !isdirectory(s:dein_dir)
  call mkdir(s:dein_dir, 'p')
  silent execute printf('!git clone %s %s', 'https://github.com/Shougo/dein.vim', s:dein_dir)
endif

if dein#load_state(s:plugin_dir)
  " Required:
  call dein#begin(s:plugin_dir)

  " plugins {{{
  " Let dein manage dein
  " Required:
  call dein#add('Shougo/dein.vim')

  " Add or remove your plugins here:

  " unite
  call dein#add('Shougo/vimproc.vim', {'build' : 'make'})
  call dein#add('Shougo/unite.vim', { 'depends' : [ 'Shougo/vimproc.vim' ] })

  call dein#add('Shougo/unite-outline', { 'depends' : [ 'Shougo/unite.vim' ] })
  call dein#add('Shougo/unite-help', { 'depends' : [ 'Shougo/unite.vim' ] })
  call dein#add('Shougo/neoyank.vim', {  'depends' : [ 'Shougo/unite.vim' ] })
  call dein#add('ujihisa/unite-colorscheme', { 'depends' : [ 'Shougo/unite.vim' ] })
  call dein#add('thinca/vim-unite-history', { 'depends' : [ 'Shougo/unite.vim' ] })
  call dein#add('Shougo/neomru.vim', { 'depends' : [ 'Shougo/unite.vim' ] })

  " filer
  call dein#add('Shougo/vimfiler')

  " shell
  call dein#add('Shougo/vimshell')

  " completion
  call dein#add('Shougo/deoplete.nvim')
  if !has('nvim')
    call dein#add('roxma/nvim-yarp')
    call dein#add('roxma/vim-hug-neovim-rpc')
  endif

  let g:deoplete#enable_at_startup = 1

  " snippet
  call dein#add('Shougo/deoppet.nvim')

  call dein#add('kana/vim-smartchr')
  call dein#add('tpope/vim-surround')
  call dein#add('tyru/caw.vim')

  " cursor
  call dein#add('terryma/vim-multiple-cursors')

  " undo history
  call dein#add('sjl/gundo.vim')

  " textobj
  call dein#add('kana/vim-textobj-user')
  call dein#add('kana/vim-textobj-fold')
  call dein#add('kana/vim-textobj-indent')
  call dein#add('kana/vim-textobj-lastpat')
  call dein#add('osyo-manga/vim-textobj-multiblock')
  call dein#add('thinca/vim-textobj-between')
  call dein#add('rhysd/vim-textobj-anyblock')

  " operator
  call dein#add('kana/vim-operator-user')
  call dein#add('rhysd/vim-operator-surround')

  " formatting
  call dein#add('vim-scripts/Align')
  call dein#add('vim-scripts/PreserveNoEOL')
  call dein#add('editorconfig/editorconfig-vim')

  " highlighting
  call dein#add('nathanaelkane/vim-indent-guides')
  call dein#add('vim-scripts/AnsiEsc.vim')
  call dein#add('ap/vim-css-color')
  call dein#add('osyo-manga/vim-over')

  " colorscheme
  call dein#add('lifepillar/vim-solarized8')
  "call dein#add('morhetz/gruvbox')
  "call dein#add('altercation/vim-colors-solarized')
  "call dein#add('w0ng/vim-hybrid')
  "call dein#add('chriskempson/vim-tomorrow-theme')
  "call dein#add('tomasr/molokai')
  "call dein#add('flazz/vim-colorschemes')

  " statusline
  call dein#add('itchyny/lightline.vim')

  " git
  call dein#add('tpope/vim-fugitive')
  call dein#add('airblade/vim-gitgutter')

  " syntax
  call dein#add('scrooloose/syntastic')
  call dein#add('othree/html5.vim')
  call dein#add('digitaltoad/vim-jade')
  call dein#add('pangloss/vim-javascript')
  call dein#add('kchmck/vim-coffee-script')
  call dein#add('hail2u/vim-css3-syntax')
  call dein#add('cakebaker/scss-syntax.vim')
  call dein#add('wavded/vim-stylus')
  call dein#add('KohPoll/vim-less')

  " help
  call dein#add('vim-jp/vimdoc-ja')
  call dein#add('rizzatti/dash.vim')

  "}}}

  " Required:
  call dein#end()

  " Required:
  filetype plugin indent on
  syntax enable

  " If you want to install not installed plugins on startup.
  if dein#check_install()
    call dein#install()
  endif
endif

" vim:foldmethod=marker:
