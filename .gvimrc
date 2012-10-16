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
" a  �r�W���A�����[�h�őI�������������V�X�e���̃N���b�v�{�[�h�ɓ���B���̃A�v���P�[�V�����ƃN���b�v�{�[�h�����L����I�v�V����
" A  "a"�Ɏ��Ă��邪�Acommand-line �őI�������Ƃ����Actrl + shift �������Ȃ���}�E�X�őI�������Ƃ��ɋ��L�̃N���b�v�{�[�h���g�p����
" c  �ȒP�Ȏ�����|�b�v�A�b�v�_�C�A���O�ł͂Ȃ��A�R���\�[�����g���悤�ɂ���
" e  tab��GUI��
" f  �V�F��������s���ꂽ�Ƃ���fork()���Ȃ��B-f�I�v�V�����ŋN�������̂Ɠ���
" i  Vim�̃A�C�R�����g�p����B
" m  ���j���[��\������B
" M  "$VIMRUNTIME/menu.vim"��ǂݍ��܂Ȃ�����
" g  �g�p�ł��Ȃ����j���[���O���[�\������
" t  ���j���[�̐؂藣�����o����悤�ɂ���
" T  �c�[���o�[��\������
" r  �E�B���h�E�̉E���ɃX�N���[���o�[��\������
" R  �c�ɕ������ꂽ�E�B���h�E�̉E���ɃX�N���[���o�[��\������
" l  �E�B���h�E�̍��ɃX�N���[���o�[��\������
" L  �c�ɕ������ꂽ�E�B���h�E�̍����ɃX�N���[���o�[��\������
" b  �����X�N���[���o�[��\������
" v  �_�C�A���O�̃{�^�����c�ɔz�u����
" p  �|�C���^�R�[���o�b�N���g��
" F  ���b�Z�[�W�t�b�^�[��\������
set guioptions-=T
set guioptions-=m

