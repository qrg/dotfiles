@echo off
REM 実行ファイルがあるディレクトリに移動する
cd /d "%~dp0"

mklink /D "%HOME%\.sh.d" "%HOME%\dotfiles\.sh.d"
mklink /D "%HOME%\.vim" "%HOME%\dotfiles\.vim"
mklink /D "%HOME%\.zsh.d" "%HOME%\dotfiles\.zsh.d"
mklink "%HOME%\.bashrc" "%HOME%\dotfiles\.bashrc"
mklink "%HOME%\.gitignore_global" "%HOME%\dotfiles\.gitignore_global"
mklink "%HOME%\.mayu" "%HOME%\dotfiles\.mayu"
mklink "%HOME%\.tmux.conf" "%HOME%\dotfiles\.tmux.conf"
mklink "%HOME%\.vimrc" "%HOME%\dotfiles\.vimrc"
mklink "%HOME%\.zshrc" "%HOME%\dotfiles\.zshrc"

pause

REM mklink /D .vim .\dotfiles\dot.vim
REM mklink /H _vimrc .\dotfiles\dot.vimrc
REM mklink /H _gvimrc .\dotfiles\dot.gvimrc