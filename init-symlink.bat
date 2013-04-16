@echo off
REM 実行ファイルがあるディレクトリに移動する
cd /d "%~dp0"

mklink /D "%HOME%\.emacs.d" "%HOME%\dotfiles\.emacs.d"
mklink /D "%HOME%\.sh.d" "%HOME%\dotfiles\.sh.d"
mklink /D "%HOME%\.vim" "%HOME%\dotfiles\.vim"
mklink /D "%HOME%\.zsh.d" "%HOME%\dotfiles\.zsh.d"
mklink "%HOME%\.bashrc" "%HOME%\dotfiles\.bashrc"
mklink "%HOME%\.gitignore_global" "%HOME%\dotfiles\.gitignore_global"
mklink "%HOME%\.mayu" "%HOME%\dotfiles\.mayu"
mklink "%HOME%\.tmux.conf" "%HOME%\dotfiles\.tmux.conf"
mklink "%HOME%\.vimrc" "%HOME%\dotfiles\.vimrc"
mklink "%HOME%\.gvimrc" "%HOME%\dotfiles\.gvimrc"
mklink "%HOME%\.zshrc" "%HOME%\dotfiles\.zshrc"
mklink "%HOME%\.tidyrc" "%HOME%\dotfiles\.tidyrc"


pause
