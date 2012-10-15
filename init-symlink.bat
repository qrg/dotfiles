@echo off
REM 実行ファイルがあるディレクトリに移動する
cd /d "%~dp0"

mklink /D "%HOME%\.sh.d" "%HOME%\dotfiles\.sh.d"
mklink /D "%HOME%\.vim" "%HOME%\dotfiles\.vim"
mklink /D "%HOME%\.zsh.d" "%HOME%\dotfiles\.zsh.d"
mklink /H "%HOME%\.bashrc" "%HOME%\dotfiles\.bashrc"
mklink /H "%HOME%\.gitignore_global" "%HOME%\dotfiles\.gitignore_global"
mklink /H "%HOME%\.mayu" "%HOME%\dotfiles\.mayu"
mklink /H "%HOME%\.tmux.conf" "%HOME%\dotfiles\.tmux.conf"
mklink /H "%HOME%\.vimrc" "%HOME%\dotfiles\.vimrc"
mklink /H "%HOME%\.gvimrc" "%HOME%\dotfiles\.gvimrc"
mklink /H "%HOME%\.zshrc" "%HOME%\dotfiles\.zshrc"

pause
