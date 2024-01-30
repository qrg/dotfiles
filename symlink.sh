#!/bin/bash

XDG_CONFIG_HOME="${HOME}/.config"
XDG_DATA_HOME="${HOME}/.local/share"
LOCAL_EXECUTABLE_DIR="${HOME}/.local/bin"

_repo="${HOME}/workspace/git/github.com/qrg/dotfiles"

mkdir -p "${XDG_CONFIG_HOME}/alacritty"
ln -isvn "${_repo}/alacritty/alacritty.toml" "${XDG_CONFIG_HOME}/alacritty/alacritty.toml"

mkdir -p "${XDG_CONFIG_HOME}/bash"
ln -isvn "${_repo}/bash/bashrc" "${HOME}/.bashrc"

mkdir -p "${XDG_CONFIG_HOME}/fish"
mkdir -p "${XDG_CONFIG_HOME}/fish/functions"
ln -isvn "${_repo}/fish/completions" "${XDG_CONFIG_HOME}/fish/completions"
ln -isvn "${_repo}/fish/functions/fish_prompt.fish" "${XDG_CONFIG_HOME}/fish/functions/fish_prompt.fish"
ln -isvn "${_repo}/fish/functions/fish_user_key_bindings.fish" "${XDG_CONFIG_HOME}/fish/functions/fish_user_key_bindings.fish"
ln -isvn "${_repo}/fish/config.fish" "${XDG_CONFIG_HOME}/fish/config.fish"
ln -isvn "${_repo}/fish/fish_plugins" "${XDG_CONFIG_HOME}/fish/fish_plugins"

mkdir -p "${XDG_CONFIG_HOME}/git"
ln -isvn "${_repo}/git/config" "${XDG_CONFIG_HOME}/git/config"
ln -isvn "${_repo}/git/attributes" "${XDG_CONFIG_HOME}/git/attributes"
ln -isvn "${_repo}/git/ignore" "${XDG_CONFIG_HOME}/git/ignore"

mkdir -p "${XDG_CONFIG_HOME}/nvim"
ln -isvn "${_repo}/nvim/init.vim" "${XDG_CONFIG_HOME}/nvim/init.vim"

mkdir -p "${XDG_CONFIG_HOME}/peco"
ln -isvn "${_repo}/peco/config.json" "${XDG_CONFIG_HOME}/peco/config.json"

mkdir -p "${XDG_CONFIG_HOME}/shell"
ln -isvn "${_repo}/shell/aliases.sh" "${XDG_CONFIG_HOME}/shell/aliases.sh"
ln -isvn "${_repo}/shell/env.sh" "${XDG_CONFIG_HOME}/shell/env.sh"

mkdir -p "${XDG_CONFIG_HOME}/tig"
mkdir -p "${XDG_DATA_HOME}/tig"
ln -isvn "${_repo}/tig/colorscheme.tigrc" "${XDG_CONFIG_HOME}/tig/colorscheme.tigrc"
ln -isvn "${_repo}/tig/config" "${XDG_CONFIG_HOME}/tig/config"
ln -isvn "${_repo}/tig/keybindings.tigrc" "${XDG_CONFIG_HOME}/tig/keybindings.tigrc"

mkdir -p "${XDG_CONFIG_HOME}/tmux"
ln -isvn "${_repo}/tmux/tmux.conf" "${XDG_CONFIG_HOME}/tmux/tmux.conf"

mkdir -p "${XDG_CONFIG_HOME}/zsh"
ln -isvn "${_repo}/zsh/.zshenv" "${HOME}/.zshenv"
ln -isvn "${_repo}/zsh/.zprofile" "${XDG_CONFIG_HOME}/zsh/.zprofile"
ln -isvn "${_repo}/zsh/.zshrc" "${XDG_CONFIG_HOME}/zsh/.zshrc"

mkdir -p $LOCAL_EXECUTABLE_DIR
ln -isvn "${_repo}/bin/clipboard-copy.sh" "${LOCAL_EXECUTABLE_DIR}/clipboard-copy.sh"

unset _repo
