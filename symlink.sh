#!/bin/bash

XDG_CONFIG_HOME="${HOME}/.config"

_repo="${HOME}/workspace/git/github.com/qrg/dotfiles"

mkdir -p "${XDG_CONFIG_HOME}/alacritty"
ln -Fisvh "${_repo}/alacritty/alacritty.yml" "${XDG_CONFIG_HOME}/alacritty/alacritty.yml"

mkdir -p "${XDG_CONFIG_HOME}/bash"
ln -Fisvh "${_repo}/bash/bashrc" "${HOME}/.bashrc"

mkdir -p "${XDG_CONFIG_HOME}/fish"
mkdir -p "${XDG_CONFIG_HOME}/fish/functions"
ln -Fisvh "${_repo}/fish/completions/" "${XDG_CONFIG_HOME}/fish/completions"
ln -Fisvh "${_repo}/fish/functions/fish_prompt.fish" "${XDG_CONFIG_HOME}/fish/functions/fish_prompt.fish"
ln -Fisvh "${_repo}/fish/functions/fish_user_key_bindings.fish" "${XDG_CONFIG_HOME}/fish/functions/fish_user_key_bindings.fish"
ln -Fisvh "${_repo}/fish/config.fish" "${XDG_CONFIG_HOME}/fish/config.fish"
ln -Fisvh "${_repo}/fish/fish_plugins" "${XDG_CONFIG_HOME}/fish/fish_plugins"

mkdir -p "${XDG_CONFIG_HOME}/git"
ln -Fisvh "${_repo}/git/config" "${XDG_CONFIG_HOME}/git/config"
ln -Fisvh "${_repo}/git/attributes" "${XDG_CONFIG_HOME}/git/attributes"
ln -Fisvh "${_repo}/git/ignore" "${XDG_CONFIG_HOME}/git/ignore"

mkdir -p "${XDG_CONFIG_HOME}/nvim"
ln -Fisvh "${_repo}/nvim/init.vim" "${XDG_CONFIG_HOME}/nvim/init.vim"

mkdir -p "${XDG_CONFIG_HOME}/peco"
ln -Fisvh "${_repo}/peco/config.json" "${XDG_CONFIG_HOME}/peco/config.json"

mkdir -p "${XDG_CONFIG_HOME}/shell"
ln -Fisvh "${_repo}/shell/aliases.sh" "${XDG_CONFIG_HOME}/shell/aliases.sh"
ln -Fisvh "${_repo}/shell/env.sh" "${XDG_CONFIG_HOME}/shell/env.sh"

mkdir -p "${XDG_CONFIG_HOME}/tig"
ln -Fisvh "${_repo}/tig/colorscheme.tigrc" "${XDG_CONFIG_HOME}/tig/colorscheme.tigrc"
ln -Fisvh "${_repo}/tig/config" "${XDG_CONFIG_HOME}/tig/config"
ln -Fisvh "${_repo}/tig/keybindings.tigrc" "${XDG_CONFIG_HOME}/tig/keybindings.tigrc"

mkdir -p "${XDG_CONFIG_HOME}/tmux"
ln -Fisvh "${_repo}/tmux/tmux.conf" "${XDG_CONFIG_HOME}/tmux/tmux.conf"

mkdir -p "${XDG_CONFIG_HOME}/zsh"
ln -Fisvh "${_repo}/zsh/.zshenv" "${HOME}/.zshenv"
ln -Fisvh "${_repo}/zsh/.zprofile" "${XDG_CONFIG_HOME}/zsh/.zprofile"
ln -Fisvh "${_repo}/zsh/.zshrc" "${XDG_CONFIG_HOME}/zsh/.zshrc"

unset _repo
