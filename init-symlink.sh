#!/bin/sh

ln -sivfn dotfiles/sh/ ${HOME}/.sh
ln -sivfn ${HOME}/dotfiles/bash_profile ${HOME}/.bash_profile
ln -sivfn ${HOME}/dotfiles/bashrc ${HOME}/.bashrc
ln -sivfn ${HOME}/dotfiles/zshrc ${HOME}/.zshrc
ln -sivfn ${HOME}/dotfiles/zshenv ${HOME}/.zshenv
ln -sivfn ${HOME}/dotfiles/zsh ${HOME}/.zsh
ln -sivfn ${HOME}/dotfiles/tmux.conf ${HOME}/.tmux.conf
ln -sivfn ${HOME}/dotfiles/tmux ${HOME}/.tmux
ln -sivfn ${HOME}/dotfiles/vimrc ${HOME}/.vimrc
ln -sivfn ${HOME}/dotfiles/gvimrc ${HOME}/.gvimrc
ln -sivfn ${HOME}/dotfiles/vim ${HOME}/.vim
ln -sivfn ${HOME}/dotfiles/emacs.d ${HOME}/.emacs.d

mkdir -pv ${HOME}/config/git
ln -sivfn ${HOME}/dotfiles/config/git/ignore ${HOME}/.config/git/ignore

ln -sivfn ${HOME}/dotfiles/tigrc ${HOME}/.tigrc
#ln -Fisv ${HOME}/dotfiles/mayu ${HOME}/.mayu

