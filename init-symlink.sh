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

ln -sivfn ${HOME}/dotfiles/git/config ${HOME}/.gitconfig
ln -sivfn ${HOME}/dotfiles/git/ignore ${HOME}/.gitignore
ln -sivfn ${HOME}/dotfiles/git/attributes ${HOME}/.gitattributes

ln -sivfn ${HOME}/dotfiles/tigrc ${HOME}/.tigrc
#ln -Fisv ${HOME}/dotfiles/mayu ${HOME}/.mayu
