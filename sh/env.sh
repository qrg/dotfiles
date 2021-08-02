# Common shell environment variable with bash and zsh
# =============================================================================

export LANG=ja_JP.UTF-8
export LESSCHARSET=utf-8

# man setlocale
export LC_COLLATE=ja_JP.UTF-8  # 正規表現のマッチング (範囲表現と等価クラスのマッチングを決定する) と文字列の照合 (collation)
export LC_CTYPE=ja_JP.UTF-8    # 正規表現のマッチング、文字の分類、文字の変換、大文字小文字比較、 ワイド文字関数
export LC_MESSAGES=ja_JP.UTF-8 # 地域化可能な自然言語メッセージ
export LC_MONETARY=ja_JP.UTF-8 # 通貨の書式
export LC_NUMERIC=ja_JP.UTF-8  # 数値の書式 (小数点や 3 桁ごとの区切り)
export LC_TIME=en_US.UTF-8     # 時刻と日付けの書式

export EDITOR=vim
#export EDITOR=emacsclient
#export VISUAL=emacsclient
#export ALTERNATE_EDITOR=emacs

export GPG_TTY=$(tty)

export MANWIDTH=100

# colorful man pages
# -----------------------------------------------------------------------------
if type source-highlight > /dev/null 2>&1; then
  export LESS='-R'
  export LESSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'
fi

export MANPAGER='less -R'
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)

# PATH
# -----------------------------------------------------------------------------
export PATH="${HOME}/scripts:${HOME}/.local/bin:/usr/local/bin:/usr/local/sbin:$PATH"

# macvim kaoriya
if [ -d /Applications/MacVim.app/Contents/MacOS ]; then
  export PATH="/Applications/MacVim.app/Contents/MacOS:$PATH"
fi

# rbenv
if [ -d ${HOME}/.rbenv ]; then
  export PATH="${HOME}/.rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# pyenv
if [ -d ${HOME}/.pyenv ]; then
  export PATH="${HOME}/.pyenv/shims:${PATH}"
  eval "$(pyenv init -)"
fi

# ndenv
if [ -s ${HOME}/.ndenv/bin ]; then
  export PATH="$HOME/.ndenv/bin:$PATH"
  eval "$(ndenv init -)"
fi

# nodenv
if [ -s ${HOME}/.nodenv ]; then
  export PATH="$HOME/.nodenv/bin:$PATH"
  eval "$(nodenv init -)"
fi

# volta
if [ -s ${$HOME}/.volta ]; then
  export VOLTA_HOME="$HOME/.volta"
  export PATH="$VOLTA_HOME/bin:$PATH"
fi

# golang
if [ -s ${HOME}/.go ]; then
  export GOPATH="${HOME}/.go"
  export PATH="${GOPATH}/bin:${PATH}"
fi

# rust
if [ -s ${HOME}/.cargo ]; then
  export PATH="$HOME/.cargo/bin:${PATH}"
  source "$HOME/.cargo/env"
fi

# tmuxinator
if [ -s ${HOME}/.tmuxinator/scripts/tmuxinator ]; then
  source ${HOME}/.tmuxinator/scripts/tmuxinator
fi

# git diff-hightlight
if [ -s /usr/local/share/git-core/contrib/diff-highlight ]; then
  export PATH="/usr/local/share/git-core/contrib/diff-highlight:${PATH}"
fi

# osx
# -----------------------------------------------------------------------------
case ${OSTYPE} in
  # macOS
  darwin*)
    export PATH="/usr/local/opt/curl/bin:/usr/local/opt/openssl/bin:$PATH"
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications --fontdir=${HOME}/Library/Fonts"
    export HOMEBREW_INSTALL_CLEANUP=1
    export GUILE_TLS_CERTIFICATE_DIRECTORY=/usr/local/etc/gnutls/
    export RUBY_CONFIGURE_OPTS="--with-openssl-dir=/usr/local/opt/openssl@1.1"
    export NODE_BUILD_DEFINITIONS=$(brew --prefix node-build-update-defs)/share/node-build
    ;;
esac
