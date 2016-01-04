# Common shell environment variable with bash and zsh
# =============================================================================

export LANG=ja_JP.UTF-8

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

# PATH
# -----------------------------------------------------------------------------
export PATH=${HOME}/scripts:${HOME}/.local/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/bin:$PATH

# macvim kaoriya
if [ -f /Applications/MacVim.app/Contents/MacOS/Vim ]; then
  export PATH=/Applications/MacVim.app/Contents/MacOS:$PATH
  alias vi="/Applications/MacVim.app/Contents/MacOS/Vim $@"
  alias vim="/Applications/MacVim.app/Contents/MacOS/Vim $@"
fi

# rbenv & phpenv
# .rbenv/ が home にあればパスを通す
# phpenv は rbenv を内部利用しているため先に rbenv に PATH を通す
if [ -d ${HOME}/.rbenv ]; then
  export PATH="${HOME}/.rbenv/bin:${HOME}/.rbenv/shims:$PATH"
  
  eval "$(rbenv init -)"

  # phpenv
  if [ -d ${HOME}/.phpenv/bin ]; then
    export PATH=${HOME}/.phpenv/bin:$PATH
    eval "$(phpenv init -)"
  fi
fi

# pyenv
if [ -d ${HOME}/.pyenv ]; then
  export PATH="${HOME}/.pyenv/shims:${PATH}"
  eval "$(pyenv init -)"
fi


if [ -s ${HOME}/.nvm/nvm.sh ]; then
  source ${HOME}/.nvm/nvm.sh
  nvm use stable
  echo ${NVM_PATH}
  export PATH=${NVM_PATH}:$PATH
fi

if [ -s ${HOME}/.ndenv/bin ]; then
  export PATH="$HOME/.ndenv/bin:$PATH"
  eval "$(ndenv init -)"
fi

# golang
if [ -s ${HOME}/.go ]; then
   export GOPATH=${HOME}/.go
   #export GOROOT=/usr/local/opt/go/libexec
   export PATH=${GOPATH}/bin:${GOROOT}/bin:${PATH}
fi

# tmuxinator
[[ -s ${HOME}/.tmuxinator/scripts/tmuxinator ]] && source ${HOME}/.tmuxinator/scripts/tmuxinator

# colorful man pages
# -----------------------------------------------------------------------------
if [ `which source-highlight` ]; then
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

# cygwin
# -----------------------------------------------------------------------------
# cygwin の場合にだけ読み込む
cygwin=false

case "$(uname)" in
  CYGWIN*) cygwin=true;;
esac

if $cygwin; then
  unset TMP
  unset TEMP
  export TMP=/tmp
  export TEMP=/tmp
fi

