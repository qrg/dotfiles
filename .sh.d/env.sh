# Common shell environment variable with bash and zsh --------------------------

export LANG=ja_JP.UTF-8
# see.) man setlocale
export LC_COLLATE=ja_JP.UTF-8  # 正規表現のマッチング (範囲表現と等価クラスのマッチングを決定する) と文字列の照合 (collation)
export LC_CTYPE=ja_JP.UTF-8    # 正規表現のマッチング、文字の分類、文字の変換、大文字小文字比較、 ワイド文字関数
export LC_MESSAGES=ja_JP.UTF-8 # 地域化可能な自然言語メッセージ
export LC_MONETARY=ja_JP.UTF-8 # 通貨の書式
export LC_NUMERIC=ja_JP.UTF-8  # 数値の書式 (小数点や 3 桁ごとの区切り)
export LC_TIME=en_US.UTF-8     # 時刻と日付けの書式

#export EDITOR=emacsclient
export EDITOR=/usr/bin/vim
if [ -s /usr/local/bin/zsh ]; then
  export SHELL=/usr/local/bin/zsh
else
  export SHELL=/bin/zsh
fi

#export VISUAL=emacsclient
#export ALTERNATE_EDITOR=emacs



# PATH ----------------------------------------------------------------
export PATH="${HOME}/scripts:/usr/local/bin:/usr/local/sbin:/usr/local/share/bin:$PATH"

# rbenv & phpenv --------------------------------
# .rbenv ディレクトリが home にあればパスを通す
# phpenv は rbenv を内部利用しているため先に rbenv に PATH を通す
if [ -d $HOME/.rbenv ]; then

  export PATH="${HOME}/.rbenv/bin:${HOME}/.rbenv/shims:$PATH"
  
  eval "$(rbenv init -)"

  # phpenv
  if [ -d $HOME/.phpenv/bin ]; then
    export PATH=$HOME/.phpenv/bin:$PATH
    eval "$(phpenv init -)"
  fi

fi



# pyenv -----------------------------------------
if [ -d $HOME/.pyenv ]; then
  export PATH="${HOME}/.pyenv/shims:${PATH}"
  eval "$(pyenv init -)"
fi

# Mac homebrew-cask
if [ $(uname) = 'Darwin' ]; then
  if type brew >/dev/null 2>&1; then
    export HOMEBREW_CASK_OPTS="--appdir=/Applications --caskroom=/usr/local/Caskroom"
  fi
fi

# cygwin --------------------------------------------------------------
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

# less and man command with colors ------------------------------------
if [ `which source-highlight` ]; then
  export LESS='-R'
  export LESSOPEN='| /usr/local/bin/src-hilite-lesspipe.sh %s'
fi


export MANPAGER='less -R'
man() {
  env \
    LESS_TERMCAP_mb=$(printf "\e[1;31m") \
    LESS_TERMCAP_md=$(printf "\e[1;31m") \
    LESS_TERMCAP_me=$(printf "\e[0m") \
    LESS_TERMCAP_se=$(printf "\e[0m") \
    LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
    LESS_TERMCAP_ue=$(printf "\e[0m") \
    LESS_TERMCAP_us=$(printf "\e[1;32m") \
    man "$@"
}
