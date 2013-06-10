# Common shell environment variable with bash and zsh --------------------------

export LANG=ja_JP.UTF-8

#export EDITOR=emacsclient
#export VISUAL=emacsclient
#export ALTERNATE_EDITOR=emacs

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

# rbenv & phpenv -------------------------------------------------------
# .rbenv ディレクトリが home にあればパスを通す
# phpenv は rbenv を内部利用しているため先に rbenv に PATH を通す
if [ -d $HOME/.rbenv/bin ]; then
  export PATH="$PATH:$HOME/.rbenv/bin"

  eval "$(rbenv init -)"

  if [ -d $HOME/.phpenv/bin ]; then
    export PATH="$PATH:$HOME/.rbenv/bin:$HOME/.phpenv/bin"
    eval "$(phpenv init -)"
  fi

fi

# MacOSX
if [ `uname` = 'Darwin' -a -d /usr/local/rbenv/bin ]; then
  export RBENV_ROOT="/usr/local/rbenv"
  export PATH="/usr/local/rbenv/bin:$PATH"
  eval "$(rbenv init -)"
fi

# CentOS
if [ `uname` = 'Linux' -a -d /usr/local/rbenv/bin ]; then
  export PATH=$PATH:/usr/local/rbenv/bin
  export RBENV_DIR="/usr/local/rbenv"
  export RBENV_ROOT="/usr/local/rbenv"
  eval "$(rbenv init -)"
fi

