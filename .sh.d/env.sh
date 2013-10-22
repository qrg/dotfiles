# Common shell environment variable with bash and zsh --------------------------

export LANG=ja_JP.UTF-8

#export EDITOR=emacsclient
#export VISUAL=emacsclient
#export ALTERNATE_EDITOR=emacs

# PATH ----------------------------------------------------------------
export PATH="/usr/local/bin:/usr/local/sbin:/usr/local/share/bin:$PATH"

# rbenv & phpenv --------------------------------
# .rbenv ディレクトリが home にあればパスを通す
# phpenv は rbenv を内部利用しているため先に rbenv に PATH を通す
if [ -d $HOME/.rbenv/bin ]; then
  export RBENV_ROOT=$HOME/.rbenv
  export PATH=$RBENV_ROOT/bin:$PATH
  eval "$(rbenv init -)"

  if [ -d $HOME/.phpenv/bin ]; then
    export PATH=$HOME/.phpenv/bin:$PATH
    eval "$(phpenv init -)"
  fi
elif [-d /usr/local/rbenv ]; then
  export RBENV_ROOT=/usr/local/rbenv
  export PATH=$RBENV_ROOT/bin:$RBENV_ROOT/shims:$PATH
  eval "$(rbenv init -)"
fi

# pyvenv ------------------------------------
if [ -d $HOME/.pyvenv/bin ]; then
    source $HOME/.pyvenv/bin/activate
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


