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


