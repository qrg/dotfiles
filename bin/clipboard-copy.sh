#!/usr/bin/env bash

_args=${@}

pass() {
  if [ -p /dev/stdin ] && [ "`echo $_args`" == "" ]; then
    # input from pipe
    cat -
  else
    # input from arguments
    echo -e $_args
  fi
}

if [ ! -z "${WSLENV}" ]; then
  pass | clip.exe
elif [ "$(uname)" == "Darwin" ]; then
  pass | pbcopy
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  if test -S "${HOME}/.local/run/clipper/clipper.sock"; then
    pass | nc -NU ~/.local/run/clipper/clipper.sock
  else
    pass | xsel --clipboard --input
    # pass | xclip
    # pass | wl-copy
  fi
fi
