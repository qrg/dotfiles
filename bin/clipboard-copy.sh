#!/usr/bin/env bash

pass() {
  if [ -p /dev/stdin -a  "`echo ${@}`" == "" ]; then
    cat -
  else
    echo -n "${@}"
  fi
}

if [ ! -z "${WSLENV}" ]; then
  pass | clip.exe
elif [ "$(uname)" == "Darwin" ]; then
  pass | pbcopy
elif [ "$(expr substr $(uname -s) 1 5)" == "Linux" ]; then
  # pass | xclip
  # pass | wl-copy
  # pass | xsel -bi
  if test -S "${HOME}/.local/run/clipper/clipper.sock"; then
    pass | nc -NU ~/.local/run/clipper/clipper.sock
  else
    pass | xsel --clipboard --input
  fi
fi

