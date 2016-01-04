source ~/.sh/env.sh

# direnv
if [ -s ${HOME}/direnv ]; then
  eval "$(direnv hook bash)"
fi

