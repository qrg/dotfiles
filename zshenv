source ${HOME}/.sh/env.sh

# direnv
if [ -s ${HOME}/direnv ]; then
   eval "$(direnv hook zsh)"
fi

