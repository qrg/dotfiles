
# PATH
# -----------------------------------------------------------------------------
export PATH="${HOME}/scripts:${HOME}/.local/bin:/usr/local/bin:/usr/local/sbin:/usr/local/share/bin:$PATH"

# macvim kaoriya
if [ -f /Applications/MacVim.app/Contents/MacOS/Vim ]; then
  export PATH="/Applications/MacVim.app/Contents/MacOS:$PATH"
  alias vi="/Applications/MacVim.app/Contents/MacOS/Vim $@"
  alias vim="/Applications/MacVim.app/Contents/MacOS/Vim $@"
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

if [ -s ${HOME}/.ndenv/bin ]; then
  export PATH="$HOME/.ndenv/bin:$PATH"
  eval "$(ndenv init -)"
fi

# golang
if [ -s ${HOME}/.go ]; then
   export GOPATH="${HOME}/.go"
   #export GOROOT=/usr/local/opt/go/libexec
   export PATH="${GOPATH}/bin:${GOROOT}/bin:${PATH}"
fi

# tmuxinator
[[ -s ${HOME}/.tmuxinator/scripts/tmuxinator ]] && source ${HOME}/.tmuxinator/scripts/tmuxinator
