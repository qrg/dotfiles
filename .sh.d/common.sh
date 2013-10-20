# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
AliasesFile=${CommonDir}/aliases.sh
EnvFile=${CommonDir}/env.sh

NvmFile=~/.nvm/nvm.sh

CommonPluginDir=${CommonDir}/plugin
GitCompletionFile=${CommonPluginDir}/git-completion.bash

# -----------------------------------------------
# shell environment variable
# -----------------------------------------------
if [ -f ${EnvFile} ]; then
	source ${EnvFile}
fi

# -----------------------------------------------
# aliases
# -----------------------------------------------
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -f ${AliasesFile} ]; then
	source ${AliasesFile}
fi

# -----------------------------------------------
# nvm
# -----------------------------------------------
if [ -f ${NvmFile} ]; then
	source ${NvmFile}
    nvm use default
fi

# -----------------------------------------------
# complement
# -----------------------------------------------
if [ -f ${GitCompletionFile} ]; then
	source ${GitCompletionFile}
fi

# rbenv & phpenv -------------------------------------------------------
# .rbenv ディレクトリが home にあればパスを通す
# phpenv は rbenv を内部利用しているため先に rbenv に PATH を通す
if [ -d $HOME/.rbenv/bin ]; then
  export PATH=$PATH:$HOME/.rbenv/bin

  eval "$(rbenv init -)"

  if [ -d $HOME/.phpenv/bin ]; then
    export PATH=$PATH:$HOME/.rbenv/bin:$HOME/.phpenv/bin
    eval "$(phpenv init -)"
  fi

fi

# MacOS
if [ `uname` = 'Darwin' -a -d /usr/local/bin/rbenv/bin ]; then
  export RBENV_ROOT=$HOME/.rbenv
  export PATH=$HOME/.rbenv/bin:$PATH
  eval "$(rbenv init -)"

  # for homebrew
  export PATH=/usr/local/bin:$PATH
  export PATH=/usr/local/sbin:$PATH
fi

# CentOS
if [ `uname` = 'Linux' -a -d /usr/local/rbenv/bin ]; then
  export PATH=$PATH:/usr/local/rbenv/bin
  export RBENV_DIR="/usr/local/rbenv"
  export RBENV_ROOT="/usr/local/rbenv"
  eval "$(rbenv init -)"
fi

