# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
EnvFile=${CommonDir}/env.sh
AliasesFile=${CommonDir}/aliases.sh

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


