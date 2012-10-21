# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
AliasesFile=${CommonDir}/aliases.sh
EnvFile=${CommonDir}/env.sh

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
# complement
# -----------------------------------------------
if [ -f ${GitCompletionFile} ]; then
	source ${GitCompletionFile}
fi
