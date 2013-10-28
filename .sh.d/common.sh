# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
EnvFile=${CommonDir}/env.sh
AliasesFile=${CommonDir}/aliases.sh

NvmFile=~/.nvm/nvm.sh

CommonPluginDir=${CommonDir}/plugin
GitCompletionDir=${CommonPluginDir}/git-completion

# -----------------------------------------------
# shell environment variable
# -----------------------------------------------
if [ -e ${EnvFile} ]; then
    source ${EnvFile}
fi

# -----------------------------------------------
# aliases
# -----------------------------------------------
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.
if [ -e ${AliasesFile} ]; then
    source ${AliasesFile}
fi

# -----------------------------------------------
# nvm
# -----------------------------------------------
if [ -e ${NvmFile} ]; then
    source ${NvmFile}
    #nvm use default
    nvm use v0.10.21
fi

# -----------------------------------------------
# complement
# -----------------------------------------------
if [ -e ${GitCompletionDir} ]; then
    case `ps -p $$ | grep -e zsh -e bash` in
        *-bash*)
            source ${GitCompletionDir}/git-completion.bash
        ;;
        *-zsh*)
            zstyle ':completion:*:*:git:*' script ${GitCompletionDir}/git-completion.bash
            fpath=(${GitCompletionDir} $fpath)
        ;;
    esac
fi
