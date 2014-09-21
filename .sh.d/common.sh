# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
EnvFile=${CommonDir}/env.sh
AliasesFile=${CommonDir}/aliases.sh

CommonPluginDir=${CommonDir}/plugin
NvmFile=~/.nvm/nvm.sh

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
    case `ps -p $$ | grep -e zsh -e bash` in
        *-bash*|*\ bash*)
            source ${NvmFile}
            nvm use default
        ;;
        *-zsh*|*\ zsh*)
            source ${NvmFile}
            [[ -s ${HOME}/.nvm/nvm.sh ]] && . ${HOME}/.nvm/nvm.sh
            nvm use v0.10.32
        ;;
    esac
fi

# -----------------------------------------------
# tmuxinator
# -----------------------------------------------
[[ -s ~/.tmuxinator/scripts/tmuxinator ]] && source ~/.tmuxinator/scripts/tmuxinator

