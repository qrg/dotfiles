# -----------------------------------------------
# path config
# -----------------------------------------------
CommonDir=~/dotfiles/.sh.d
EnvFile=${CommonDir}/env.sh
AliasesFile=${CommonDir}/aliases.sh

CommonPluginDir=${CommonDir}/plugin
GitCompletionDir=${CommonPluginDir}/git-completion
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

# -----------------------------------------------
# nvm
# -----------------------------------------------
if [ -e ${NvmFile} ]; then
    case `ps -p $$ | grep -e zsh -e bash` in
        *-bash*)
            source ${NvmFile}
            nvm use default
        ;;
        *-zsh*)
            source ${NvmFile}
            [ -s ~/.nvm/nvm.sh ] && . ~/.nvm/nvm.sh
            nvm use v0.10.22
        ;;
    esac
fi

# -----------------------------------------------
# tmuxinator
# -----------------------------------------------
[[ -s /Users/qurage/.tmuxinator/scripts/tmuxinator ]] && source /Users/qurage/.tmuxinator/scripts/tmuxinator


