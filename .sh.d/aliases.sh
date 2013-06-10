# User specific aliases and functions -----------------------------------------

alias rm='rm -iv'                # -i, --interactive  prompt before any removal
alias cp='cp -iv'                # -i, --interactive  prompt before overwrite
alias mv='mv -iv'                # -i, --interactive  prompt before overwrite
alias mkdir='mkdir -v'
alias rmdir='rmdir -v'

if [ $(uname) = 'Darwin' ]; then
    alias ls='ls -G'
    alias ll='ls -lG'
    alias la='ls -lapG'
else
    alias ls='ls --color=auto'
    alias ll='ls -l --color=auto'
    alias la='ls -lap --color=auto'
fi

alias chown='chown -v'
alias chmod='chmod -v'
alias cat='cat -n'              # -n, --number    number all output lines
alias pd='popd'
alias tree='tree -Dghpu'

alias cal='LANG=en_US.UTF-8 cal -3'
alias man='LANG=ja_JP.UTF-8 man'

# enable aliases with sudo
alias sudo='sudo '

alias wget='wget -cvS'
# -c, --continue
#     Continue getting a partially-downloaded file.
#     only works with FTP servers and with HTTP servers that support the "Range" header.
# -S, --server-response
#     Print the headers sent by HTTP servers and responses sent by FTP servers.

alias vi='vim'

# Emacs Server -----------------------------------------------------------------
# if emacsserver was enable, use emacsclient, or not, use emacs
#alias e='emacsclient -n -f ~/.emacs.d/local/server/server -a emacs '
#alias emacsclient='emacsclient -n -f ~/.emacs.d/local/server/server -a emacs '
alias e='emacsclient -n -a emacs '
alias emacsclient='emacsclient -n -a emacs '

# -n, --no-wait
#     Don't wait for the server to return
#
# -f SERVER, --server-file=SERVER
#     Set filename of the TCP authentication file
#
# -a EDITOR, --alternate-editor=EDITOR
#     Editor to fallback to if the server is not running
#     If EDITOR is the empty string, start Emacs in daemon
#     mode and try connecting again

alias w3m='w3m -B'
alias tmux='tmux -2'
alias mysql='mysql --pager="less -S"'

# Git --------------------------------------------------------------------------
alias g='git'
alias gs='git status'
#alias gll='git log --color-words --color --graph --name-status --abbrev-commit --decorate --branches'
alias gl='git log --pretty=format:"%C(red reverse)%d%Creset%C(white reverse) %h% Creset %C(green reverse) %an %Creset %C(cyan)%ar%Creset%n%C(white bold)%w(80)%s%Creset%n%n%w(80,2,2)%b" --graph --name-status'
alias glmin='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow reverse)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit'
alias ga='git add'
alias gc='git commit -v'
alias gco='git checkout'
alias gd='git diff'
alias gbr='git branch'
alias gbra='git branch -a'
alias gsb='git show-branch'
alias ghelp='git help'
alias gpull='git pull'
alias gpush='git push'
