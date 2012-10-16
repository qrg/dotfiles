# User specific aliases and functions -----------------------------------------

alias rm='rm -iv'                # -i, --interactive  prompt before any removal
alias cp='cp -iv'                # -i, --interactive  prompt before overwrite
alias mv='mv -iv'                # -i, --interactive  prompt before overwrite
alias mkdir='mkdir -v'
alias rmdir='rmdir -v'
alias ls='ls --color=auto'
alias ll='ls -l --color=auto'
alias la='ls -lap --color=auto'
alias chown='chown -v'
alias chmod='chmod -v'
alias cat='cat -n'              # -n, --number    number all output lines

alias wget='wget -cvS'
# -c, --continue
#     Continue getting a partially-downloaded file.
#     only works with FTP servers and with HTTP servers that support the "Range" header.
# -S, --server-response
#     Print the headers sent by HTTP servers and responses sent by FTP servers.

alias pd='popd'

alias vi='vim'
#alias emacs='emacsclient -n -f ~/.emacs.d/var/server/server -a emacs'
#alias e='emacsclient -n -f ~/.emacs.d/var/server/server -a emacs'

alias cal='LANG=en_US.UTF-8 cal -3'
alias man='LANG=ja_JP.UTF-8 man'
alias sudo='sudo '              # enable aliases with sudo

alias w3m='w3m -B'
alias tmux='tmux -2'
alias mysql="mysql --pager='less -S'"

# git aliases ---------------------------------------------------------
alias g="git"
alias gs="git status"
#alias gll="git log --color-words --color --graph --name-status --abbrev-commit --decorate --branches"
alias gl="git log --pretty='medium-reverse' --graph --name-status"
alias glmin="git log --graph --pretty=format:'%Cred%h%Creset -%C(yellow reverse)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset' --abbrev-commit"
alias ga="git add"
alias gc="git commit -v"
alias gco="git checkout"
alias gd="git diff"
alias gb="git branch"
alias gh="git help"
alias gpl="git pull"
alias gps="git push"

