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
alias pd='popd'


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
alias e='emacsclient -n -f ~/.emacs.d/var/server/server -a emacs '
alias emacsclient='emacsclient -n -f ~/.emacs.d/var/server/server -a emacs '
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
alias gl='git log --pretty="medium-reverse" --graph --name-status'
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
