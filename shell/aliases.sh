# User specific aliases and functions -----------------------------------------

alias rm='rm -iv'                # -i, --interactive  prompt before any removal
alias cp='cp -iv'                # -i, --interactive  prompt before overwrite
alias mv='mv -iv'                # -i, --interactive  prompt before overwrite
alias mkdir='mkdir -v'
alias rmdir='rmdir -v'

if [ $(uname) == 'Darwin' ]; then
    alias ls='ls -G'
    alias ll='ls -lG'
    alias la='ls -lapG'
else
    alias ls='ls --color=auto'
    alias ll='ls -l --color=auto'
    alias la='ls -lap --color=auto'
fi

# eza
if type direnv > /dev/null 2>&1; then
    alias ls='eza'
    alias la='eza --all --git --long --group --header'
fi

alias chown='chown -v'
alias chmod='chmod -v'
alias pd='popd'
alias tree='tree -Dghpua'

alias cal='LANG=en_US.UTF-8 cal -3'
alias man='LANG=ja_JP.UTF-8 man'

# enable aliases with sudo
alias sudo='sudo env PATH=$PATH'

alias wget='wget -cvS'
# -c, --continue
#     Continue getting a partially-downloaded file.
#     only works with FTP servers and with HTTP servers that support the "Range" header.
# -S, --server-response
#     Print the headers sent by HTTP servers and responses sent by FTP servers.

alias vi='nvim'
alias vim='nvim'

alias readus='find . -regex "./node_modules/[^/]*/README.*" -type f | fzf --preview="less {}" --preview-window=right:60% --ansi --bind "enter:execute(open -a \"Marked 2\" {})" --delimiter=/ --with-nth=3 --header="open README"'

alias rg='rg --hidden --follow --glob "!({.git,node_modules}/*|*.lock)"'

alias w3m='w3m -B'
alias tmux='tmux -2u'
alias mysql='mysql --pager="less -S"'

alias mn='memo new'
alias ml='memo list'
alias me='memo edit'

alias cdg='cd $(ghq list -p | peco)'

# Git --------------------------------------------------------------------------
alias g='git'
alias gs='git status'
#alias gll='git log --color-words --color --graph --name-status --abbrev-commit --decorate --branches'
alias gl='git log --pretty=format:"%C(red reverse)%d%Creset%C(white reverse) %h% Creset %C(green reverse) %an %Creset %C(cyan)%ar%Creset%n%C(white bold)%w(80)%s%Creset%n%n%w(80,2,2)%b" --graph --name-status'
alias glmin='git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow reverse)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit'
alias ga='git add'
alias gc='git commit -v'
alias gca='git commit -v --amend'
alias gco='git checkout'
alias gsw='git switch'
alias gswc='git switch --create'

alias gd='git diff'
alias gds='git diff --cached'
alias gdt='git difftool -y'
alias gbr='git branch'
alias gbra='git branch -a'
alias gsb='git show-branch'
alias ghelp='git help'
alias gpull='git pull'
alias gpush='git push'
