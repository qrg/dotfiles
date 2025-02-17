set --local _os (uname)

# remove greeting message
set fish_greeting
# show cwd as full path in a prompt
set --global --export fish_prompt_pwd_dir_length 0

# Base enviroment variables
# ------------------------------------------------------------------------------
set --global --export XDG_CONFIG_HOME "$HOME/.config"
set --global --export XDG_CACHE_HOME "$HOME/.cache"
set --global --export XDG_DATA_HOME "$HOME/.local/share"
set --global --export XDG_STATE_HOME "$HOME/.local/state"

set --global --export LANG 'en_US.UTF-8'
set --global --export LC_ALL 'en_US.UTF-8'
set --global --export LC_COLLATE 'ja_JP.UTF-8' # 正規表現のマッチング (範囲表現と等価クラスのマッチングを決定する) と文字列の照合 (collation)
set --global --export LC_CTYPE 'ja_JP.UTF-8' # 正規表現のマッチング、文字の分類、文字の変換、大文字小文字比較、 ワイド文字関数
set --global --export LC_MESSAGES 'ja_JP.UTF-8' # 地域化可能な自然言語メッセージ
set --global --export LC_MONETARY 'ja_JP.UTF-8' # 通貨の書式
set --global --export LC_NUMERIC 'ja_JP.UTF-8' # 数値の書式 (小数点や 3 桁ごとの区切り)
set --global --export LC_TIME 'en_US.UTF-8' # 時刻と日付けの書式

# local config
# ------------------------------------------------------------------------------
if test -f "$XDG_CONFIG_HOME/fish/config.local.fish"
  source "$XDG_CONFIG_HOME/fish/config.local.fish"
end

# PATH
# ------------------------------------------------------------------------------

fish_add_path "$HOME/workspace/scripts" "$HOME/.local/bin" /usr/local/bin /usr/local/sbin

# Visual Studio Code on WSL
if test -n $WSLENV
  fish_add_path "/mnt/c/Users/$USER/AppData/Local/Programs/Microsoft VS Code/bin"
end

if test "$_os" = Darwin; and type -q brew
  fish_add_path "/opt/homebrew/bin"
  # add executable pathes that was installed with Homebrew
  eval (brew shellenv)
  set --global --export HOMEBREW_CASK_OPTS "--appdir=$HOME/Applications --fontdir=$HOME/Library/Fonts"
  set --global --export GUILE_TLS_CERTIFICATE_DIRECTORY /usr/local/etc/gnutls/ # https://formulae.brew.sh/formula/gnutls
  set --global --export LIBRARY_PATH /opt/homebrew/lib:$LIBRARY_PATH
  set --global --export CPATH /opt/homebrew/include:$CPATH
end

# nodenv
if type -q nodenv
  fish_add_path "$XDG_DATA_HOME/nodenv/bin" "$XDG_DATA_HOME/nodenv/shims"
  status --is-interactive; and source (nodenv init - | psub)
  if type -q brew
    if test -e (brew --prefix node-build-update-defs)
      set --global --export NODE_BUILD_DEFINITIONS (brew --prefix node-build-update-defs)"/share/node-build"
    end
  end
end

# rbenv
if test -e "$XDG_DATA_HOME/rbenv"
  fish_add_path "$XDG_DATA_HOME/rbenv/shims"
  status --is-interactive; and source (rbenv init - | psub)
end

# pnpm
if test -e "$XDG_DATA_HOME/pnpm"
  set --global --export PNPM_HOME "$XDG_DATA_HOME/pnpm"

  if not string match -q -- $PNPM_HOME $PATH
    set --global --export PATH "$PNPM_HOME" $PATH
  end

  # tabtab source for packages
  # uninstall by removing these lines
  if test -f "$XDG_CONFIG_HOME/tabtab/fish/__tabtab.fish"
    source "$XDG_CONFIG_HOME/tabtab/fish/__tabtab.fish"
  end
end

# golang
if test -e "$XDG_DATA_HOME/go"
  set --global --export GOPATH "$XDG_DATA_HOME/go"
  fish_add_path "$GOPATH/bin"
end

# rust
if test -e "$XDG_DATA_HOME/cargo"
  set --global --export CARGO_HOME "$XDG_DATA_HOME/cargo"
  set --global --export RUSTUP_HOME "$XDG_DATA_HOME/rustup"
  fish_add_path "$CARGO_HOME/bin"
end

# java
if test -e "$HOMEBREW_PREFIX/opt/openjdk/bin/java"
  fish_add_path "$HOMEBREW_PREFIX/opt/openjdk/bin"
end


# mice
# https://mise.jdx.dev/
if type -q mise or "$XDG_CONFIG_HOME/mise/config.toml"
  mise activate fish | source
end

# unique $PATH
set --local _path
for p in $PATH
  if not contains $p $_path
    set --append _path $p
  end
end

set --global --export PATH $_path

# Enviroment variables
# ------------------------------------------------------------------------------

# https://github.com/fish-shell/fish-shell/issues/7701#issuecomment-775719271
if test "$COLORTERM" = truecolor; \
  or test "$TERM" = "alacritty"; \
  or string match --all --ignore-case --quiet "*-256color" "$TERM"
  set -g fish_term24bit 1
end

set --global --export EDITOR nvim
set --global --export GPG_TTY (tty)
set --global --export GNUPGHOME "$XDG_DATA_HOME/gnupg"

set --global --export MANWIDTH 100
set --global --export LESSCHARSET utf-8
set --global --export LESSHISTFILE "$XDG_DATA_HOME/less/history"

set --global --export FISH_SHELL_PATH (which fish)
set --global --export GIBO_BOILERPLATES "$XDG_DATA_HOME/gibo-boilerplates"

set --global --export NPM_CONFIG_USERCONFIG "$XDG_CONFIG_HOME/npm/config"

# Man pages
# ------------------------------------------------------------------------------
if type -q source-highlight
  set --global --export LESS -R
  set --global --export LESSOPEN '| src-hilite-lesspipe.sh %s'
end

set --global --export MANPAGER 'less -R'
set --global --export LESS_TERMCAP_mb (tput bold; tput setaf 2) # green
set --global --export LESS_TERMCAP_md (tput bold; tput setaf 6) # cyan
set --global --export LESS_TERMCAP_me (tput sgr0)
set --global --export LESS_TERMCAP_so (tput bold; tput setaf 3; tput setab 4) # yellow on blue
set --global --export LESS_TERMCAP_se (tput rmso; tput sgr0)
set --global --export LESS_TERMCAP_us (tput smul; tput bold; tput setaf 7) # white
set --global --export LESS_TERMCAP_ue (tput rmul; tput sgr0)
set --global --export LESS_TERMCAP_mr (tput rev)
set --global --export LESS_TERMCAP_mh (tput dim)
set --global --export LESS_TERMCAP_ZN (tput ssubm)
set --global --export LESS_TERMCAP_ZV (tput rsubm)
set --global --export LESS_TERMCAP_ZO (tput ssupm)
set --global --export LESS_TERMCAP_ZW (tput rsupm)

# starship
# if type -q starship
#  starship init fish | source
# end

# zoxide
# https://github.com/ajeetdsouza/zoxide
if type -q zoxide
  set --global --export _ZO_DATA_DIR $XDG_DATA_HOME
  zoxide init fish | source
end

# Aliases
# ------------------------------------------------------------------------------

abbr --add rm 'rm -iv' # -i, --interactive  prompt before any removal
abbr --add cp 'cp -iv' # -i, --interactive  prompt before overwrite
abbr --add mv 'mv -iv' # -i, --interactive  prompt before overwrite
abbr --add mkdir 'mkdir -v'
abbr --add rmdir 'rmdir -v'

switch "$_os"
  case Darwin
    abbr --add ls 'ls -G'
    abbr --add ll 'ls -lG'
    abbr --add la 'ls -lapG'
  case '*'
    abbr --add ls 'ls --color=auto'
    abbr --add ll 'ls -l --color=auto'
    abbr --add la 'ls -lap --color=auto'
end

abbr --add chown 'chown -v'
abbr --add chmod 'chmod -v'
abbr --add pd popd
abbr --add tree 'tree -Dghpua'

abbr --add cal 'env LANG=en_US.UTF-8 cal -3'
abbr --add man 'env LANG=ja_JP.UTF-8 man'

abbr --add wget 'wget -cvS'
# -c, --continue
#   Continue getting a partially-downloaded file.
#   only works with FTP servers and with HTTP servers that support the "Range" header.
# -S, --server-response
#   Print the headers sent by HTTP servers and responses sent by FTP servers.

abbr --add vi nvim
abbr --add vim nvim

abbr --add readus 'find . -regex "./node_modules/[^/]*/README.*" -type f | fzf --preview="less {}" --preview-window=right:60% --ansi --bind "enter:execute(open -a \"Marked 2\" {})" --delimiter=/ --with-nth=3 --header="open README"'

abbr --add rg 'rg --hidden --follow --glob "!({.git,node_modules}/*|*.lock)"'

abbr --add mysql 'mysql --pager="less -S"'

abbr --add mn 'memo new'
abbr --add ml 'memo list'
abbr --add me 'memo edit'

# Git
abbr --add g git
abbr --add gs 'git status'
#abbr --add gll 'git log --color-words --color --graph --name-status --abbrev-commit --decorate --branches'
abbr --add gl 'git log --pretty=format:"%C(red reverse)%d%Creset%C(white reverse) %h% Creset %C(green reverse) %an %Creset %C(cyan)%ar%Creset%n%C(white bold)%w(80)%s%Creset%n%n%w(80,2,2)%b" --graph --name-status'
abbr --add glmin 'git log --graph --pretty=format:"%Cred%h%Creset -%C(yellow reverse)%d%Creset %s %Cgreen(%cr) %C(bold blue)<%an>%Creset" --abbrev-commit'
abbr --add ga 'git add'
abbr --add gap 'git add --patch'
abbr --add gc 'git commit --verbose'
abbr --add gca 'git commit --verbose --amend'

abbr --add gch 'git checkout'

abbr --add gco 'git switch'
abbr --add gcoc 'git switch --create'
abbr --add gsw 'git switch'
abbr --add gswc 'git switch --create'

abbr --add gd 'git diff'
abbr --add gdh 'git diff HEAD'
abbr --add gds 'git diff --cached'
abbr --add gdt 'git difftool --no-prompt'
abbr --add gbr 'git branch'
abbr --add gbra 'git branch --all'
abbr --add gsb 'git show-branch'
abbr --add ghelp 'git help'
abbr --add gpull 'git pull'
abbr --add gpush "git push (git branch --all | grep -E '^\*' | sed -e 's/^\* //')"

# gh

# e.g. fdpr ${COMMIT_HASH}
abbr --add fdpr "gh pr list --state all --web --search"

# pnpm
abbr --add p "pnpm"

# eza
if type -q eza
  abbr la 'eza --all --git --long --group --header'
  abbr ls 'eza'
end

# Initialize
# ------------------------------------------------------------------------------

# tabtab source for packages
# uninstall by removing these lines
if test -f $XDG_CONFIG_HOME/tabtab/fish/__tabtab.fish
  source $XDG_CONFIG_HOME/tabtab/fish/__tabtab.fish
end

# WSL
if test -n "$WSLENV"

  # https://github.com/microsoft/WSL/issues/4401
  function isWinDir
    switch (pwd -P)
    case '/mnt/*'
      return 0
    case '*'
      return 1
      end
  end
  function git
    if isWinDir
      git.exe $argv
    else
      /usr/bin/git $argv
    end
  end

  # open
  set --global --export BROWSER "powershell.exe /c start"
  function open
    powershell.exe /c start $argv[1]
  end

end
