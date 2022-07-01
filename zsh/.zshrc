# .zshrc
# cf.) man zshoptions
# cf.) http://www.gentei.org/~yuuji/rec/pc/zsh/zshcompsys.txt
#

# =============================================================================
# completion
# cf.) man zshcompsys
#
# completer
#   _complete     普通の補完関数
#   _approximate  ミススペルを訂正して補完
#   _match        ワイルドカード(*)などのグロブによってコマンドを補完
#                 e.g.) vi* と打つと vi, vim などの補完候補を表示
#   _expand       グロブや変数の展開を行う
#   _history      履歴から補完を行う。_history_complete_word から使われる
#   _prefix       カーソルの位置で補完を行う
#
# =============================================================================
fpath=(${HOME}/.zsh/site-functions $fpath)
# zsh basic completion
autoload -Uz compinit
# -d オプションで zcompdump ファイルをに指定したパス,ファイル名で保存する
compinit -u -d ${HOME}/.zsh/local/zcompdump.${HOST}.${USER}

# enable complement with sudo
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                              /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin \
                              /usr/local/git/bin

# ファイル補完候補を LS_COLORS の設定に応じてカラー表示
# LS_COLORS は ls コマンドが各file/dirを表示するときに基準とする色設定用 変数
export LS_COLORS='di=34:ln=35:so=32:pi=33:ex=31:bd=46;34:cd=43;34:su=41;30:sg=46;30:tw=42;30:ow=43;30'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# 補完の時に大文字小文字を区別しない
# ただし大文字を打った場合は小文字に変換しない
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'

# 補完候補を arrow key, C-f, C-b で選択
# Tab (C-i) を何度か押すと選択モードになり、arrow key, C-f, C-b で移動できる
# この設定を有効化する場合 C-i 選択, C-m 補完決定, を繰り返すのが楽。補完キャンセルは C-c
# select=1 だと補完してほしくないタイミングでも補完されるので 2 にする
# http://qiita.com/items/13d150c590508d518d26
zstyle ':completion:*:default' menu select=2

# 補完関数の表示を強化
# http://qiita.com/items/ed2d36698a5cc314557d
zstyle ':completion:*' verbose yes
zstyle ':completion:*:messages' format '%F{yellow}%d'$DEFAULT
zstyle ':completion:*:warnings' format '%F{red}No matches for:''%F{yellow} %d'$DEFAULT
zstyle ':completion:*:descriptions' format '%F{yellow} %B%d%b'$DEFAULT
zstyle ':completion:*:options' description 'yes'
zstyle ':completion:*:descriptions' format '%F{yellow}%B%d%b%f'$DEFAULT

# マッチ種別を別々に表示
zstyle ':completion:*' group-name ''

# command option help 表示 セパレータ設定
# `ls -a` のように打ってみて確認できる
zstyle ':completion:*' list-separator '-->'
zstyle ':completion:*:manuals' separate-sections true

# cdr (cd recent dirs) を補完
autoload -Uz chpwd_recent_dirs cdr add-zsh-hook
add-zsh-hook chpwd chpwd_recent_dirs
zstyle ':chpwd:*' recent-dirs-max 5000
zstyle ':chpwd:*' recent-dirs-default yes
zstyle ':completion:*' recent-dirs-insert both

# auto change directory
setopt auto_cd

# = 以降でも補完できるようにする(--prefix=/usr 等の場合)
setopt magic_equal_subst

# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd

# ミススペルを訂正する
setopt correct

# compacked complete list display
setopt list_packed

# 補完候補一覧にファイル種別マークを表示 ( ls -F の file type indicator)
setopt list_types

# コマンドラインでも hash(#) 以降をコメントと見なす
setopt interactive_comments

# 日本語ファイル名などの8ビットを通す
setopt print_eight_bit

# ~ や ^ など、拡張グロブで補完
# e.g.) *.txt~memo.txt とした場合、memo.txt を除外した  *.txt にマッチする
setopt extended_glob

# 明確なドットの指定なしで.から始まるファイルをマッチ
setopt glob_dots

# If a parameter is completed whose content is the name of a directory,
# then add a trailing slash.
setopt no_auto_param_slash

# aliasを展開して補完
setopt complete_aliases

# 補完後，不要な "/" を削除する/しない
# auto-fu.zsh を利用する場合、autoremoveslash を unsetopt しておかないと
# パスを補完した際にスラッシュが二重になる。
#setopt no_auto_remove_slash
unsetopt no_auto_remove_slash

# If a pattern for filename generation has no matches, print an error,
# instead of leaving it unchanged in the argument list.
# This also applies to file expansion of an initial ~ or =.
setopt no_nomatch

# 履歴による予測入力 (man zshcontrib)
autoload -U predict-on
zle -N predict-on
zle -N predict-off

# current dir は補完候補から外す
zstyle ':completion:*' ignore-parents parent pwd ..''

# man 補完でセクション番号別に表示
zstyle ':completion:*:manuals' separate-sections true

# enable case-insensitive search
zstyle ':filter-select' case-insensitive yes

# =============================================================================
# history
# =============================================================================
HISTFILE=${HOME}/.zsh/local/zsh-history
HISTSIZE=10000000
SAVEHIST=1000000

# ignore duplication command history list
setopt hist_ignore_dups
setopt hist_ignore_all_dups

setopt hist_ignore_space  # do not record if there was space at the line top
setopt share_history      # share command history data
setopt extended_history   # add timestamp with history
setopt inc_append_history # 履歴をすぐに反映
setopt hist_reduce_blanks # 余分なスペースを削除

# =============================================================================
# plugins
# =============================================================================

# =============================================================================
# prompt
#
# %B               underline
# %/ or %d         ディレクトリ (0=全て, -1=前方からの数)
# %~               ディレクトリ
# %h or %!         現在の履歴イベント番号
# %L               現在の $SHLVL の値
# %M               マシンのフルホスト名
# %m               ホスト名の最初の `.' までの部分
# %S (%s)          突出モードの開始 (終了)
# %U (%u)          下線モードの開始 (終了)
# %B (%b)          太字モードの開始 (終了)
# %t or %@         12 時間制, am/pm 形式での現在時刻
# %n or $USERNAME  ユーザー ($USERNAME は環境変数なので setopt prompt_subst が必要)
# %N               シェル名
# %i               %N によって与えられるスクリプト, ソース, シェル関数で, 現在実行されている行の番号 (debug用)
# %T               24 時間制での現在時刻
# %*               24 時間制での現在時刻, 秒付き
# %w               `曜日-日' の形式での日付
# %W               `月/日/年' の形式での日付
# %D               `年-月-日' の形式での日付
# %D{string}       strftime 関数を用いて整形された文字列 (man 3 strftime でフォーマット指定が分かる)
# %l               ログインしている端末から, /dev/ プレフィックスを取り除いたもの
# %y               ログインしている端末から, /dev/ プレフィックスを取り除いたもの (/dev/tty* はそのまま)
# %?               プロンプトの直前に実行されたコマンドのリターンコード
# %_               パーサの状態
# %E               行末までクリア
# %#               特権付きでシェルが実行されているならば `#', そうでないならば `%' == %(!.#.%%)
# %v               psvar 配列パラメータの最初の要素の値
# %{...%}          リテラルのエスケープシーケンスとして文字列をインクルード
# %c, %., %C       $PWD の後ろ側の構成要素
#
# %(x.true-text.false-text)
#     三つ組の式
#
# ${WINDOW:+"[$WINDOW]"}
#     screen 実行時にスクリーン番号を表示 (prompt_subst が必要)
#
# %<string<, %>string>, %[xstring]
#     プロンプトの残りの部分に対する, 切り詰めの振る舞い
#     `<' の形式は文字列の左側を切り詰め, `>' の形式は文字列の右側を切り詰めます
#
# =============================================================================

# ----------------------------------------------------------
# default PROMPT
# %F{color} フォーマットについて
# > http://zsh.sourceforge.net/Doc/Release/Prompt-Expansion.html#Visual-effects
PROMPT="
%F{green}%n@%m %F{cyan}%f%~%F{gray} ---  %D{%m}.%D{%d} $(LANG=C date +'%a') %D{%T}$(LANG=ja_JP.UTF-8)%f
%(!.#.$) "

# ----------------------------------------------------------
# Git PROMPT
# http://qiita.com/items/7180eb6c788233280502

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

# 下のformatsの値をそれぞれの変数に入れてくれる機能の、変数の数の最大。
# デフォルトだと 2 くらい
# 指定しておかないと、下のformatsがほぼ動かない。
zstyle ':vcs_info:*' max-exports 10

# 左から順番に vcs_info_msg_{n}_ という名前の変数に格納されるので、下で利用する
zstyle ':vcs_info:*' formats '%R' '%S' '%b' '%s'
# 状態が特殊な場合のformats
zstyle ':vcs_info:*' actionformats '%R' '%S' '%b|%a' '%s'

# zsh 4.3.10以上の場合は、check-for-changesという機能を使う。
autoload -Uz is-at-least
if is-at-least 4.3.10; then
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' formats '%R' '%S' '%b' '%s' '%c' '%u'
    zstyle ':vcs_info:*' actionformats '%R' '%S' '%b|%a' '%s' '%c' '%u'
fi

# zsh の PROMPT に渡す文字列は可読性があまり良くないため
# 文字列を組み立てるのは関数で行う
function echo_rprompt () {
    local repos branch st color

    STY= LANG=en_US.UTF-8 vcs_info
    if [[ -n "$vcs_info_msg_1_" ]]; then
        # -Dつけて、~とかに変換
        repos=`print -nD "$vcs_info_msg_0_"`

        if [[ -n "$vcs_info_msg_2_" ]]; then
            branch="$vcs_info_msg_2_"
        else
            branch=$(basename "`git symbolic-ref HEAD 2> /dev/null`")
        fi

        if [[ -n "$vcs_info_msg_4_" ]]; then # staged
            branch="%K{green}%F{white} $branch %f%k"
        elif [[ -n "$vcs_info_msg_5_" ]]; then # unstaged
            branch="%K{red}%F{white} $branch %f%k"
        else
            branch="%K{blue}%F{white} $branch %f%k"
        fi

        print -n "[%35<..<"
        print -n "%F{yellow}$vcs_info_msg_1_%f"
        print -n "%<<]"

        print -n "%30<..<"
        print -nD "%F{yellow}$repos%f"
        print -n " $branch"
        print -n "%<<"

    else
        print -nD "[%F{yellow}%60<..<%~%<<%f]"
    fi
}

# git stash count
function git_prompt_stash_count {
  local COUNT=$(git stash list 2>/dev/null | wc -l | tr -d ' ')
  if [ "$COUNT" -gt 0 ]; then
    echo " %K{red}%F{white} $COUNT %f%k"
  fi
}

setopt prompt_subst
RPROMPT='`echo_rprompt``git_prompt_stash_count`'




# ----------------------------------------------------------
# MySQL PROMPT

# \c コマンド何回実行したかを数えるカウンタ。
# \D 日時(フル Tue May 29 14:40:48 2012)
# \d 利用中のデフォルトデータベース
# \h 接続先ホスト(サーバ)
# \l デリミタ(区切り文字、デフォルトはセミコロン)(5.1.12で新規追加)
# \m 分
# \n 改行
# \O 月名(頭3文字 Jan, Feb, …)
# \o 月名(数字)
# \P am/pm
# \p TCP/IPポート番号、またはソケットファイル名
# \R 時(24時間制)
# \r 時(12時間制)
# \S セミコロン
# \s 秒
# \t タブ
# \U ユーザ名(ホスト名含む)
# \u ユーザ名
# \v MySQLサーバのバージョン
# \w 曜日名(頭3文字 Mon, Tue, …)
# \Y 年(4桁)
# \y 年(2桁)
# \_ 空白
# \  空白(スラッシュの後に空白文字)
# \' シングルクォーテーションマーク
# \" ダブルクォーテーションマーク
# \\ バックスラッシュ

# mysqlプロンプトのカラー
export MYSQL_PS1="\n$GRAY [MySQL] $GREEN\u$GRAY@$GREEN\h $CYAN\d $GRAY\v $GRAY --- \D $NORMAL\n> "


# =============================================================================
# title
# =============================================================================
# user@host: current/dir/Path を window title に表示
case "${TERM}" in
  kterm*|xterm*|rxvt*)
    precmd() {
        echo -ne "\e]2;${USER}@${HOST}: $(pwd)\a"
    }
    ;;
esac

# =============================================================================
# misc
# =============================================================================

# no beep sound when complete list displayed
setopt nolistbeep

# ここで設定した秒数以上の時間を必要とした処理のあとは統計情報を表示する
# 処理所用時間, CPU使用率 など
REPORTTIME=3


# ==============================================================================
# keybind
#
# `$ bindkey` コマンドでキーバインド一覧表示
# ^   -> Ctrl
# ^[  -> Meta
# auto-fu.zsh を使う場合は、一旦 off にしないと使えないキーバインドがある
# C-g で toggle auto-fu on/off
# ==============================================================================

# like emacs keybindings
bindkey -e

# Shift-Tab 補完候補を逆順する ("\e[Z"でも動作する)
bindkey "\e[Z" reverse-menu-complete

# undo, redo
bindkey "^[U" undo
bindkey "^[R" redo

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# 前方一致履歴検索
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# IGNOREEOF forces the user to type exit or logout, instead of just pressing ^D.
# ^D で logout させない(ミスタイプによる意図しないログアウト防止).
# ただし10回連続で ^D すると logout.
# auto-fu.zsh を利用する場合 IGNOREEOF の設定が無視される
bindkey -r "^D"
setopt ignoreeof

bindkey '^R' history-incremental-search-backward

# ^S で stop しない
# ^S は ^R で command history から incremental search する機能の forward
# として使いたい
stty stop undef

function git-checkout-peco() {
  local selected_branch_name="$(git branch -a | peco | tr -d ' ')"
  case "$selected_branch_name" in
    *-\>* )
      selected_branch_name="$(echo ${selected_branch_name} | perl -ne 's/^.*->(.*?)\/(.*)$/\2/;print')";;
    remotes* )
      selected_branch_name="$(echo ${selected_branch_name} | perl -ne 's/^.*?remotes\/(.*?)\/(.*)$/\2/;print')";;
  esac
  if [ -n "$selected_branch_name" ]; then
    BUFFER="git checkout ${selected_branch_name}"
    zle accept-line
  fi
  zle clear-screen
}

function select-history-peco() {
  local tac
  ((($+commands[gtac])) && tac="gtac") || \
    (($+commands[tac])) && tac="tac" || \
    tac="tail -r"
  BUFFER=$(fc -l -n 1 | eval $tac | peco --query "$LBUFFER")
  CURSOR=$#BUFFER
  zle -R -c
}

function ghq-src-peco () {
  local selected=$(ghq list -p | peco --query "$LBUFFER")
  if [ -n "$selected" ]; then
    BUFFER="cd ${selected}"
    zle accept-line
  fi
  zle clear-screen
}

function ghq-src-fzf() {
    local selected
    selected="$(ghq list | fzf-tmux --query="$LBUFFER")"
    if [ -n "$(ghq root)/${selected}" ]; then
        BUFFER="cd $(ghq root)/${selected}"
        zle accept-line
    fi
    zle reset-prompt
}
zle -N select-history-peco
bindkey '^r' select-history-peco
zle -N ghq-src-peco
bindkey '^xgg' ghq-src-peco
zle -N git-checkout-peco
bindkey '^xgb' git-checkout-peco

# ==============================================================================
# functions
# ==============================================================================
load_if_exists () {
    if [ -e $1 ]; then
        source $1
    fi
}

# ==============================================================================
# shell common settings
# ==============================================================================
load_if_exists "${HOME}"/.sh/env.sh
load_if_exists ${HOME}/.sh/aliases.sh

compdef g=git # git の alias を使う場合にも git のサブコマンドの補完を有効にする

# -----------------------------------------------
# awscli complement
# -----------------------------------------------
if [ -e ${HOME}/.pyenv/shims/aws_zsh_completer.sh ]; then
  source ${HOME}/.pyenv/versions/`pyenv global`/bin/aws_zsh_completer.sh
fi

# -----------------------------------------------
# direnv
# -----------------------------------------------
if type direnv > /dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi

# -----------------------------------------------
# iterm2
# https://www.iterm2.com/documentation-shell-integration.html
# -----------------------------------------------
test -e "${HOME}/.iterm2_shell_integration.zsh" && source "${HOME}/.iterm2_shell_integration.zsh"

[ -f "${HOME}/.zsh/fzf.zsh" ] && source "${HOME}/.zsh/fzf.zsh"
