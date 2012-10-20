# .zshrc
# cf.) man zshoptions

# -----------------------------------------------------------------------------
# path
# -----------------------------------------------------------------------------
ZDOTDIR=~/.zsh.d
ZCOMPFILE=${ZDOTDIR}/zcompdump.${HOST}.${USER}
HISTFILE=${ZDOTDIR}/zsh_history

COMMONFILE=~/.sh.d/common.sh

ZshPluginDir=${ZDOTDIR}/plugin

AutoFuFile=${ZshPluginDir}/auto-fu.zsh/auto-fu.zsh
ZshCompetionsDir=${ZshPluginDir}/zsh-completions
GitCompletionFile=${ZshPluginDir}/git-completion.bash
GitPromptFile=${ZshPluginDir}/git-prompt.sh

# -----------------------------------------------------------------------------
# completion
# cf.) man zshcompsys
# -----------------------------------------------------------------------------

# incr-0.2.zsh の後継がauto-fu.zsh
# incr-0.2.zsh
# if [ -f ${ZDOTDIR}/plugin/incr-0.2.zsh ]; then
#     source ${ZDOTDIR}/plugin/incr-0.2.zsh
# fi

# auto-fu.zsh
if [ -f ${AutoFuFile} ]; then
    source ${AutoFuFile}
    function zle-line-init (){ auto-fu-init }
    zle -N zle-line-init
    zstyle ':completion:*' completer _oldlist _complete _history _expand _prefix
    zstyle ':auto-fu:var' postdisplay ''
fi

# additional zsh-completions
fpath=(${ZshCompletionsDir} $fpath)
zstyle ':completion:*:sudo:*' command-path /usr/local/sbin /usr/local/bin \
                              /usr/sbin /usr/bin /sbin /bin /usr/X11R6/bin \
                              /usr/local/git/bin

# ファイル補完候補に色を付ける
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# 補完の時に大文字小文字を区別しない(ただし大文字を打った場合は小文字に変換しない)
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'


# zsh basic completion
autoload -U compinit
compinit -u -d ${ZCOMPFILE}

# git-completion
autoload bashcompinit
bashcompinit
if [ -f ${GitCompletionFile} ]; then
    source ${GitCompletionFile}
fi

# auto change directory
setopt auto_cd

# = 以降でも補完できるようにする(--prefix=/usr 等の場合)
setopt magic_equal_subst

# auto directory pushd that you can get dirs list by cd -[tab]
setopt auto_pushd

# command correct edition before each completion attempt
setopt correct

# compacked complete list display
setopt list_packed

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
bindkey '^xp' predict-on
bindkey '^x^p' predict-off

# -----------------------------------------------------------------------------
# history
# -----------------------------------------------------------------------------
HISTSIZE=10000000
SAVEHIST=10000000

# ignore duplication command history list
setopt hist_ignore_dups

# share command history data
setopt share_history

# add timestamp with history
setopt extended_history

# -----------------------------------------------------------------------------
# prompt
# -----------------------------------------------------------------------------

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

# load plugin that shows git branch at PROMPT
if [ -f ${GitPromptFile} ]; then
    source ${GitPromptFile}
fi

PROMPT="
%F{green}%n@%m %F{cyan}%f%~%F{gray} ---  %D{%m}.%D{%d} $(LANG=C date +'%a') %D{%T}$(LANG=ja_JP.UTF-8)%f
%(!.#.$) "

# update time
TRAPALRM () { zle reset-prompt }
TMOUT=5


# Git PROMPT ------------------------------------------------------------------
# http://qiita.com/items/7180eb6c788233280502

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git

# 下のformatsの値をそれぞれの変数に入れてくれる機能の、変数の数の最大。
# デフォルトだと2くらいなので、指定しておかないと、下のformatsがほぼ動かない。
zstyle ':vcs_info:*' max-exports 10

# 左から順番に、vcs_info_msg_{n}_ という名前の変数に格納されるので、下で利用する
zstyle ':vcs_info:*' formats '%R' '%S' '%b' '%s'
# 状態が特殊な場合のformats
zstyle ':vcs_info:*' actionformats '%R' '%S' '%b|%a' '%s'

# 4.3.10以上の場合は、check-for-changesという機能を使う。
autoload -Uz is-at-least
if is-at-least 4.3.10; then
    zstyle ':vcs_info:*' check-for-changes true
    zstyle ':vcs_info:*' formats '%R' '%S' '%b' '%s' '%c' '%u'
    zstyle ':vcs_info:*' actionformats '%R' '%S' '%b|%a' '%s' '%c' '%u'
fi

# zshのPTOMPTに渡す文字列は、可読性がそんなに良くなくて、読み書きしたり、つまりデバッグが
# 大変なため、文字列を組み立てるのは関数でやることにする。
# そのほうが分岐などを追加するのが楽。
# この先、追加で表示させたい情報はいろいろでてくるとおもうし。
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

# MySQL PROMPT ----------------------------------------------------------------

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

# export MYSQL_PS1="
#[MySQL]$GREEN\U $NORMAL[$CYAN\d$NORMAL]$GRAY --- $(LANG=C date '+%m.%d %A') %*$(LANG=ja_JP.UTF-8)$NORMAL
#>"

# mysqlプロンプトのカラー
export MYSQL_PS1="\n$GRAY [MySQL] $GREEN\u$GRAY@$GREEN\h $CYAN\d $GRAY\v $GRAY --- \D $NORMAL\n> "


# -----------------------------------------------------------------------------
# title
# -----------------------------------------------------------------------------
case "${TERM}" in
kterm*|xterm)
    precmd() {
        echo -ne "\033]0;${USER}@${HOST}\007"
    }
    ;;
esac


# ------------------------------------------------------------------------------
# misc
# ------------------------------------------------------------------------------

# no beep sound when complete list displayed
setopt nolistbeep

# show time taken to finish off commands
# when it takes more than value number (sec)
REPORTTIME=1

# ------------------------------------------------------------------------------
# keybind
# ------------------------------------------------------------------------------

# like emacs
bindkey -e

autoload history-search-end
zle -N history-beginning-search-backward-end history-search-end
zle -N history-beginning-search-forward-end history-search-end

# 前方一致履歴検索
bindkey "^P" history-beginning-search-backward-end
bindkey "^N" history-beginning-search-forward-end

# IGNOREEOF forces the user to type exit or logout, instead of just pressing ^D.
# ^D でログアウトさせない(ミスタイプによる意図しないログアウト防止). ただし10回連続で ^D すると ログアウト.
setopt IGNOREEOF

# ^S で stop しない
stty stop undef

# ------------------------------------------------------------------------------
# common shell settings
# ------------------------------------------------------------------------------
# common settings with bash and zsh
if [ -f ${COMMONFILE} ]; then
    source ${COMMONFILE}
fi


# ------------------------------------------------------------------------------
# functions
# ------------------------------------------------------------------------------
function showcolors() {
    for ((f = 0; f < 255; f++)); do
        printf "\e[38;5;%dm %3d#\e[m" $f $f
        if [[ $f%8 -eq 7 ]] then
            printf "\n"
        fi
    done
    echo
}

