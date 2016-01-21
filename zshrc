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

# 補完除外対象
# zstyle ':completion:*:*files' ignored-patterns '*?.o' '*?~' '*\#'

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
# "Solve conflict of zsh zaw plugin"
# http://myfuturesightforpast.blogspot.jp/2014/07/solve-conflict-of-zsh-zaw-plugin.html
# zaw の読み込みは以下の点に注意
# * antigen を利用する場合、zshenv も含めた zaw 読み込み前に `alias ls="ls -F"` を書かない
# * auto-fu 併用の場合、zaw の設定を auto-fu の前に書く
# * `setopt sh_word_split` は書かない
if [[ -f ${HOME}/.zsh/plugins/antigen/antigen.zsh ]]; then
  source ${HOME}/.zsh/plugins/antigen/antigen.zsh

  # Load the oh-my-zsh's library.
  antigen use oh-my-zsh

  antigen bundle brew
  antigen bundle git
  antigen bundle osx
  antigen bundle command-not-found
  antigen bundle zsh-users/zaw
  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen bundle zsh-users/zsh-completions src
  # antigen bundle ollifier/anyframe

  antigen apply
fi


# ----------------------------------------------------------
# auto-fu.zsh
# C-g で toggle auto-fu on/off
#if [ -e ${AutoFuFile} ]; then
#    source ${AutoFuFile}
#    function zle-line-init (){ auto-fu-init }
#    zle -N zle-line-init
#    zstyle ':completion:*' completer _oldlist _complete _history _expand _prefix _match _approximate _list
#    zstyle ':auto-fu:var' postdisplay ''
#fi

# ----------------------------------------------------------
# z - jump around
# https://github.com/rupa/z
# z keyword で keyword に match する dir に移動
# cd history を _Z_DATA に自動保存
#_Z_CMD=j
#
#source ${ZjumpAroundFile}
#precmd() {
#    _z --add "$(pwd -P)"
#}
#_Z_DATA=${ZshLocalDir}/zjumpdata

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

#powerline-daemon -q
#source ${HOME}/.pyenv/versions/2.7.9/lib/python2.7/site-packages/powerline/bindings/zsh/powerline.zsh

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

# zshのPTOMPTに渡す文字列は可読性があまり良くないため
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

# 履歴から補完機能を on/off
#bindkey '^xp' predict-on
#bindkey '^x^p' predict-off

# すぐ上のディレクトリ移動にキーバインド
#function cdup() {
#    echo
#    cd ..
#    zle reset-prompt
#}
#zle -N cdup
# ctrl + up
#bindkey "^[[1;5A" cdup

# zaw
# auto-fu と併用する場合、^g を先に押す
bindkey '^r' zaw-history
bindkey '^b' zaw-bookmark
bindkey '^xgf' zaw-git-files
bindkey '^xgs' zaw-git-status
bindkey '^xgb' zaw-git-branches
bindkey '^x^x' zaw-cdr

# enable case-insensitive search
zstyle ':filter-select' case-insensitive yes

# anyframe
#bindkey '^xb' anyframe-widget-cdr
#bindkey '^x^b' anyframe-widget-checkout-git-branch
#bindkey '^xr' anyframe-widget-execute-history
#bindkey '^x^r' anyframe-widget-execute-history
#bindkey '^xi' anyframe-widget-put-history
#bindkey '^x^i' anyframe-widget-put-history
#bindkey '^xg' anyframe-widget-cd-ghq-repository
#bindkey '^x^g' anyframe-widget-cd-ghq-repository
#bindkey '^xk' anyframe-widget-kill
#bindkey '^x^k' anyframe-widget-kill
#bindkey '^xe' anyframe-widget-insert-git-branch
#bindkey '^x^e' anyframe-widget-insert-git-branch


# ==============================================================================
# functions
# ==============================================================================
# ターミナルアプリで利用できる256色を一覧表示する。256色正しく表示されるか確認用
# `$ showcolors` として使う
function showcolors() {
    for ((f = 0; f < 255; f++)); do
        printf "\e[38;5;%dm %3d#\e[m" $f $f
        if [[ $f%8 -eq 7 ]] then
            printf "\n"
        fi
    done
    echo
}


# Credits to npm's. Awesome completion utility.
#
# Bower completion script, based on npm completion script.

###-begin-bower-completion-###
#
# Installation: bower completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: bower completion > /usr/local/etc/bash_completion.d/npm
#

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if type complete &>/dev/null; then
  _bower_completion () {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           bower completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F _bower_completion bower
elif type compdef &>/dev/null; then
  _bower_completion() {
    si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 bower completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _bower_completion bower
elif type compctl &>/dev/null; then
  _bower_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       bower completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _bower_completion bower
fi
###-end-bower-completion-###

###-begin-npm-completion-###
#
# npm command completion script
#
# Installation: npm completion >> ~/.bashrc  (or ~/.zshrc)
# Or, maybe: npm completion > /usr/local/etc/bash_completion.d/npm
#

COMP_WORDBREAKS=${COMP_WORDBREAKS/=/}
COMP_WORDBREAKS=${COMP_WORDBREAKS/@/}
export COMP_WORDBREAKS

if type complete &>/dev/null; then
  _npm_completion () {
    local si="$IFS"
    IFS=$'\n' COMPREPLY=($(COMP_CWORD="$COMP_CWORD" \
                           COMP_LINE="$COMP_LINE" \
                           COMP_POINT="$COMP_POINT" \
                           npm completion -- "${COMP_WORDS[@]}" \
                           2>/dev/null)) || return $?
    IFS="$si"
  }
  complete -F _npm_completion npm
elif type compdef &>/dev/null; then
  _npm_completion() {
    si=$IFS
    compadd -- $(COMP_CWORD=$((CURRENT-1)) \
                 COMP_LINE=$BUFFER \
                 COMP_POINT=0 \
                 npm completion -- "${words[@]}" \
                 2>/dev/null)
    IFS=$si
  }
  compdef _npm_completion npm
elif type compctl &>/dev/null; then
  _npm_completion () {
    local cword line point words si
    read -Ac words
    read -cn cword
    let cword-=1
    read -l line
    read -ln point
    si="$IFS"
    IFS=$'\n' reply=($(COMP_CWORD="$cword" \
                       COMP_LINE="$line" \
                       COMP_POINT="$point" \
                       npm completion -- "${words[@]}" \
                       2>/dev/null)) || return $?
    IFS="$si"
  }
  compctl -K _npm_completion npm
fi
###-end-npm-completion-###

# ==============================================================================
# shell common settings
# ==============================================================================
[[ -s "${HOME}"/.sh/aliases.sh ]] && source ${HOME}/.sh/aliases.sh
source "${HOME}"/.sh/env.sh

# -----------------------------------------------
# awscli complement
# -----------------------------------------------
if [ -e "${HOME}"/.pyenv/shims/aws_zsh_completer.sh ]; then
    source "${HOME}"/.pyenv/versions/`pyenv global`/bin/aws_zsh_completer.sh
fi

# -----------------------------------------------
# # direnv
# -----------------------------------------------
if type direnv > /dev/null 2>&1; then
  eval "$(direnv hook zsh)"
fi
