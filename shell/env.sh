# Base environment variable
# =============================================================================
export XDG_CONFIG_HOME="${HOME}/.config"
export XDG_CACHE_HOME="${HOME}/.cache"
export XDG_DATA_HOME="${HOME}/.local/share"
export XDG_STATE_HOME="${HOME}/.local/state"

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8
export LC_COLLATE=ja_JP.UTF-8  # 正規表現のマッチング (範囲表現と等価クラスのマッチングを決定する) と文字列の照合 (collation)
export LC_CTYPE=ja_JP.UTF-8    # 正規表現のマッチング、文字の分類、文字の変換、大文字小文字比較、 ワイド文字関数
export LC_MESSAGES=ja_JP.UTF-8 # 地域化可能な自然言語メッセージ
export LC_MONETARY=ja_JP.UTF-8 # 通貨の書式
export LC_NUMERIC=ja_JP.UTF-8  # 数値の書式 (小数点や 3 桁ごとの区切り)
export LC_TIME=en_US.UTF-8     # 時刻と日付けの書式

# PATH
# -----------------------------------------------------------------------------
local _path="${HOME}/workspace/scripts:${HOME}/.local/bin:/usr/local/bin:/usr/local/sbin"

case "${OSTYPE}" in
  # macOS
  darwin*)
    if [ -s /opt/homebrew/bin/brew ]; then
      eval "$(/opt/homebrew/bin/brew shellenv)"
    fi
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications --fontdir=${HOME}/Library/Fonts"
    # https://formulae.brew.sh/formula/gnutls
    export GUILE_TLS_CERTIFICATE_DIRECTORY=/usr/local/etc/gnutls/
  ;;
esac

# rbenv
if [ -d ${XDG_DATA_HOME}/rbenv ]; then
  export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
  _path="${RBENV_ROOT}/bin:${_path}"
  eval "$(rbenv init -)"
fi

# nodenv
if [ -s ${XDG_DATA_HOME}/nodenv ]; then
  export NODENV_ROOT="${XDG_DATA_HOME}/nodenv"
  _path="${NODENV_ROOT}/bin:${_path}"

  if type brew > /dev/null 2>&1; then
    if [ -e $(brew --prefix node-build-update-defs) ]; then
      export NODE_BUILD_DEFINITIONS=$(brew --prefix node-build-update-defs)/share/node-build
    fi
  fi

  eval "$(nodenv init -)"
fi

# direnv
if type direnv > /dev/null 2>&1; then
  if [ -n "$ZSH_VERSION" ]; then
    eval "$(direnv hook zsh)"
  elif [ -n "$BASH_VERSION" ]; then
    eval "$(direnv hook bash)"
  fi
fi

# volta
if [ -s ${XDG_DATA_HOME}/volta ]; then
  export VOLTA_HOME="${XDG_DATA_HOME}/volta"
  _path="${VOLTA_HOME}/bin:${_path}"
fi

# pnpm
if [ -s ${XDG_DATA_HOME}/pnpm ]; then
  export PNPM_HOME="${XDG_DATA_HOME}/pnpm"
  _path="${PNPM_HOME}:${_path}"
fi

# golang
if [ -s ${XDG_DATA_HOME}/go ]; then
  export GOPATH="${XDG_DATA_HOME}/go"
  _path="${GOPATH}/bin:${_path}"
fi

# rust
if [ -s ${XDG_DATA_HOME}/cargo ]; then
  export CARGO_HOME="${XDG_DATA_HOME}/cargo"
  _path="${CARGO_HOME}/bin:${_path}"
  source "${CARGO_HOME}/env"
fi

case ":$PATH:" in
  *":$_path:"*)
    export PATH="${PATH}"
    ;;
  *)
    export PATH="${_path}:${PATH}"
    ;;
esac

uniq_path(){
  local _paths=($(echo $PATH | tr ':' ' '))
  local _from
  local _to
  local _uniq_paths

  if [ -n "$ZSH_VERSION" ]; then
    _from=${#_paths[@]}
    _to=1
  elif [ -n "$BASH_VERSION" ]; then
    _from=${#_paths[@]}-1
    _to=0
  fi

  for (( idx=$_from ; idx>=$_to ; idx-- )) ; do
    if [[ ! " ${_uniq_paths[*]} " =~ " ${_paths[idx]} " ]]; then
      _uniq_paths=(${_paths[idx]} ${_uniq_paths[*]})
    fi
  done
  echo $(echo "${_uniq_paths[*]}" | tr " " ":")
}

# Environment variables
# -----------------------------------------------------------------------------
export PATH=$(uniq_path)

export EDITOR=nvim
export GPG_TTY=$(tty)
export GNUPGHOME="${XDG_DATA_HOME}/gnupg"

export MANWIDTH=100
export LESSCHARSET=utf-8
export LESSHISTFILE="${XDG_DATA_HOME}/less/history"

export GIBO_BOILERPLATES="${XDG_DATA_HOME}/gibo-boilerplates"
export FISH_SHELL_PATH=`which fish`

# man pages
# -----------------------------------------------------------------------------
if type source-highlight > /dev/null 2>&1; then
  export LESS='-R'
  export LESSOPEN='| src-hilite-lesspipe.sh %s'
fi

export MANPAGER='less -R'
export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
export LESS_TERMCAP_md=$(tput bold; tput setaf 6) # cyan
export LESS_TERMCAP_me=$(tput sgr0)
export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
export LESS_TERMCAP_so=$(tput bold; tput setaf 3; tput setab 4) # yellow on blue
export LESS_TERMCAP_ue=$(tput rmul; tput sgr0)
export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 7) # white
export LESS_TERMCAP_mr=$(tput rev)
export LESS_TERMCAP_mh=$(tput dim)
export LESS_TERMCAP_ZN=$(tput ssubm)
export LESS_TERMCAP_ZV=$(tput rsubm)
export LESS_TERMCAP_ZO=$(tput ssupm)
export LESS_TERMCAP_ZW=$(tput rsupm)
