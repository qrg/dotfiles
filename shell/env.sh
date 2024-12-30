# shellcheck shell=bash

function configure_shell_env() {
  local _os
  _os=$(uname)

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

  if [ "${_os}" = 'Darwin' ] && [ -x /opt/homebrew/bin/brew ]; then
    _path="/opt/homebrew/bin/:${_path}"
    # add executable pathes that was installed with Homebrew
    eval "$(/opt/homebrew/bin/brew shellenv)"
    export HOMEBREW_CASK_OPTS="--appdir=${HOME}/Applications --fontdir=${HOME}/Library/Fonts"
    export GUILE_TLS_CERTIFICATE_DIRECTORY=/usr/local/etc/gnutls/ # https://formulae.brew.sh/formula/gnutls
    export LIBRARY_PATH="/opt/homebrew/lib:$LIBRARY_PATH"
    export CPATH="/opt/homebrew/include:$CPATH"
  fi

  # mise
  # https://mise.jdx.dev/
  if [ -f "${XDG_CONFIG_HOME}/mise/config.toml" ]; then
    if [ -n "$ZSH_VERSION" ]; then
      eval "$(mise activate zsh)"
    elif [ -n "$BASH_VERSION" ]; then
      eval "$(mise activate bash)"
    fi
  fi

  # nodenv
  if [ -d "${XDG_DATA_HOME}"/nodenv ]; then
    export NODENV_ROOT="${XDG_DATA_HOME}/nodenv"
    _path="${NODENV_ROOT}/bin:${_path}"

    if type brew > /dev/null 2>&1; then
      if [ -e "$(brew --prefix node-build-update-defs)" ]; then
        NODE_BUILD_DEFINITIONS=$(brew --prefix node-build-update-defs)/share/node-build
        export NODE_BUILD_DEFINITIONS
      fi
    fi

    eval "$(nodenv init -)"
  fi

  # rbenv
  if [ -d "${XDG_DATA_HOME}"/rbenv ]; then
    export RBENV_ROOT="${XDG_DATA_HOME}/rbenv"
    _path="${RBENV_ROOT}/bin:${_path}"
    eval "$(rbenv init -)"
  fi

  # pnpm
  if [ -d "${XDG_DATA_HOME}"/pnpm ]; then
    export PNPM_HOME="${XDG_DATA_HOME}/pnpm"
    _path="${PNPM_HOME}:${_path}"
  fi

  # golang
  if [ -d "${XDG_DATA_HOME}"/go ]; then
    export GOPATH="${XDG_DATA_HOME}/go"
    _path="${GOPATH}/bin:${_path}"
  fi

  # rust
  if [ -d "${XDG_DATA_HOME}"/cargo ]; then
    export CARGO_HOME="${XDG_DATA_HOME}/cargo"
    export RUSTUP_HOME="${XDG_DATA_HOME}/rustup"
    _path="${CARGO_HOME}/bin:${_path}"
  fi

  # fzf
  if [ -n "$ZSH_VERSION" ] && [ -f ~/.fzf.zsh ]; then
    # shellcheck source=/home/yourusername/.fzf.zsh
    source ~/.fzf.zsh
  elif [ -n "$BASH_VERSION" ] && [ -f ~/.fzf.bash ]; then
    # shellcheck source=/home/yourusername/.fzf.bash
    source ~/.fzf.bash
  fi

  # Visual Studio Code on WSL
  if [ -n "$WSLENV" ]; then
    _path=${_path}:"/mnt/c/Users/${USER}/AppData/Local/Programs/Microsoft VS Code/bin"
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
    local _paths=($(echo "$PATH" | tr ':' ' '))
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
  export FISH_SHELL_PATH=$(which fish)

  export NPM_CONFIG_USERCONFIG=${XDG_CONFIG_HOME}/npm/config

  # zoxide
  # https://github.com/ajeetdsouza/zoxide
  if [ -x "$(command -v zoxide)" ]; then
    export _ZO_DATA_DIR=$XDG_DATA_HOME

    if [ -n "$ZSH_VERSION" ]; then
      eval "$(zoxide init zsh)"
    fi
    if [ -n "$BASH_VERSION" ]; then
      eval "$(zoxide init bash)"
    fi
  fi


  # Man pages
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

  # Initialize
  # -----------------------------------------------------------------------------

  # tabtab source for packages
  # uninstall by removing these lines
  if [ -f "${XDG_CONFIG_HOME}"/tabtab/zsh/__tabtab.zsh ] && [ -n "$ZSH_VERSION" ]; then
    source "${XDG_CONFIG_HOME}"/tabtab/zsh/__tabtab.zsh
  fi

  if [ -f "${XDG_CONFIG_HOME}"/tabtab/bash/__tabtab.bash ] && [ -n "$BASH_VERSION" ]; then
    source "${XDG_CONFIG_HOME}"/tabtab/bash/__tabtab.bash
  fi

  # WSL
  if [ -n "$WSLENV" ]; then

    # https://github.com/microsoft/WSL/issues/4401
    function isWinDir {
      case $PWD/ in
        /mnt/*) return $(true);;
        *) return $(false);;
      esac
    }
    function git {
      if isWinDir
      then
        git.exe "$@"
      else
        /usr/bin/git "$@"
      fi
    }

    # open
    export BROWSER="powershell.exe /c start"
    function open {
      powershell.exe /c start "$1"
    }

  fi
}

configure_shell_env
