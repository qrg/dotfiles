function fish_prompt
  # Cache exit status
  set -l last_status $status


  # Setup colors
  set -l base1_color '426773'
  set -l base2_color '648995'
  set -l base3_color 97acb3

  if not set -q __prompt_char
    switch (id -u)
      case 0
        set -g __prompt_char \u276f\u276f
      case '*'
        set -g __prompt_char »
    end
  end

  # Adds a badge if we're in an SSH session (first letter of hostname, uppercased)
  function __ssh_badge
    if test -n "$SSH_CLIENT$SSH2_CLIENT$SSH_TTY"
      set_color -b 1f363c -o 475d63
      echo -n " "(string upper (string sub -s 1 -l 1 (hostname -s)))" "
      set_color normal
      echo -n " "
    end
    echo -n ''
  end

  # only display a host name if we're in an ssh session
  function __ssh_host
    if test -n "$SSH_CLIENT$SSH2_CLIENT$SSH_TTY"
      set_color -d white
      echo -n $USER@
      set_color normal
      set_color -d -o brmagenta
      echo -n (hostname -s)
      set_color normal
    end
    echo -n ''
  end

  function __user_host
    if test (id -u) -eq 0
      set_color --bold red
    else
      set_color $base1_color
    end
    echo -n $USER "at "

    if test -n "$SSH_CLIENT$SSH2_CLIENT$SSH_TTY"
      set_color -d -o white
      echo -n (hostname -s)
      set_color normal
    else
      echo -n (hostname -s)
      set_color normal
    end
    echo -n ''
  end

  function __current_path
    # Replace HOME with ~
    set -l path (string replace "$HOME" "~" (pwd))
    # Highlight last path element
    set -l parts (string split "/" $path)
    set parts[-1] (set_color --bold blue)$parts[-1](set_color normal)
    set path (string join "/" $parts)
    echo $path
  end

  # Configure __fish_git_prompt
  set -g __fish_git_prompt_show_informative_status true
  set -g __fish_git_prompt_showcolorhints false
  set -g __fish_git_prompt_showdirtystate true
  set -g __fish_git_prompt_showstashstate true
  set -g __fish_git_prompt_showupstream 'auto'
  set -g __fish_git_prompt_showuntrackedfiles true
  set -g __fish_git_prompt_describe_style 'branch'

  # https://github.com/fish-shell/fish-shell/blob/e93e85f3ce97c4f0256020b5cef4c0df2d349735/share/functions/fish_git_prompt.fish#L534-L550
  __fish_git_prompt_set_char __fish_git_prompt_char_dirtystate '*' '*'
  __fish_git_prompt_set_char __fish_git_prompt_char_invalidstate '#' '✖'
  __fish_git_prompt_set_char __fish_git_prompt_char_stagedstate '+' '●'
  __fish_git_prompt_set_char __fish_git_prompt_char_stashstate '$' '⚑'
  __fish_git_prompt_set_char __fish_git_prompt_char_stateseparator ' ' ' '
  __fish_git_prompt_set_char __fish_git_prompt_char_untrackedfiles '%' '…'
  __fish_git_prompt_set_char __fish_git_prompt_char_upstream_ahead '>' '↑'
  __fish_git_prompt_set_char __fish_git_prompt_char_upstream_behind '<' '↓'
  __fish_git_prompt_set_char __fish_git_prompt_char_upstream_diverged '<>'
  __fish_git_prompt_set_char __fish_git_prompt_char_upstream_equal '='
  __fish_git_prompt_set_char __fish_git_prompt_char_upstream_prefix ' '

  # Top
  echo
  echo
  echo (set_color $base1_color)(date +'%Y-%m-%d %a %H:%M:%S')
  echo (__current_path) (set_color $base1_color)(__fish_git_prompt)
  echo

  # Bottom
  echo -n (set_color purple)$__prompt_char (set_color normal)
end

function fish_right_prompt
  #echo -n (date +'%m.%d %a %H:%M:%S')
end

# https://github.com/fish-shell/fish-shell/blob/e93e85f3ce97c4f0256020b5cef4c0df2d349735/share/functions/fish_git_prompt.fish#L514-L532
function __fish_git_prompt_set_char
  set -l user_variable_name "$argv[1]"
  set -l char $argv[2]

  if set -q argv[3]
    and begin
      set -q __fish_git_prompt_show_informative_status
      or set -q __fish_git_prompt_use_informative_chars
    end
    set char $argv[3]
  end

  set -l variable _$user_variable_name
  set -l variable_done "$variable"_done

  if not set -q $variable
    set -g $variable (set -q $user_variable_name; and echo $$user_variable_name; or echo $char)
  end
end
