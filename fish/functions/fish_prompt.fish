function fish_prompt
  # Cache exit status
  set -l last_status $status

  # Just calculate these once, to save a few cycles when displaying the prompt
  if not set -q __fish_prompt_hostname
    set -g __fish_prompt_hostname (hostname|cut -d . -f 1)
  end
  if not set -q __fish_prompt_char
    switch (id -u)
      case 0
        set -g __fish_prompt_char \u276f\u276f
      case '*'
        set -g __fish_prompt_char »
    end
  end

  # Setup colors
  set -l normal (set_color normal)
  set -l cyan (set_color cyan)
  set -l green (set_color green)
  set -l blue (set_color blue)
  set -l yellow (set_color yellow)
  set -l white (set_color white)
  set -l black (set_color black)
  set -l bpurple (set_color -o purple)
  set -l bgreen (set_color -o green)
  set -l bred (set_color -o red)
  set -l bcyan (set_color -o cyan)
  set -l bblue (set_color -o blue)
  set -l bwhite (set_color -o white)
  set -l bblack (set_color -o black)
  set -l muted (set_color 426773)
  set -l muted2 (set_color 648995)

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

  # Color prompt char red for non-zero exit status
  set -l pcolor $bpurple
  if [ $last_status -ne 0 ]
    set pcolor $bred
  end

  # Top
  echo
  echo $muted(date +'%H:%M:%S %m.%d %a') - $USER.$__fish_prompt_hostname$normal(__fish_git_prompt)
  echo -n $muted2(prompt_pwd)$normal

  echo

  # Bottom
  echo -n $pcolor$__fish_prompt_char $normal
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
