function fish_user_key_bindings
  bind \cr peco_select_history
  bind \cg peco_select_ghq_repository

  bind \cx\ck peco_kill
  bind \cx\cb peco_select_git_branch
  bind \cx\cf peco_select_git_file
  bind \cx\cs peco_select_git_file_with_status
  bind \cx\ct peco_select_git_tag
  bind \cx\ch peco_select_git_hash

  bind \eq push_line

  bind \cx\cx peco_select_z
end

function peco_select_z
  set -l query (commandline)
  if test -n $query
    set peco_flags --query "$query"
  end

  z -l | sort -rn | cut -c 12- | peco $peco_flags | read line
  if test $line
    cd $line
    commandline -f repaint
  end
end
