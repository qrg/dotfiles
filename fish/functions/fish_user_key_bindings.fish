function fish_user_key_bindings
  #bind \cr peco_select_history
  bind \cr _fzf_search_history

  bind \cg\cg peco_select_ghq_repository
  bind \cgb peco_select_git_branch
  bind \cgf peco_select_git_file
  bind \cgs peco_select_git_file_with_status
  bind \cgl _fzf_search_git_log
  bind \cgs _fzf_search_git_status
  bind \cgt peco_select_git_tag
  bind \cgh peco_select_git_hash

  bind \cx\ck peco_kill
  bind \cxp _fzf_search_processes

  bind \cxf _fzf_search_directory

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
