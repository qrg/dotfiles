function fish_user_key_bindings
  #bind \cr peco_select_history
  bind \cr _fzf_search_history

  #bind \cg\cg peco_select_ghq_repository
  bind \cg\cg __ghq_repository_search

  #bind \cgb peco_select_git_branch
  bind \cgb _fzf_git_branch

  #bind \cgf peco_select_git_file
  #bind \cgs peco_select_git_file_with_status
  bind \cgl _fzf_search_git_log
  bind \cgs _fzf_search_git_status
  #bind \cgt peco_select_git_tag
  #bind \cgh peco_select_git_hash

  #bind \cx\ck peco_kill
  bind \cx\ck _fzf_kill

  bind \cxp _fzf_search_processes

  bind \cxf _fzf_search_directory

  bind \eq push_line

  #bind \cx\cx peco_select_z
  bind \cx\cx _fzf_select_z
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

function _fzf_select_z
  set -l query (commandline)
  if test -n $query
    set fzf_flags --query "$query"
  end

  z -l | sort -rn | cut -c 12- | fzf $fzf_flags | read line
  if test $line
    cd $line
    commandline -f repaint
  end
end

function _fzf_kill
    set -l signal ''
    test -z $argv[1]
    or set signal $argv[1]

    set -l uid (id -u)
    set -l pid
    if test $uid -ne 0
        set pid (ps -f -u $uid | sed 1d | fzf -m | awk '{ print $2; }')
    else
        set pid (ps -ef | sed 1d | fzf -m | awk '{ print $2; }')
    end

    test -z $pid
    or echo $pid | xargs kill $signal
end

function _fzf_git_branch -d 'fzf: checkout git branch, sorted by most recent commit, 30 latest branches'
    set -l branches (git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format='%(refname:short)')
    and set -l branch (string replace -a ' ' '\n' $branches | fzf-tmux -d (math 2 + (count $branches)) +m)
    and git checkout (echo $branch | sed -e 's/.* //' -e '#remotes/[^/]*/##')
end
