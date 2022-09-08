function fish_user_key_bindings
  #bind \cr peco_select_history
  bind \cr _fzf_search_history

  #bind \cg\cg peco_select_ghq_repository
  bind \cg\cg __ghq_repository_search

  #bind \cgb peco_select_git_branch
  bind \cgb _fzf_git_branch

  #bind \cgf peco_select_git_file
  #bind \cgs peco_select_git_file_with_status
  #bind \cgl _fzf_search_git_log
  bind \cgs _fzf_search_git_status
  bind \cgl _fzf_select_git_log
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

  z -l | sort -rn | cut -c 12- | fzf-tmux $fzf_flags | read line
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
        set pid (ps -f -u $uid | sed 1d | fzf-tmux -m | awk '{ print $2; }')
    else
        set pid (ps -ef | sed 1d | fzf-tmux -m | awk '{ print $2; }')
    end

    test -z $pid
    or echo $pid | xargs kill $signal
end

function _fzf_git_branch -d 'fzf: checkout git branch, sorted by most recent commit, 30 latest branches'
    set -l branches (git for-each-ref --count=30 --sort=-committerdate refs/heads/ --format='%(refname:short)')
    and set -l branch (string replace -a ' ' '\n' $branches | fzf-tmux -d (math 2 + (count $branches)) +m)
    and git checkout (echo $branch | sed -e 's/.* //' -e '#remotes/[^/]*/##')
end

function _fzf_is_in_git_repo
  command -s -q git
    and git rev-parse HEAD >/dev/null 2>&1
end

function _fzf-down
  fzf-tmux --min-height 20 --border --bind ctrl-p:toggle-preview --bind ctrl-a:select-all $argv
end

function _fzf_add_multi_hashes_to_commandline -d 'add multiple hashes as a range'
  read -d \n -z -a result
  set -l hashes
  for hash in $result
    if test -n (string trim $hash)
      set -a hashes $hash
    end
  end
  commandline -t ""
  if test (count $hashes) -gt 1
    commandline -it -- $hashes[1]".. "$hashes[-1]
  else
    commandline -it -- $hashes[1]
  end
  commandline -f repaint
end

function _copy_multi_git_logs_as_markdown -d 'copy multiple git log messages to clipboard'
  set -l hashes

  for hash in $argv
    if test -n (string trim $hash)
      set --append hashes $hash
    end
  end

  set -l reversed_hashes $hashes[-1..1]

  set -l messages
  for hash in $reversed_hashes
    set -l hash_full (git log --pretty=%H --max-count=1 $hash)
    set -l url (string trim (gh browse $hash_full --no-browser))
    #set -l items (string split "\n\n\n" (string trim (git log --format=%B --max-count=1 $hash_full)))
    set -l items (\
      git log --format=%B --max-count=1 $hash_full \
      | string replace --regex '^[\n\t\r ]*(.+)[\n\t\r ]*$' '$1' \
      | string join "\n" \
      | string split "\n\n" \
    )
    set -l title $items[1]
    set -l texts $items[2..-1]
    set -l msg
    set --append msg "- $url"
    set --append msg "  - $title"
    for text in $texts
      set --append msg "    - "(\
        echo -n $text \
        | string replace --regex '^[\n\t\r ]*(.+)[\n\t\r ]*$' '$1' \
        | string replace --all "\n" "\n      " \
      )
    end
    set --append messages $msg
  end
  echo -e (printf "%s\n" $messages | string join "\n") | clipboard-copy.sh
end

function _fzf_select_git_log --description "Select the output of git log and preview commits. Replace the current token with the selected commit hash."
  if not type -q gh
    echo 'Missing dependencies GitHub cli/cli.'
    echo 'https://cli.github.com/'
    return 1
  end
  if not type -q clipboard-copy.sh
    echo 'Missing dependencies clipboard-copy.sh'
    return 1
  end

  if not git rev-parse --git-dir >/dev/null 2>&1
    echo '_fzf_select_git_log: Not in a git repository.' >&2
  else
    # see documentation for git format placeholders at https://git-scm.com/docs/git-log#Documentation/git-log.txt-emnem
    # %h gives you the abbreviated commit hash, which is useful for saving screen space, but we will have to expand it later below
    set log_fmt_str '%C(bold blue)%h%C(reset) - %C(cyan)%ad%C(reset) %C(yellow)%d%C(reset) %C(normal)%s%C(reset)  %C(dim normal)[%an]%C(reset)'
    set selected_log_lines (
      git log --color=always --format=format:$log_fmt_str --date=short | \
      fzf-tmux --ansi \
        --multi \
        --reverse \
        --tiebreak=index \
        --preview='git show --color=always --stat --patch {1}' \
        --query=(commandline --current-token) \
        $fzf_git_log_opts
    )
    if test $status -eq 0
      for line in $selected_log_lines
        set abbreviated_commit_hash (string split --field 1 " " $line)
        set full_commit_hash (git rev-parse $abbreviated_commit_hash)
        set --append commit_hashes $full_commit_hash
      end
      commandline --current-token --replace (string join ' ' $commit_hashes)
    end
  end
  commandline --replace ''
  commandline --function repaint
  _copy_multi_git_logs_as_markdown $commit_hashes
end
