#!/bin/sh

cd $(dirname $0)

ln -Fisv $PWD/.sh.d $HOME
ln -Fisv $PWD/.vim $HOME
ln -Fisv $PWD/.zsh.d $HOME
ln -Fisv $PWD/.bashrc $HOME
ln -Fisv $PWD/.gitignore_global $HOME
ln -Fisv $PWD/.mayu $HOME
ln -Fisv $PWD/.tmux.conf $HOME
ln -Fisv $PWD/.vimrc $HOME
ln -Fisv $PWD/.zshrc $HOME

# ln
# -d, -F, --directory    スーパーユーザがディレクトリに対するハードリンクを
#                        作成することを許可する。(備考: スーパーユーザであっても
#                        システムの制限で失敗することがある)
# -i, --interactive      対象を削除するかどうか確認メッセージを表示する
# -s, --symbolic         ハードリンクの代わりにシンボリックリンクを作成する
# -v, --verbose          print name of each linked file
