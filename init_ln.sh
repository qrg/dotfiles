#!/bin/sh
cd $(dirname $0)
for dotfile in .?*
do
    if [ $dotfile != '..' ] && [ $dotfile != '.git' ] && [ $dotfile != '.gitignore' ] && [ $dotfile != '.gitmodules' ] && [ $dotfile = '.gitignore_global' ]
# if の条件をもっと綺麗に書きたいけど書き方がわからない
    then
    	echo $dotfile
        ln -Fis "$PWD/$dotfile" $HOME

# -F, --directory   スーパーユーザがディレクトリに対するハードリンクを
#                    作成することを許可する。(備考: スーパーユーザであっても
#                    システムの制限で失敗することがある)
# -i, --interactive  対象を削除するかどうか確認メッセージを表示する
# -s, --symbolic     ハードリンクの代わりにシンボリックリンクを作成する

    fi
done

