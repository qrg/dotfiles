qrg dotfiles
================================================================================


Install
--------------------------------------------------------------------------------

``` bash
$ cd ~
$ git clone git@github.com:qrg/dotfiles.git
$ ~/dotfiles/init_ln.sh
$ git submodule init
$ git submodule update
```

* `init_ln.sh` `~/dotfiles` 内にあるファイルとディレクトリと同名のものを
  `~/` から探して、symlink を自動で設定する

* ↑そのようなことをするので、`init_ln.sh` 実行時に上書き確認が出る場合、
  それらのファイルとディレクトリのバックアップはとっといたほうがいいとおもう

